module FSharp.Explicit.Json.Parse

open System
open System.Text.Json
open FsToolkit.ErrorHandling

[<Struct>]
type NodeType =
    | Undefined
    | Object
    | Array
    | String
    | Number
    | Bool
    | Null

type JsonParserError<'t> =
    | MissingProperty of string
    | UnexpectedType of Expected<NodeType> * Actual<NodeType>
    | InvalidTupleLength of Expected<int> * Actual<int>
    | ValueOutOfRange of Type * string
    | UserError of 't
    member this.ToMessage(toMessage: 't -> string) =
        match this with
        | MissingProperty name -> $"Object has missing property named {name}"
        | UnexpectedType (expected, actual) -> $"Parser was expecting type {Expected expected} but was {Actual actual}"
        | InvalidTupleLength (expected, actual) -> $"Parser was a tuple with length {Expected expected} but was {Actual actual}"
        | ValueOutOfRange (targetType, rawValue) -> $"The value {rawValue} is not a valid {targetType} "
        | UserError x -> toMessage x

let kindToType (jsonValueKind: JsonValueKind) =
    match jsonValueKind with
    | JsonValueKind.Undefined -> Undefined
    | JsonValueKind.Object -> Object
    | JsonValueKind.Array -> Array
    | JsonValueKind.String -> String
    | JsonValueKind.Number -> Number
    | JsonValueKind.True -> Bool
    | JsonValueKind.False -> Bool
    | JsonValueKind.Null -> Null
    | _ -> failwithf "Panic: Missing case for JsonValueKind when convertint to NodeType"

type ParserContext(element: JsonElement) =
    member _.Element = element
    member _.NodeType = kindToType element.ValueKind

type Parser<'t, 'e> = ParserContext -> Validation<'t, JsonParserError<'e>>

let prop (propertyName: string) (context: ParserContext) : Validation<ParserContext, JsonParserError<'e>> = validation {
    let propExists, element = context.Element.TryGetProperty(propertyName)
    if propExists then
        return ParserContext element
    else
        return! MissingProperty propertyName |> Error
}

let getValue (nodeType: NodeType) (valueGetter: JsonElement -> Validation<'t, JsonParserError<'e>>) (context: ParserContext) = validation {
    let element = context.Element
    let actualType = context.NodeType
    if actualType <> nodeType then
        return! (Expected nodeType, Actual actualType) |> UnexpectedType |> Error
    else
        return! valueGetter element
}

let validateTupleLength (expectedLength: int) (actualLength: int)  =
    if expectedLength = actualLength then
        Ok ()
    else
        Error (InvalidTupleLength (Expected expectedLength, Actual actualLength))

let unit (context: ParserContext) : Validation<unit, JsonParserError<'e>> =
    getValue Null (fun _ -> Ok ()) context

let bool (context: ParserContext) : Validation<bool, JsonParserError<'e>> =
    getValue Bool (fun e -> e.GetBoolean() |> Ok) context

let byte (context: ParserContext) : Validation<byte, JsonParserError<'e>> =
    getValue Number (fun e -> e.GetByte() |> Ok) context

let int16 (context: ParserContext) : Validation<int16, JsonParserError<'e>> =
    getValue Number (fun e -> e.GetInt16() |> Ok) context

let int64 (context: ParserContext) : Validation<int64, JsonParserError<'e>> =
    getValue Number (fun e -> e.GetInt64() |> Ok) context

let int (context: ParserContext) : Validation<int, JsonParserError<'e>> =
    getValue Number (fun e ->
        match e.TryGetInt32() with
        | true, x -> Ok x
        | false, _ -> Error [ValueOutOfRange (typeof<int32>, e.GetRawText())]
    ) context

let single (context: ParserContext) : Validation<float32, JsonParserError<'e>> =
    getValue Number (fun e -> e.GetSingle() |> Ok) context

let double (context: ParserContext) : Validation<double, JsonParserError<'e>> =
    getValue Number (fun e -> e.GetDouble() |> Ok) context

let decimal (context: ParserContext) : Validation<decimal, JsonParserError<'e>> =
    getValue Number (fun e -> e.GetDecimal() |> Ok) context

let string (context: ParserContext) : Validation<string, JsonParserError<'e>> =
    getValue String (fun e -> e.GetString() |> Ok) context

let tuple2 (parserA: Parser<'a, 'e>) (parserB: Parser<'b, 'e>) (context: ParserContext) : Validation<'a * 'b, JsonParserError<'e>> =
    getValue Array (fun e -> validation {
        do! validateTupleLength 2 (e.GetArrayLength())
        let! item0 = parserA (ParserContext (e.Item 0))
        and! item1 = parserB (ParserContext (e.Item 1))
        return (item0, item1)
    }) context

let tuple3 (parserA: Parser<'a, 'e>) (parserB: Parser<'b, 'e>) (parserC: Parser<'c, 'e>) (context: ParserContext) : Validation<'a * 'b * 'c, JsonParserError<'e>> =
    getValue Array (fun e -> validation {
        do! validateTupleLength 3 (e.GetArrayLength())
        let! item0 = parserA (ParserContext (e.Item 0))
        and! item1 = parserB (ParserContext (e.Item 1))
        and! item2 = parserC (ParserContext (e.Item 2))
        return (item0, item1, item2)
    }) context

let list (itemParser: Parser<'a, 'e>) (context: ParserContext) : Validation<'a list, JsonParserError<'e>> =
    getValue Array (fun e -> 
        e.EnumerateArray()
        |> Seq.map (fun e -> 
            itemParser (ParserContext e)
        )
        |> Seq.fold (fun validation item ->
            Validation.map2 (fun (x: 'a) (s: 'a list) -> x::s) item validation
        ) (Ok [])
        |> Result.map List.rev
    ) context

let array (itemParser: Parser<'a, 'e>) (context: ParserContext) : Validation<'a array, JsonParserError<'e>> =
    list itemParser context
    |> Result.map Seq.toArray

let object (parse: ParserContext -> Validation<'t, JsonParserError<'e>>) (context: ParserContext) : Validation<'t, JsonParserError<'e>> = validation {
    let! value = getValue Object (fun _ -> () |> Ok) context
    return! parse context
}

type ParserContext with
    member this.prop (propertyName: string) (parser: Parser<'t, 'e>) = validation {
        let! value = prop propertyName this
        return! parser value
    }

let document (parserResult: ParserContext -> Validation<'t, JsonParserError<'e>>) (document: JsonDocument) =
    ParserContext document.RootElement |> parserResult
