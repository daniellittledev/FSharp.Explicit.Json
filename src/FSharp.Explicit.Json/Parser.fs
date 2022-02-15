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

type JsonParserErrorReason<'t> =
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

type JsonParserError<'t> =
    {
        path: string list
        reason: JsonParserErrorReason<'t>
    }

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

type ParserContext(path: string list, element: JsonElement) =

    new(element: JsonElement) = ParserContext([], element)
    member _.Element = element
    member _.Path = path
    member _.NodeType = kindToType element.ValueKind

let error (context: ParserContext) (reason: JsonParserErrorReason<'t>) =
    [{ path = context.Path; reason = reason }] |> Error

let liftError (context: ParserContext) (reason: JsonParserErrorReason<'t>) =
    [{ path = context.Path; reason = reason }] |> Error

type Parser<'t, 'e> = ParserContext -> Validation<'t, JsonParserError<'e>>

let prop (propertyName: string) (context: ParserContext) : Validation<ParserContext, JsonParserError<'e>> = validation {
    let propExists, element = context.Element.TryGetProperty(propertyName)
    if propExists then
        return ParserContext (propertyName :: context.Path, element)
    else
        return! MissingProperty propertyName |> error context
}

let getValue (nodeType: NodeType) (valueGetter: JsonElement -> Validation<'t, JsonParserError<'e>>) (context: ParserContext) = validation {
    let element = context.Element
    let actualType = context.NodeType
    if actualType <> nodeType then
        return! (Expected nodeType, Actual actualType) |> UnexpectedType |> error context
    else
        return! valueGetter element
}

let validateTupleLength (expectedLength: int) (actualLength: int) (context: ParserContext) =
    if expectedLength = actualLength then
        Ok ()
    else
        InvalidTupleLength (Expected expectedLength, Actual actualLength) |> error context

let unit (context: ParserContext) =
    getValue Null (fun _ -> Ok ()) context

let bool (context: ParserContext) =
    getValue Bool (fun e -> e.GetBoolean() |> Ok) context

let byte (context: ParserContext) =
    getValue Number (fun e ->
        match e.TryGetByte() with
        | true, x -> Ok x
        | false, _ -> ValueOutOfRange (typeof<byte>, e.GetRawText()) |> error context
    ) context

let int16 (context: ParserContext) =
    getValue Number (fun e ->
        match e.TryGetInt16() with
        | true, x -> Ok x
        | false, _ -> ValueOutOfRange (typeof<int16>, e.GetRawText()) |> error context
    ) context

let int64 (context: ParserContext) =
    getValue Number (fun e ->
        match e.TryGetInt64() with
        | true, x -> Ok x
        | false, _ -> ValueOutOfRange (typeof<int64>, e.GetRawText()) |> error context
    ) context

let int (context: ParserContext) =
    getValue Number (fun e ->
        match e.TryGetInt32() with
        | true, x -> Ok x
        | false, _ -> ValueOutOfRange (typeof<int32>, e.GetRawText()) |> error context
    ) context

let single (context: ParserContext) =
    getValue Number (fun e ->
        match e.TryGetSingle() with
        | true, x -> Ok x
        | false, _ -> ValueOutOfRange (typeof<float32>, e.GetRawText()) |> error context
    ) context

let double (context: ParserContext) =
    getValue Number (fun e ->
        match e.TryGetDouble() with
        | true, x -> Ok x
        | false, _ -> ValueOutOfRange (typeof<double>, e.GetRawText()) |> error context
    ) context

let decimal (context: ParserContext) =
    getValue Number (fun e ->
        match e.TryGetDecimal() with
        | true, x -> Ok x
        | false, _ -> ValueOutOfRange (typeof<decimal>, e.GetRawText()) |> error context
    ) context

let string (context: ParserContext) =
    getValue String (fun e -> e.GetString() |> Ok) context

let tuple2 (parserA: Parser<'a, 'e>) (parserB: Parser<'b, 'e>) (context: ParserContext) : Validation<'a * 'b, JsonParserError<'e>> =
    getValue Array (fun e -> validation {
        do! validateTupleLength 2 (e.GetArrayLength()) context
        let! item0 = parserA (ParserContext (e.Item 0))
        and! item1 = parserB (ParserContext (e.Item 1))
        return (item0, item1)
    }) context

let tuple3 (parserA: Parser<'a, 'e>) (parserB: Parser<'b, 'e>) (parserC: Parser<'c, 'e>) (context: ParserContext) : Validation<'a * 'b * 'c, JsonParserError<'e>> =
    getValue Array (fun e -> validation {
        do! validateTupleLength 3 (e.GetArrayLength()) context
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
