namespace FSharp.Explicit.Json

open System.Text.Json
open FsToolkit.ErrorHandling

type Expected<'t> = Expected of 't

type Actual<'t> = Actual of 't

type JsonParserError<'t> =
    | SyntaxError //List of errors: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Errors/JSON_bad_parse
    | MissingProperty of string
    | UnexpectedType of Expected<JsonValueKind> * Actual<JsonValueKind>
    | InvalidTupleLength of Expected<int> * Actual<int>
    | UserError of 't

module Parse =

    type ParserContext(element: JsonElement) =
        member _.Element = element

    type Parser<'t, 'e> = ParserContext -> Validation<'t, JsonParserError<'e>>

    let prop (propertyName: string) (context: ParserContext) : Validation<ParserContext, JsonParserError<'e>> = validation {
        let propExists, element = context.Element.TryGetProperty(propertyName)
        if propExists then
            return ParserContext element
        else
            return! MissingProperty propertyName |> Error
    }

    let getValue (jsonValueKind: JsonValueKind) (valueGetter: JsonElement -> Validation<'t, JsonParserError<'e>>) (context: ParserContext) = validation {
        let element = context.Element
        if element.ValueKind <> jsonValueKind then
            return! (Expected jsonValueKind, Actual element.ValueKind) |> UnexpectedType |> Error
        else
            return! valueGetter element
    }

    let validateTupleLength (expectedLength: int) (actualLength: int)  =
        if expectedLength = actualLength then
            Ok ()
        else
            Error (InvalidTupleLength (Expected expectedLength, Actual actualLength))

    let unit (context: ParserContext) : Validation<unit, JsonParserError<'e>> =
        getValue JsonValueKind.Null (fun _ -> Ok ()) context

    let int (context: ParserContext) : Validation<int, JsonParserError<'e>> =
        getValue JsonValueKind.Number (fun e -> e.GetInt32() |> Ok) context

    let tuple2 (parserA: Parser<'a, 'e>) (parserB: Parser<'b, 'e>) (context: ParserContext) : Validation<'a * 'b, JsonParserError<'e>> =
        getValue JsonValueKind.Array (fun e -> validation {
            do! validateTupleLength 2 (e.GetArrayLength())
            let! item0 = parserA (ParserContext (e.Item 0))
            and! item1 = parserB (ParserContext (e.Item 1))
            return (item0, item1)
        }) context

    let list (itemParser: Parser<'a, 'e>) (context: ParserContext) : Validation<'a list, JsonParserError<'e>> =
        getValue JsonValueKind.Array (fun e -> 
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

    let decimal (context: ParserContext) : Validation<decimal, JsonParserError<'e>> =
        getValue JsonValueKind.Number (fun e -> e.GetDecimal() |> Ok) context

    let string (context: ParserContext) : Validation<string, JsonParserError<'e>> =
        getValue JsonValueKind.String (fun e -> e.GetString() |> Ok) context

    let object (parse: ParserContext -> Validation<'t, JsonParserError<'e>>) (context: ParserContext) : Validation<'t, JsonParserError<'e>> = validation {
        let! value = getValue JsonValueKind.Object (fun _ -> () |> Ok) context
        return! parse context
    }

    type ParserContext with
        member this.prop (propertyName: string) (parser: Parser<'t, 'e>) = validation {
            let! value = prop propertyName this
            return! parser value
        }

    let document (parserResult: ParserContext -> Validation<'t, JsonParserError<'e>>) (document: JsonDocument) =
        ParserContext document.RootElement |> parserResult

module Render =
    let doit = ()

