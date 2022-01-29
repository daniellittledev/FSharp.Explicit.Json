namespace FSharp.Explicit.Json

open System.Text.Json
open FsToolkit.ErrorHandling

type ExpectedType = ExpectedType of JsonValueKind
type ActualType = ActualType of JsonValueKind

type JsonParserError<'t> =
    | SyntaxError //List of errors: https://developer.mozilla.org/en-US/docs/Web/JavaScript/Reference/Errors/JSON_bad_parse
    | MissingProperty of string
    | UnexpectedType of ExpectedType * ActualType
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

    let getValue (jsonValueKind: JsonValueKind) (valueGetter: JsonElement -> 't) (context: ParserContext) = validation {
        let element = context.Element
        if element.ValueKind <> jsonValueKind then
            return! (ExpectedType jsonValueKind, ActualType element.ValueKind) |> UnexpectedType |> Error
        else
            return valueGetter element
    }

    let unit (context: ParserContext) : Validation<unit, JsonParserError<'e>> =
        getValue JsonValueKind.Null (fun _ -> ()) context

    let int (context: ParserContext) : Validation<int, JsonParserError<'e>> =
        getValue JsonValueKind.Number (fun e -> e.GetInt32()) context

    let decimal (context: ParserContext) : Validation<decimal, JsonParserError<'e>> =
        getValue JsonValueKind.Number (fun e -> e.GetDecimal()) context

    let string (context: ParserContext) : Validation<string, JsonParserError<'e>> =
        getValue JsonValueKind.String (fun e -> e.GetString()) context

    let object (parse: ParserContext -> Validation<'t, JsonParserError<'e>>) (context: ParserContext) : Validation<'t, JsonParserError<'e>> = validation {
        let! value = getValue JsonValueKind.Object (fun _ -> ()) context
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

