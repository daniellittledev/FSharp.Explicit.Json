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

    let prop (propertyName: string) (element: JsonElement) : Result<JsonElement, JsonParserError<'e>> = result {
        let propExists, element = element.TryGetProperty(propertyName)
        if propExists then
            return element
        else
            return! MissingProperty propertyName |> Error
    }

    let getValue (jsonValueKind: JsonValueKind) (valueGetter: JsonElement -> 't) (element: JsonElement) = result {
        if element.ValueKind <> jsonValueKind then
            return! (ExpectedType jsonValueKind, ActualType element.ValueKind) |> UnexpectedType |> Error
        else
            return valueGetter element
    }

    let string (propertyName: string) (element: JsonElement) : Result<string, JsonParserError<'e>> = result {
        let! property = prop propertyName element
        let! value = getValue JsonValueKind.String (fun e -> e.GetString()) property
        return value
    }

    let int (propertyName: string) (element: JsonElement) : Result<int, JsonParserError<'e>> = result {
        let! property = prop propertyName element
        let! value = getValue JsonValueKind.Number (fun e -> e.GetInt32()) property
        return value
    }

    let object
        (propertyName: string)
        (mapper: JsonElement -> Result<'t, JsonParserError<'e> list>)
        (element: JsonElement)
        : Result<'t, JsonParserError<'e> list> = validation {
        let! value = prop propertyName element
        return! value |> mapper
    }

    type ParserContext with
        member x.object (propertyName: string) (parser: ParserContext -> Result<'t, JsonParserError<'e> list>) =
            x.Element |> object propertyName (fun e -> parser (ParserContext e))

    type ParserContext with
        member x.string(propertyName: string) = x.Element |> string propertyName

    let document (parserResult: ParserContext -> Result<'t, JsonParserError<'e> list>) (document: JsonDocument) =
        ParserContext document.RootElement |> parserResult

module Render =
    let doit = ()

