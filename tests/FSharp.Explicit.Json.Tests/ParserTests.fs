module FSharp.Explicit.Json.ParserTests

open Expecto
open System.Text.Json
open FsToolkit.ErrorHandling

[<AutoOpen>]
module TestTypes =
    type ObjectB = { propA: string }

    type ObjectA = { prop1: ObjectB }

    type UnionA =
        | Alpha of ObjectA
        | Beta

let tests =
    testList "Parser Tests" [
        
        testCase "Sample" <| fun _ ->

            let json =
                """{
                    "type": "Alpha",
                    "prop1": {
                        "propA": "Test"
                    }
                }"""

            let doc = JsonDocument.Parse(json)

            let result =
                doc
                |> Parse.document (fun parse -> validation {

                    let! typeName = parse.string "type"
                    and! prop1 =
                        parse.object "prop1" (fun parse ->
                            validation {
                                let! propA = parse.string "propA"
                                return { propA = propA }
                            })

                    return!
                        match typeName with
                        | "Alpha" -> Alpha { prop1 = prop1 } |> Ok
                        | _ -> UserError "Expected known a DU name" |> Error
                })

            printfn "%A" result
            ()

    ]