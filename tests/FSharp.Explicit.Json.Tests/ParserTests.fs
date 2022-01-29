module FSharp.Explicit.Json.ParserTests

open Expecto
open Expecto.Flip.Expect
open System.Text.Json
open FsToolkit.ErrorHandling

[<AutoOpen>]
module TestTypes =
    type ObjectB = { propA: string }

    type ObjectA = { prop1: ObjectB }

    type UnionA =
        | Alpha of ObjectA
        | Beta

let parse (json: string) f =
    let doc = JsonDocument.Parse(json)
    Parse.document f doc

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
                |> Parse.document (fun node -> validation {

                    let! typeName = node.prop "type" Parse.string
                    and! prop1 =
                        let parseObj =
                            Parse.object <| fun node -> validation {
                                let! propA = node.prop "propA" Parse.string
                                return { propA = propA }
                            }
                        node.prop "prop1" parseObj

                    return!
                        match typeName with
                        | "Alpha" -> Alpha { prop1 = prop1 } |> Ok
                        | _ -> UserError "Expected known a DU name" |> Error
                })

            printfn "%A" result
            ()

        testList "Parse Primitives" [
        
            testCase "Parse unit" <| fun _ ->
                let expected = Ok ()
                let actual = parse "null" Parse.unit
                equal "null -> Ok ()" expected actual

            testCase "Parse int" <| fun _ ->
                let expected = Ok 12
                let actual = parse "12" Parse.int
                equal "null -> Ok ()" expected actual

            testCase "Parse decimal" <| fun _ ->
                let expected = Ok 0.1111111111111111111111111111m
                let actual = parse "0.1111111111111111111111111111" Parse.decimal
                equal "null -> Ok ()" expected actual
        ]
    ]
