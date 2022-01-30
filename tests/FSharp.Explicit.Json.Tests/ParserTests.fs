module FSharp.Explicit.Json.ParserTests

open Expecto
open Expecto.Flip.Expect
open System.Text.Json
open FsToolkit.ErrorHandling
open FSharp.Explicit.Json.Parse

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

            // Parse Numeric

            // Parse DateTimeOffset/DateTime/Date

            // Parse TimeSpan

            // Parse Enum

            testCase "Parse list" <| fun _ ->
                let expected = Ok [ 1; 2; 3 ]
                let actual = parse "[1, 2, 3]" (Parse.list Parse.int)
                equal "[1, 2, 3] -> Ok [1; 2; 3]" expected actual
        ]

        // Parse Option

        testList "Parse Tuples" [
            testCase "Parse Tuple2" <| fun _ ->
                let expected = Ok(1, "string")

                let json = """[1, "string"]"""
                let actual = parse json (Parse.tuple2 Parse.int Parse.string)

                
                equal $"""{json} -> (1, "string")""" expected actual

            testCase "Parse Tuple2 types mismatch" <| fun _ ->
                ()
        ]

        // Parse Union

        // Parse OneOf

        // Parse Auto Object
    ]
