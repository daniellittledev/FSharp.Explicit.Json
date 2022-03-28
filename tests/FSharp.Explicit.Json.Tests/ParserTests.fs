module FSharp.Explicit.Json.ParserTests

open Expecto
open Expecto.Flip.Expect
open System
open System.Text.Json
open FsToolkit.ErrorHandling
open FSharp.Explicit.Json.Parse

[<AutoOpen>]
module rec TestTypes =
    type Name = string
    type Scalar = { name: Name; value: decimal }
    type Vector = { name: Name; value: decimal list }
    type Operation = { name: string; valueA: Value; valueB: Value }

    type Formula =
    | Add of Operation
    | Subtract of Operation

    type Value =
    | NoValue
    | Reference of Name
    | Scalar of Scalar
    | Vector of Vector
    | Formula of Formula

let leftError (path: string list) (reason: JsonParserErrorReason<'t>) =
    [{ path = path; reason = reason }] |> Error

let error (path: string list) (reason: JsonParserErrorReason<'t>) =
    { path = path; reason = reason }

let jsonString (value: string) =
    $"\"{value}\""

let parse (json: string) f =
    let doc = JsonDocument.Parse(json)
    Parse.document f doc

let tests =
    testList "Parser Tests" [
        
        testCase "Sample" <| fun _ ->

            let json =
                """{
                    "type": "Formula",
                    "name": "Test",
                    "valueA": "Value1",
                    "valueB": {
                        "type": "Vector",
                        "value": [1, 2, 3]
                    }
                }"""

            let doc = JsonDocument.Parse(json)

            let rec parseValue (node: ParserContext) = validation {
                match node.NodeType with
                | Null ->
                    return NoValue
                | String ->
                    let! name = Parse.string node
                    return Reference name
                | Object ->
                    let! typeName = node.prop "type" Parse.string

                    return! validation {
                        let! name = node.prop "name" Parse.string
                        match typeName with
                        | "Scalar" ->
                            let! scalar = Parse.decimal node
                            return Scalar { name = name; value = scalar}
                        | "Vector" ->
                            let! vector = Parse.list Parse.decimal node
                            return Vector { name = name; value = vector}
                        | "Add"
                        | "Subtract" ->
                            let! typeCreator =
                                match typeName with
                                | "Add" -> Ok Add
                                | "Subtract" -> Ok Subtract
                                | _ -> UserError "Unexpected formula type" |> Parse.liftError node
                            let! valueA = node.prop "valueA" parseValue
                            and! valueB = node.prop "valueB" parseValue
                            return Formula (typeCreator {
                                name = name
                                valueA = valueA
                                valueB = valueB
                            })
                        | _ ->
                            return! UserError "Unexpected type when matching on value"  |> Parse.liftError node
                    }
                | x ->
                    return! UnexpectedType (Expected Object, Actual x) |> Parse.liftError node
            }

            let result = doc |> Parse.document parseValue

            printfn "%A" result
            ()

        testList "Parse Primitives" [
        
            testCase "Parse unit" <| fun _ ->
                let expected = Ok ()
                let actual = parse "null" Parse.unit
                equal "null -> Ok ()" expected actual

            testCase "Parse bool" <| fun _ ->
                let expected = Ok true
                let actual = parse "true" Parse.bool
                equal "true -> Ok true" expected actual

            testCase "Parse byte" <| fun _ ->
                let expected = Ok 256
                let actual = parse "256" Parse.int
                equal "256 -> Ok 256" expected actual

            testCase "Parse out of range byte" <| fun _ ->
                let json = $"{Byte.MaxValue.ToString()}0"
                let expected = ValueOutOfRange (typeof<byte>, json) |> leftError []
                let actual = parse json Parse.byte
                equal (sprintf "%A -> %A" json expected) expected actual

            testCase "Parse int16" <| fun _ ->
                let number = Int16.MaxValue
                let expected = Ok number
                let json = number.ToString()
                let actual = parse json Parse.int16
                equal $"{json} -> Ok {json}" expected actual

            testCase "Parse out of range int16" <| fun _ ->
                let json = $"{Int16.MaxValue.ToString()}0"
                let expected = ValueOutOfRange (typeof<int16>, json) |> leftError []
                let actual = parse json Parse.int16
                equal (sprintf "%A -> %A" json expected) expected actual

            testCase "Parse int" <| fun _ ->
                let number = Int32.MaxValue
                let expected = Ok number
                let json = number.ToString()
                let actual = parse json Parse.int
                equal $"{json} -> Ok {json}" expected actual

            testCase "Parse out of range int" <| fun _ ->
                let json = $"{Int32.MaxValue.ToString()}0"
                let expected = ValueOutOfRange (typeof<int32>, json) |> leftError []
                let actual = parse json Parse.int
                equal (sprintf "%A -> %A" json expected) expected actual

            testCase "Parse long" <| fun _ ->
                let number = Int64.MaxValue
                let expected = Ok number
                let json = number.ToString()
                let actual = parse json Parse.long
                equal $"{json} -> Ok {json}" expected actual

            testCase "Parse out of range long" <| fun _ ->
                let json = $"{Int64.MaxValue.ToString()}0"
                let expected = ValueOutOfRange (typeof<int64>, json) |> leftError []
                let actual = parse json Parse.long
                equal (sprintf "%A -> %A" json expected) expected actual

            testCase "Parse single" <| fun _ ->
                let number = Single.MaxValue
                let expected = Ok number
                let json = number.ToString()
                let actual = parse json Parse.single
                equal $"{json} -> Ok {json}" expected actual

            testCase "Parse out of range single" <| fun _ ->
                let json = $"{Single.MaxValue.ToString()}0"
                let expected = Ok Single.PositiveInfinity
                let actual = parse json Parse.single
                equal (sprintf "%A -> %A" json expected) expected actual

            testCase "Parse double" <| fun _ ->
                let number = Double.MaxValue
                let expected = Ok number
                let json = number.ToString()
                let actual = parse json Parse.double
                equal $"{json} -> Ok {json}" expected actual

            testCase "Parse out of range double" <| fun _ ->
                let json = $"{Double.MaxValue.ToString()}0"
                let expected = Ok Double.PositiveInfinity
                let actual = parse json Parse.double
                equal (sprintf "%A -> %A" json expected) expected actual

            testCase "Parse deciaml" <| fun _ ->
                let number = System.Decimal.MaxValue
                let expected = Ok number
                let json = number.ToString()
                let actual = parse json Parse.decimal
                equal $"{json} -> Ok {json}" expected actual

            testCase "Parse out of range decimal" <| fun _ ->
                let json = $"{Decimal.MaxValue.ToString()}0"
                let expected = ValueOutOfRange (typeof<decimal>, json) |> leftError []
                let actual = parse json Parse.decimal
                equal (sprintf "%A -> %A" json expected) expected actual

            testCase "Parse DateTime" <| fun _ ->
                let date = DateTime.MaxValue
                let expected = Ok date
                let json = date.ToString("o") |> jsonString
                let actual = parse json Parse.dateTime
                equal $"{json} -> Ok {json}" expected actual

            testCase "Parse DateTime Offset" <| fun _ ->
                let date = new DateTimeOffset(DateTime.MaxValue, TimeSpan.FromHours(14))
                let expected = Ok date
                let json = date.ToString("o") |> jsonString
                let actual = parse json Parse.dateTimeOffset
                equal $"{json} -> Ok {json}" expected actual

            testCase "Parse TimeSpan" <| fun _ ->
                let timespan = TimeSpan.MaxValue
                let expected = Ok timespan
                let json = timespan.ToString() |> jsonString
                let actual = parse json Parse.timeSpan
                equal $"{json} -> Ok {json}" expected actual

            testCase "Parse Enum Name" <| fun _ ->
                let value = DateTimeKind.Utc
                let expected = Ok DateTimeKind.Utc
                let json = value.ToString() |> jsonString
                let actual = parse json Parse.enumNameOf<DateTimeKind>
                equal $"{json} -> Ok {json}" expected actual

            testCase "Parse list" <| fun _ ->
                let expected = Ok [ 1; 2; 3 ]
                let actual = parse "[1, 2, 3]" (Parse.list Parse.int)
                equal "[1, 2, 3] -> Ok [1; 2; 3]" expected actual
        ]

        testList "Parse Option" [
            testCase "Parse Some x" <| fun _ ->
                let expected = Ok (Some 1)

                let json = "1"
                let actual = parse json (Parse.option Parse.int)

                equal $"{json} -> Some 1" expected actual

            testCase "Parse None" <| fun _ ->
                let expected: Result<int option, _> = Ok None

                let json = "null"
                let actual = parse json (Parse.option Parse.int)

                equal $"{json} -> None" expected actual

            testCase "Parse Some Wrong Type" <| fun _ ->
                let expected = UnexpectedType (Expected NodeType.Number, Actual NodeType.Bool) |> leftError []

                let json = "false"
                let actual = parse json (Parse.option Parse.int)

                equal $"{json} -> Type Error for int" expected actual
        ]

        testList "Parse Tuples" [
            testCase "Parse Tuple2" <| fun _ ->
                let expected = Ok(1, "a")

                let json = """[1, "a"]"""
                let actual = parse json (Parse.tuple2 Parse.int Parse.string)

                equal $"""{json} -> (1, "a")""" expected actual

            testCase "Parse Tuple2 types mismatch" <| fun _ ->
                let expected =
                    Error [
                        UnexpectedType (Expected Number, Actual Bool) |> error []
                        UnexpectedType (Expected String, Actual Number) |> error []
                    ]

                let json = """[false, 1]"""
                let actual = parse json (Parse.tuple2 Parse.int Parse.string)

                equal $"""{json} -> Type Error for (int * string)""" expected actual

            testCase "Parse Tuple3" <| fun _ ->
                let expected = Ok(1, "a", false)

                let json = """[1, "a", false]"""
                let actual = parse json (Parse.tuple3 Parse.int Parse.string Parse.bool)

                equal $"""{json} -> (1, "a", false)""" expected actual
        ]

        testList "Parse Choice" [

            testCase "Parse Choice1Of2" <| fun _ ->
                let expected = Ok <| Choice1Of2 1
                
                let json = "1"
                let actual = parse json (Parse.choiceOf2 Parse.int Parse.bool)
                
                equal $"{json} -> 1" expected actual

            testCase "Parse Choice2Of2" <| fun _ ->
                let expected = Ok <| Choice2Of2 true

                let json = "true"
                let actual = parse json (Parse.choiceOf2 Parse.int Parse.bool)

                equal $"{json} -> true" expected actual


            testCase "Parse Choice3Of3" <| fun _ ->
                let expected = Ok <| Choice3Of3 "y"

                let json = "\"y\""
                let actual = parse json (Parse.choiceOf3 Parse.int Parse.bool Parse.string)

                equal $"{json} -> \"y\"" expected actual

            // Up to 7of7

            // Error handling
            // Ambiguous types
            // Not matching types

        ]

        testList "Parse Error Handling" [
            testCase "Missing Properties" <| fun _ ->
        
                let json = "{}"
        
                let doc = JsonDocument.Parse(json)
                let actual = doc |> Parse.document (fun node -> validation {
                    let! typeName = node.prop "type" Parse.string
                    and! prop1 = node.prop "prop1" Parse.string
                    and! prop2 = node.prop "prop2" Parse.string
                    return (typeName, prop1, prop2)
                })
        
                let expected =
                    Error [
                        { path = []; reason = MissingProperty "type"}
                        { path = []; reason = MissingProperty "prop1"}
                        { path = []; reason = MissingProperty "prop2"}
                    ]

                equal $"""All three properties are missing""" expected actual
        ]

        // Parse Discriminated Union

        // Parse Choice/Union/OneOf

        // Parse Auto Object
    ]
