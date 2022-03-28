module FSharp.Explicit.Json.RenderTests

open System
open Expecto
open Expecto.Flip.Expect
open System.Text.Json
open System.IO
open FSharp.Explicit.Json.Render

type IndentStyle =
    | Inline
    | Indented

type JsonWriter(style: IndentStyle) =

    let options = JsonWriterOptions(Indented = match style with Inline -> false | Indented -> true)
    let stream = new MemoryStream()
    let writer = new Utf8JsonWriter(stream, options)

    new() = new JsonWriter(IndentStyle.Inline)

    member _.GetJsonString(render: Render) =
        render.Invoke(writer)

        writer.Flush()
        stream.Seek(0L, SeekOrigin.Begin) |> ignore
        let reader = new StreamReader(stream)
        reader.ReadToEnd()

    interface IDisposable with
        member _.Dispose() =
            stream.Dispose()

let tests =
    testList "Render Tests" [

        testCase "Sample" <| fun _ ->
            use jsonWriter = new JsonWriter(IndentStyle.Indented)
            let jsonRender =
                object {
                    prop "alpha" (object {
                        prop "beta" (Render.string "1")
                    })
                    prop "gamma" (Render.string "2")
                }
            let jsonText = jsonWriter.GetJsonString(jsonRender)

            let expected = """{
  "alpha": {
    "beta": "1"
  },
  "gamma": "2"
}"""

            equal "JSON is equal" expected jsonText

        testList "Render Primitives" [
        
            testCase "Render Unit" <| fun _ ->
                use jsonWriter = new JsonWriter()
                let jsonRender = Render.unit ()
                let jsonText = jsonWriter.GetJsonString(jsonRender)
                let expected = "null"
                equal "JSON is equal" expected jsonText

            testCase "Render Byte" <| fun _ ->
                use jsonWriter = new JsonWriter()
                let jsonRender = Render.unit ()
                let jsonText = jsonWriter.GetJsonString(jsonRender)
                let expected = "null"
                equal "JSON is equal" expected jsonText

            testCase "Render Int" <| fun _ ->
                use jsonWriter = new JsonWriter()
                let jsonRender = Render.int 1
                let jsonText = jsonWriter.GetJsonString(jsonRender)
                let expected = "1"
                equal "JSON is equal" expected jsonText
            
            testCase "Render Decimal" <| fun _ ->
                use jsonWriter = new JsonWriter()
                let jsonRender = Render.decimal 2m
                let jsonText = jsonWriter.GetJsonString(jsonRender)
                let expected = "2"
                equal "JSON is equal" expected jsonText

            testCase "Render DateTime" <| fun _ ->
                use jsonWriter = new JsonWriter()
                let jsonRender = Render.datetime (DateTime(2000, 01, 01, 1, 1, 1, DateTimeKind.Utc))
                let jsonText = jsonWriter.GetJsonString(jsonRender)
                let expected = "\"2000-01-01T01:01:01Z\""
                equal "JSON is equal" expected jsonText

            testCase "Render DateTimeOffset" <| fun _ ->
                use jsonWriter = new JsonWriter()
                let jsonRender = Render.datetimeoffset (DateTimeOffset(2000, 01, 01, 1, 1, 1, TimeSpan.FromHours 10))
                let jsonText = jsonWriter.GetJsonString(jsonRender)
                let expected = "\"2000-01-01T01:01:01+10:00\""
                equal "JSON is equal" expected jsonText

            testCase "Render TimeSpan" <| fun _ ->
                use jsonWriter = new JsonWriter()
                let jsonRender = Render.timespan (TimeSpan.FromMinutes 90)
                let jsonText = jsonWriter.GetJsonString(jsonRender)
                let expected = "\"01:30:00\""
                equal "JSON is equal" expected jsonText

            testCase "Render Enum" <| fun _ ->
                use jsonWriter = new JsonWriter()
                let jsonRender = Render.enum (System.DayOfWeek.Monday)
                let jsonText = jsonWriter.GetJsonString(jsonRender)
                let expected = "1"
                equal "JSON is equal" expected jsonText

            testCase "Render Enum Name" <| fun _ ->
                use jsonWriter = new JsonWriter()
                let jsonRender = Render.enumName System.DayOfWeek.Monday
                let jsonText = jsonWriter.GetJsonString(jsonRender)
                let expected = "\"Monday\""
                equal "JSON is equal" expected jsonText

            testCase "Render Array" <| fun _ ->
                use jsonWriter = new JsonWriter()
                let jsonRender = array {
                    Render.enumName System.DayOfWeek.Monday
                    Render.enumName System.DayOfWeek.Tuesday
                    Render.enumName System.DayOfWeek.Wednesday
                }
                let jsonText = jsonWriter.GetJsonString(jsonRender)
                let expected = "[\"Monday\",\"Tuesday\",\"Wednesday\"]"
                equal "JSON is equal" expected jsonText
        ]

        // Render Option
        testList "Render Option" [
            
            testCase "Render Some" <| fun _ ->
                use jsonWriter = new JsonWriter()
                let jsonRender = (Some "Thing") |> Render.option Render.string
                let jsonText = jsonWriter.GetJsonString(jsonRender)
                let expected = "\"Thing\""
                equal "Some renders value" expected jsonText

            testCase "Render None" <| fun _ ->
                use jsonWriter = new JsonWriter()
                let jsonRender = None |> Render.option Render.string
                let jsonText = jsonWriter.GetJsonString(jsonRender)
                let expected = "null"
                equal "None renders null" expected jsonText
        ]

        // Render Tuples
        testList "Render Tuples" [
            
            testCase "Render Tuple 2" <| fun _ ->
                use jsonWriter = new JsonWriter()
                let jsonRender = (1, 2) |> Render.tuple2 Render.int Render.int
                let jsonText = jsonWriter.GetJsonString(jsonRender)
                let expected = "[1,2]"
                equal "Renders Tuple 2" expected jsonText

            testCase "Render Tuple 3" <| fun _ ->
                use jsonWriter = new JsonWriter()
                let jsonRender = (1, 2, 3) |> Render.tuple3 Render.int Render.int Render.int
                let jsonText = jsonWriter.GetJsonString(jsonRender)
                let expected = "[1,2,3]"
                equal "Renders Tuple 3" expected jsonText

            testCase "Render Tuple 4" <| fun _ ->
                use jsonWriter = new JsonWriter()
                let jsonRender = (1, 2, 3, 4) |> Render.tuple4 Render.int Render.int Render.int Render.int
                let jsonText = jsonWriter.GetJsonString(jsonRender)
                let expected = "[1,2,3,4]"
                equal "Renders Tuple 4" expected jsonText

            testCase "Render Tuple 5" <| fun _ ->
                use jsonWriter = new JsonWriter()
                let jsonRender = (1, 2, 3, 4, 5) |> Render.tuple5 Render.int Render.int Render.int Render.int Render.int
                let jsonText = jsonWriter.GetJsonString(jsonRender)
                let expected = "[1,2,3,4,5]"
                equal "Renders Tuple 5" expected jsonText

            testCase "Render Tuple 6" <| fun _ ->
                use jsonWriter = new JsonWriter()
                let jsonRender = (1, 2, 3, 4, 5, 6) |> Render.tuple6 Render.int Render.int Render.int Render.int Render.int Render.int
                let jsonText = jsonWriter.GetJsonString(jsonRender)
                let expected = "[1,2,3,4,5,6]"
                equal "Renders Tuple 6" expected jsonText

            testCase "Render Tuple 7" <| fun _ ->
                use jsonWriter = new JsonWriter()
                let jsonRender = (1, 2, 3, 4, 5, 6, 7) |> Render.tuple7 Render.int Render.int Render.int Render.int Render.int Render.int Render.int
                let jsonText = jsonWriter.GetJsonString(jsonRender)
                let expected = "[1,2,3,4,5,6,7]"
                equal "Renders Tuple 7" expected jsonText
        ]

        // Render Discriminated Union
        testList "Render Discriminated Union" [
            
            testCase "Render Union Case" <| fun _ ->
                use jsonWriter = new JsonWriter()
                // A union has an indeterminate structure so rendering is manual
                let jsonRender = object {
                    prop "type" <| Render.string "CaseA"
                    prop "value" <| Render.int 1
                }
                let jsonText = jsonWriter.GetJsonString(jsonRender)
                let expected = """{"type":"CaseA","value":1}"""
                equal "Renders Union Case" expected jsonText
        ]

        // Render Choice/Union/OneOf
        testList "Render Choice" [
            
            testCase "Render Choice1Of2" <| fun _ ->
                use jsonWriter = new JsonWriter()
                // A union has an indeterminate structure so rendering is manual
                let jsonRender = Choice1Of2 "1Of2" |> Render.choiceOf2 Render.string Render.string
                let jsonText = jsonWriter.GetJsonString(jsonRender)
                let expected = "\"1Of2\""
                equal "Renders Choice Value" expected jsonText

            testCase "Render Choice2Of2" <| fun _ ->
                use jsonWriter = new JsonWriter()
                // A union has an indeterminate structure so rendering is manual
                let jsonRender = Choice2Of2 "2Of2" |> Render.choiceOf2 Render.string Render.string
                let jsonText = jsonWriter.GetJsonString(jsonRender)
                let expected = "\"2Of2\""
                equal "Renders Choice Value" expected jsonText

            testCase "Render Choice3Of3" <| fun _ ->
                use jsonWriter = new JsonWriter()
                // A union has an indeterminate structure so rendering is manual
                let jsonRender = Choice3Of3 "3Of3" |> Render.choiceOf3 Render.string Render.string Render.string
                let jsonText = jsonWriter.GetJsonString(jsonRender)
                let expected = "\"3Of3\""
                equal "Renders Choice Value" expected jsonText

            testCase "Render Choice4Of4" <| fun _ ->
                use jsonWriter = new JsonWriter()
                // A union has an indeterminate structure so rendering is manual
                let jsonRender = Choice4Of4 "4Of4" |> Render.choiceOf4 Render.string Render.string Render.string Render.string
                let jsonText = jsonWriter.GetJsonString(jsonRender)
                let expected = "\"4Of4\""
                equal "Renders Choice Value" expected jsonText

            testCase "Render Choice5Of5" <| fun _ ->
                use jsonWriter = new JsonWriter()
                // A union has an indeterminate structure so rendering is manual
                let jsonRender = Choice5Of5 "5Of5" |> Render.choiceOf5 Render.string Render.string Render.string Render.string Render.string
                let jsonText = jsonWriter.GetJsonString(jsonRender)
                let expected = "\"5Of5\""
                equal "Renders Choice Value" expected jsonText

            testCase "Render Choice6Of6" <| fun _ ->
                use jsonWriter = new JsonWriter()
                // A union has an indeterminate structure so rendering is manual
                let jsonRender = Choice6Of6 "6Of6" |> Render.choiceOf6 Render.string Render.string Render.string Render.string Render.string Render.string
                let jsonText = jsonWriter.GetJsonString(jsonRender)
                let expected = "\"6Of6\""
                equal "Renders Choice Value" expected jsonText
                
            testCase "Render Choice7Of7" <| fun _ ->
                use jsonWriter = new JsonWriter()
                // A union has an indeterminate structure so rendering is manual
                let jsonRender = Choice7Of7 "7Of7" |> Render.choiceOf7 Render.string Render.string Render.string Render.string Render.string Render.string Render.string
                let jsonText = jsonWriter.GetJsonString(jsonRender)
                let expected = "\"7Of7\""
                equal "Renders Choice Value" expected jsonText
        ]

        // Render Auto Object
    ]
