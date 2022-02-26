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
        ]

        // Render Discriminated Union

        // Render Choice/Union/OneOf

        // Render Auto Object
    ]
