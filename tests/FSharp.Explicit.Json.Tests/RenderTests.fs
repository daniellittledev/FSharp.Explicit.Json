module FSharp.Explicit.Json.RenderTests

open System
open Expecto
open Expecto.Flip.Expect
open System.Text.Json
open System.IO
open FSharp.Explicit.Json.Render

type JsonWriter() =
    let options = JsonWriterOptions(Indented = true)
    let stream = new MemoryStream()
    let writer = new Utf8JsonWriter(stream, options)

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
            use jsonWriter = new JsonWriter()
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

            // Render TimeSpan

            // Render Enum

            // Render Array
        ]

        // Render Option

        // Render Tuples

        // Render Discriminated Union

        // Render Choice/Union/OneOf

        // Render Auto Object
    ]
