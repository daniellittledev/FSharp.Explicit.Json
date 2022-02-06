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
        
            // Render Unit

            // Render Number

            // Parse Dates

            // Parse TimeSpan

            // Parse Enum

            // Render Array
        ]

        // Render Option

        // Render Tuples

        // Render Union

        // Render OneOf

        // Render Auto Object
    ]
