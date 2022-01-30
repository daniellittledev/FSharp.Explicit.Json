module FSharp.Explicit.Json.RenderTests

open Expecto
open Expecto.Flip.Expect
open System.Text.Json
open System.IO
open FSharp.Explicit.Json.Render

let tests =
    testList "Render Tests" [
        
        testCase "Sample" <| fun _ ->

            let options = JsonWriterOptions()
            use stream = new MemoryStream()

            let writer = new Utf8JsonWriter(stream, options)

            let json =
                object {
                    prop "alpha" (object {
                        prop "beta" (Render.string "1")
                    })
                    prop "gamma" (Render.string "2")
                }
            json.Invoke(writer)

            writer.Flush()

            // Reset Stream
            stream.Seek(0L, SeekOrigin.Begin) |> ignore

            let reader = new StreamReader(stream)
            let result = reader.ReadToEnd()

            printfn "%A" result
            ()

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
