module FSharp.Explicit.Json.Tests

open System
open Expecto

[<Tests>]
let allTests =
    testList "All Tests" [
        ParserTests.tests
    ]

[<EntryPoint>]
let main argv =
    printfn "Running tests!"

    let config =
        if System.Diagnostics.Debugger.IsAttached
        then defaultConfig
        else {defaultConfig with colour = Expecto.Logging.Colour0 }

    runTestsWithArgs config argv allTests
