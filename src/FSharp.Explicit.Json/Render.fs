module FSharp.Explicit.Json.Render

open System.Text.Json

type Render = delegate of Utf8JsonWriter -> unit

type ObjectRenderer() =

    member inline _.Yield([<InlineIfLambda>] render: Render) =
        render

    member inline this.Run([<InlineIfLambda>] children: Render) : Render =
        Render(fun x ->
            x.WriteStartObject()
            children.Invoke(x)
            x.WriteEndObject()
        )

    member inline _.Combine([<InlineIfLambda>] x1: Render, [<InlineIfLambda>] x2: Render) =
        Render(fun writer ->
            x1.Invoke(writer)
            x2.Invoke(writer)
        )

    member inline _.Delay([<InlineIfLambda>] f: unit -> Render) =
        f()

let inline prop (propertyName: string) (render: Render) : Render =
    Render(fun x ->
        x.WritePropertyName propertyName
        render.Invoke(x)
    )

// null

let inline string (value: string) : Render =
    Render(fun (x: Utf8JsonWriter) -> x.WriteStringValue value)

// array

let object = ObjectRenderer ()
