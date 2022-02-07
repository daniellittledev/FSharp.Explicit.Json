module FSharp.Explicit.Json.Render

open System
open System.Text.Json

type Render = delegate of Utf8JsonWriter -> unit

type ObjectRenderer() =

    member inline _.Yield([<InlineIfLambda>] render: Render) =
        render

    member inline _.Run([<InlineIfLambda>] children: Render) : Render =
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

let object = ObjectRenderer ()

type ArrayRenderer() =

    member inline _.Yield([<InlineIfLambda>] render: Render) =
        render

    member inline _.Run([<InlineIfLambda>] children: Render) : Render =
        Render(fun x ->
            x.WriteStartArray()
            children.Invoke(x)
            x.WriteEndArray()
        )

    member inline _.Combine([<InlineIfLambda>] x1: Render, [<InlineIfLambda>] x2: Render) =
        Render(fun writer ->
            x1.Invoke(writer)
            x2.Invoke(writer)
        )

    member inline _.Delay([<InlineIfLambda>] f: unit -> Render) =
        f()

let array = ArrayRenderer ()

let inline prop (propertyName: string) (render: Render) : Render =
    Render(fun x ->
        x.WritePropertyName propertyName
        render.Invoke(x)
    )

let inline unit () : Render =
    Render(fun (x: Utf8JsonWriter) -> x.WriteNullValue ())

let inline bool (value: bool) : Render =
    Render(fun (x: Utf8JsonWriter) -> x.WriteBooleanValue value)

let inline byte (value: byte) : Render =
    Render(fun (x: Utf8JsonWriter) -> x.WriteNumberValue (int value))

let inline int (value: int) : Render =
    Render(fun (x: Utf8JsonWriter) -> x.WriteNumberValue value)

let inline long (value: int64) : Render =
    Render(fun (x: Utf8JsonWriter) -> x.WriteNumberValue value)

let inline single long (value: float32) : Render =
    Render(fun (x: Utf8JsonWriter) -> x.WriteNumberValue value)

let inline double (value: float) : Render =
    Render(fun (x: Utf8JsonWriter) -> x.WriteNumberValue value)

let inline decimal (value: decimal) : Render =
    Render(fun (x: Utf8JsonWriter) -> x.WriteNumberValue value)

let inline uint32 (value: uint32) : Render =
    Render(fun (x: Utf8JsonWriter) -> x.WriteNumberValue value)

let inline uint64 (value: uint64) : Render =
    Render(fun (x: Utf8JsonWriter) -> x.WriteNumberValue value)

let inline string (value: string) : Render =
    Render(fun (x: Utf8JsonWriter) -> x.WriteStringValue value)

let inline datetime (value: DateTime) : Render =
    Render(fun (x: Utf8JsonWriter) -> x.WriteStringValue value)

let inline datetimeoffset (value: DateTimeOffset) : Render =
    Render(fun (x: Utf8JsonWriter) -> x.WriteStringValue value)

let inline timespan (value: TimeSpan) : Render =
    Render(fun (x: Utf8JsonWriter) -> x.WriteStringValue (value.ToString("c")))

let inline enum (value: Enum) : Render =
    Render(fun (x: Utf8JsonWriter) ->
        let baseValue = value :> obj
        let enumType = Enum.GetUnderlyingType(value.GetType())
        if enumType = typeof<byte> then
            (baseValue :?> int) |> x.WriteNumberValue
        elif enumType = typeof<int> then
            (baseValue :?> int) |> x.WriteNumberValue
        elif enumType = typeof<int64> then
            (baseValue :?> int64) |> x.WriteNumberValue
        elif enumType = typeof<float> then
            (baseValue :?> float) |> x.WriteNumberValue
        elif enumType = typeof<float32> then
            (baseValue :?> float32) |> x.WriteNumberValue
        elif enumType = typeof<decimal> then
            (baseValue :?> decimal) |> x.WriteNumberValue
        elif enumType = typeof<uint32> then
            (baseValue :?> uint32) |> x.WriteNumberValue
        elif enumType = typeof<uint64> then
            (baseValue :?> uint64) |> x.WriteNumberValue
        else
            raise (NotSupportedException (sprintf "Enum has an unsupported base type %A" (x.GetType())))
    )

let inline enumName (value: 't when 't :> Enum) : Render =
    Render(fun (x: Utf8JsonWriter) -> Enum.GetName(typeof<'t>, value) |> x.WriteStringValue)

let inline guid (value: Guid) : Render =
    Render(fun (x: Utf8JsonWriter) -> x.WriteStringValue value)
