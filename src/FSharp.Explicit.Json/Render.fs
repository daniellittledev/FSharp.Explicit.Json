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

let inline option (render: 't -> Render) (value: 't option) : Render =
    match value with
    | Some some -> render some
    | None -> unit ()

let inline tuple2 (renderA: 'a -> Render) (renderB: 'b -> Render) (a: 'a, b: 'b) : Render =
    array { renderA a; renderB b }

let inline tuple3 (renderA: 'a -> Render) (renderB: 'b -> Render) (renderC: 'c -> Render) (a: 'a, b: 'b, c: 'c) : Render =
    array { renderA a; renderB b; renderC c}

let inline tuple4 (renderA: 'a -> Render) (renderB: 'b -> Render) (renderC: 'c -> Render) (renderD: 'd -> Render) (a: 'a, b: 'b, c: 'c, d: 'd) : Render =
    array { renderA a; renderB b; renderC c; renderD d}

let inline tuple5 (renderA: 'a -> Render) (renderB: 'b -> Render) (renderC: 'c -> Render) (renderD: 'd -> Render) (renderE: 'e -> Render) (a: 'a, b: 'b, c: 'c, d: 'd, e: 'e) : Render =
    array { renderA a; renderB b; renderC c; renderD d; renderE e}

let inline choiceOf2 (renderA: 'a -> Render) (renderB: 'b -> Render) (choice: Choice<'a, 'b>) : Render =
    match choice with
    | Choice1Of2 a -> renderA a
    | Choice2Of2 b -> renderB b

let inline choiceOf3 (renderA: 'a -> Render) (renderB: 'b -> Render) (renderC: 'c -> Render) (choice: Choice<'a, 'b, 'c>) : Render =
    match choice with
    | Choice1Of3 a -> renderA a
    | Choice2Of3 b -> renderB b
    | Choice3Of3 c -> renderC c

let inline choiceOf4 (renderA: 'a -> Render) (renderB: 'b -> Render) (renderC: 'c -> Render) (renderD: 'd -> Render) (choice: Choice<'a, 'b, 'c, 'd>) : Render =
    match choice with
    | Choice1Of4 a -> renderA a
    | Choice2Of4 b -> renderB b
    | Choice3Of4 c -> renderC c
    | Choice4Of4 d -> renderD d

let inline choiceOf5 (renderA: 'a -> Render) (renderB: 'b -> Render) (renderC: 'c -> Render) (renderD: 'd -> Render) (renderE: 'e -> Render) (choice: Choice<'a, 'b, 'c, 'd, 'e>) : Render =
    match choice with
    | Choice1Of5 a -> renderA a
    | Choice2Of5 b -> renderB b
    | Choice3Of5 c -> renderC c
    | Choice4Of5 d -> renderD d
    | Choice5Of5 e -> renderE e

let inline choiceOf6 (renderA: 'a -> Render) (renderB: 'b -> Render) (renderC: 'c -> Render) (renderD: 'd -> Render) (renderE: 'e -> Render) (renderF: 'f -> Render) (choice: Choice<'a, 'b, 'c, 'd, 'e, 'f>) : Render =
    match choice with
    | Choice1Of6 a -> renderA a
    | Choice2Of6 b -> renderB b
    | Choice3Of6 c -> renderC c
    | Choice4Of6 d -> renderD d
    | Choice5Of6 e -> renderE e
    | Choice6Of6 f -> renderF f

let inline choiceOf7 (renderA: 'a -> Render) (renderB: 'b -> Render) (renderC: 'c -> Render) (renderD: 'd -> Render) (renderE: 'e -> Render) (renderF: 'f -> Render) (renderG: 'g -> Render) (choice: Choice<'a, 'b, 'c, 'd, 'e, 'f, 'g>) : Render =
    match choice with
    | Choice1Of7 a -> renderA a
    | Choice2Of7 b -> renderB b
    | Choice3Of7 c -> renderC c
    | Choice4Of7 d -> renderD d
    | Choice5Of7 e -> renderE e
    | Choice6Of7 f -> renderF f
    | Choice7Of7 g -> renderG g
