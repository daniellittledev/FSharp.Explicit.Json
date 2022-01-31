## FSharp.Explicit.Json

### Parsing

```fsharp
let result =
    doc |> Parse.document (fun node -> validation {
        let! alpha = node.prop "alpha" Parse.string
        and! beta = node.prop "beta" Parse.string
        
        and! gamma =
            node.prop "gamma" (
                Parse.object <| fun node -> validation {
                    let! alpha = node.prop "alpha" Parse.string
                    and! beta = node.prop "beta" Parse.string

                    return
                        {
                            alpha = alpha
                            beta = beta
                        }
                })

        return
            {
                alpha = alpha
                beta = beta
                gamma = gamma
            }
    })
```

### Rendering

```
let writer = new Utf8JsonWriter(stream, options)

let json =
    object {
        
        prop "alpha" (Render.string "1")
        prop "beta" (Render.string "2")
        prop "gamma" (object {
            prop "alpha" (Render.string "3")
            prop "beta" (Render.string "4")
        })
    }
json.Invoke(writer)
```
