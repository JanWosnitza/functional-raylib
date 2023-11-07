
type Condition =
    | Down
    | Pressed

[<RequireQualifiedAccess>]
type Action =
    | Up
    | Down
    | Left
    | Right
    | Select
    | Cancel

let down (key:Action) (event) = (Down, key, event)
let pressed (key:Action) (event) = (Pressed, key, event)

let map (mapper) (bindings:list<Condition * Action * 'a>) =
    bindings
    |> List.map (fun (condition, key, x) -> (condition, key, mapper x))
