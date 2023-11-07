
type Condition =
    | Down
    | Pressed

type Key =
    | UpKey
    | DownKey
    | LeftKey
    | RightKey
    | SelectKey
    | CancelKey

let down (key:Key) (event) = (Down, key, event)
let pressed (key:Key) (event) = (Pressed, key, event)

let map (mapper) (bindings:list<Condition * Key * 'a>) =
    bindings
    |> List.map (fun (condition, key, x) -> (condition, key, mapper x))
