#load "Input.fsx"
#load "Scene.fsx"
#load "Pcg.fsx"
#load "StateM.fsx"

let Config = {|
    FieldSizeX = 20
    FieldSizeY = 20
    SecondsPerUpdate = System.TimeSpan.FromSeconds(0.1)
|}

type State = {
    Field : bool[,]
    NextMove : System.TimeSpan
}

type Event =
    | Restart
    | Tick

let InputBindings = [
    Input.pressed Input.Action.Select Restart
]

let neighbors = [
    (-1,  1); (0, 1); (1,  1)
    (-1,  0);         (1,  0)
    (-1, -1); (0,-1); (1, -1)
]

let step (field : bool[,]) =
    let w = Array2D.length1 field
    let h = Array2D.length2 field
    field
    |> Array2D.mapi (fun x y v ->
        let count =
            neighbors
            |> Seq.filter (fun (dx, dy) -> field[(x + dx + w) % w, (y + dy + h) % h])
            |> Seq.length

        if v then
            count = 2 || count = 3
        else
            count = 3
    )

let makeField = StateM.m<Pcg.Pcg32> {
    let field = Array2D.zeroCreate Config.FieldSizeX Config.FieldSizeY

    for x = 0 to Config.FieldSizeX - 1 do
        for y = 0 to Config.FieldSizeY - 1 do
            let! value = Pcg.range (0, 4)
            field[x, y] <- value = 0

    return field
}

let makeInitial = StateM.m<Pcg.Pcg32> {
    let! field = makeField
    return {
            Field = field
            NextMove = Config.SecondsPerUpdate
        }
}

let Update (state:State) (time:System.TimeSpan) (event) = StateM.m<Pcg.Pcg32> {
    match event with
    | Restart ->
        let! field = makeField
        return
            {state with
                Field = field
            }

    | Tick ->
        if time < state.NextMove then
            return state
        else
            return
                {state with
                    NextMove = state.NextMove + Config.SecondsPerUpdate
                    Field = step state.Field
                }
}

let Draw (state:State) = Scene.graph {
    for x = 0 to Config.FieldSizeX - 1 do
        for y = 0 to Config.FieldSizeY - 1 do
            if state.Field[x, y] then
                yield Scene.Rectangle {
                    Position = (x, y)
                    Size = (1, 1)
                    Color = Scene.Color.Black
                }
}
