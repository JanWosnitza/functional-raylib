#load "Scene.fsx"
#load "Input.fsx"
#load "Scene.fsx"
#load "Pcg.fsx"

let Config = {|
    FieldSizeX = 20
    FieldSizeY = 20
    SecondsPerUpdate = System.TimeSpan.FromSeconds(0.1)
|}

type State = {
    Field : bool[,]
    Pcg : Pcg.Pcg
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

let makeField (pcg) =
    let mutable pcg = pcg
    let field =
        Array2D.init Config.FieldSizeX Config.FieldSizeY (fun _ _ ->
            let (pcg', value) = Pcg.range (0, 4) pcg
            pcg <- pcg'
            value = 0
        )
        |> step

    (pcg, field)

let makeInitial (seed) =
    let (pcg, field) = Pcg.make seed |> makeField
    {
        Field = field
        Pcg = pcg
        NextMove = Config.SecondsPerUpdate
    }

let Update (state:State) (time:System.TimeSpan) (event) =
    match event with
    | Restart ->
        let (pcg, field) = makeField state.Pcg
        {state with
            Field = field
            Pcg = pcg
        }
    | Tick ->
        if time < state.NextMove then state else
        {state with
            NextMove = state.NextMove + Config.SecondsPerUpdate
            Field = step state.Field
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
