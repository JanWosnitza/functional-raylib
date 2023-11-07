#load "Input.fsx"
#load "Scene.fsx"
#load "Pcg.fsx"

let Config = {|
    FieldSizeX = 20
    FieldSizeY = 20
    SnakeHeadColor = Scene.Color.Green
    SnakeTailColor =  Scene.Color.DarkGreen
    FoodColor = Scene.Color.Red
    SecondsPerMove = System.TimeSpan.FromSeconds(0.1)
|}

type Direction = | Up | Down | Left | Right

type State = {
    LastLookDirection : Direction
    LookDirection : Direction
    HeadPosition : int * int
    TailPositions : list<int * int>
    FoodPosition : int * int
    Pcg : Pcg.Pcg
    NextMove : System.TimeSpan
}

type Event =
    | Look of Direction
    | Tick

type Command =
    | PlaySound of string

let InputBindings = [
    Input.down Input.Action.Up (Look Up)
    Input.down Input.Action.Down (Look Down)
    Input.down Input.Action.Left (Look Left)
    Input.down Input.Action.Right (Look Right)
]

let getFoodPosition (forbiddenPositions:list<int * int>) (pcg) : Pcg.Pcg * (int * int) =
    pcg
    |> Seq.unfold (fun pcg ->
        let (pcg, x) = pcg |> Pcg.range (0, Config.FieldSizeX - 1)
        let (pcg, y) = pcg |> Pcg.range (0, Config.FieldSizeX - 1)
        Some ((pcg, (x, y)), pcg)
    )
    |> Seq.filter (fun (_, position) -> forbiddenPositions |> List.contains position |> not)
    |> Seq.head

let makeInitialState (seed) = 
    let position = (10, 10)
    let pcg = Pcg.make seed
    let (pcg, foodPosition) = pcg |> getFoodPosition [position]
    { HeadPosition = (10, 10)
      LookDirection = Right
      LastLookDirection = Right
      TailPositions = []
      FoodPosition = foodPosition
      Pcg = pcg
      NextMove = Config.SecondsPerMove
    }

let updateDirection (direction:Direction) (state) =
    match (state.LastLookDirection, direction) with
    | (Up, Down) | (Down, Up)
    | (Left, Right) | (Right, Left) -> state
    | (_, direction) -> {state with LookDirection = direction}

let isValid (state:State) =
    let checkSelfCollision (postion) (tail) =
        tail
        |> List.contains postion
        |> not

    let checkBounds (x, y) =
        x >= 0 && x < Config.FieldSizeX
        && y >= 0 && y < Config.FieldSizeY

    checkSelfCollision state.HeadPosition state.TailPositions
    && checkBounds state.HeadPosition

let Update (state) (time:System.TimeSpan) (event) : option<State> * list<Command> =
    match event with
    | Look direction ->
        let state =
            state
            |> updateDirection direction
        (Some state, [])

    | Tick ->
        if time < state.NextMove then (Some state, []) else
        let (state, commands) =
            let position =
                let (x, y) = state.HeadPosition
                match state.LookDirection with
                | Up    -> (x, y - 1)
                | Down  -> (x, y + 1)
                | Left  -> (x - 1, y)
                | Right -> (x + 1, y)

            let tail = state.HeadPosition :: state.TailPositions
            let NextMove = time + Config.SecondsPerMove
            if state.FoodPosition = position then
                let (pcg, foodPosition) = state.Pcg |> getFoodPosition (position :: tail)
                let state =
                    {state with
                        HeadPosition = position
                        TailPositions = tail
                        LastLookDirection = state.LookDirection
                        FoodPosition = foodPosition
                        Pcg = pcg
                        NextMove = NextMove
                    }
                (state, [PlaySound "Snake_Eat.wav"])
            else
                let state =
                    {state with
                        HeadPosition = position
                        TailPositions = tail |> List.take state.TailPositions.Length
                        LastLookDirection = state.LookDirection
                        NextMove = NextMove
                    }
                (state, [])

        if isValid state then
            (Some state, commands)
        else
            (None, commands)

let Draw (state:State) = Scene.graph {
    yield Scene.Ellipse {
        Position = state.FoodPosition
        Size = (1, 1)
        Color = Config.FoodColor
    }

    for pos in state.TailPositions do
        yield Scene.Rectangle {
            Position = pos
            Size = (1, 1)
            Color = Config.SnakeTailColor
        }

    yield Scene.Rectangle {
        Position = state.HeadPosition
        Size = (1, 1)
        Color = Config.SnakeHeadColor
    }
}
