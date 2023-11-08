#load "Input.fsx"
#load "Scene.fsx"
#load "Pcg.fsx"
#load "StateM.fsx"

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

let rec getFoodPosition (forbiddenPositions:list<int * int>) =
    StateM.m<Pcg.Pcg32> {
    let! x = Pcg.range (0, Config.FieldSizeX - 1)
    let! y = Pcg.range (0, Config.FieldSizeY - 1)
    if forbiddenPositions |> List.contains (x, y) |> not then
        return (x, y)
    else
        return! getFoodPosition forbiddenPositions
    }

let makeInitialState = StateM.m<Pcg.Pcg32> {
    let startPosition = (10, 10)
    let! foodPosition = getFoodPosition [startPosition]
    return {
            HeadPosition = startPosition
            LookDirection = Right
            LastLookDirection = Right
            TailPositions = []
            FoodPosition = foodPosition
            NextMove = Config.SecondsPerMove
        }
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

let Update (state:State) (time:System.TimeSpan) (event) = StateM.m<Pcg.Pcg32> {
    match event with
    | Look direction ->
        let state = 
            state
            |> updateDirection direction
        return (Some state, [])

    | Tick ->
        if time < state.NextMove then
            return (Some state, [])
        else
            let verify (state) =
                if isValid state then
                    Some state
                else
                    None

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
                let! foodPosition = getFoodPosition (position :: tail)
                let state =
                    {state with
                        HeadPosition = position
                        TailPositions = tail
                        LastLookDirection = state.LookDirection
                        FoodPosition = foodPosition
                        NextMove = NextMove
                    }
                return (verify state, [PlaySound "Snake_Eat.wav"])
            else
                let state =
                    {state with
                        HeadPosition = position
                        TailPositions = tail |> List.take state.TailPositions.Length
                        LastLookDirection = state.LookDirection
                        NextMove = NextMove
                    }
                return (verify state, [])
}

let Draw (state:State) = Scene.graph {
    let state = state

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
