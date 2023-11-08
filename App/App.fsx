#load "Input.fsx"
#load "Scene.fsx"
#load "Menu.fsx"
#load "Snake.fsx"
#load "GameOfLife.fsx"

type Mode =
    | Menu
    | Snake of (System.TimeSpan * Snake.State)
    | GameOfLife of (System.TimeSpan * GameOfLife.State)

type State = {
    MenuState : Menu.State
    Mode : Mode
    Ticks : int
}

type Event =
    | MenuEvent of Menu.Event
    | SnakeEvent of Snake.Event
    | GameOfLifeEvent of GameOfLife.Event
    | Tick
    | Cancel

type Command =
    | PlaySound of string
    | Quit

let InputBindings = [
    yield Input.pressed Input.Action.Cancel Cancel

    yield! Menu.InputBindings
        |> Input.map MenuEvent

    yield! Snake.InputBindings
        |> Input.map SnakeEvent

    yield! GameOfLife.InputBindings
        |> Input.map GameOfLifeEvent
]

let makeInitial =
    {
        MenuState = Menu.makeInitial ()
        Mode = Menu
        Ticks = 0
    }

let Update (appState:State) (time:System.TimeSpan) (appEvent:Event) = StateM.m<Pcg.Pcg32> {
    if appEvent = Cancel then
        match appState.Mode with
        | Menu -> return (appState, [Quit])
        | _ ->
            return ({appState with Mode = Menu}, [])
    else
        match appState.Mode with
        | Menu ->
            match appEvent with
            | MenuEvent menuEvent ->
                match Menu.Update appState.MenuState menuEvent with
                | Menu.MenuState menuState ->
                    let appState =
                        {appState with
                            MenuState = menuState
                        }
                    return (appState, [])

                | Menu.StartSnake ->
                    let! snake = Snake.makeInitialState
                    let appState =
                        {appState with
                            Mode = Snake (time, snake)
                            Ticks = 0
                        }
                    return (appState, [])

                | Menu.StartGameOfLife ->
                    let! gol = GameOfLife.makeInitial
                    let appState =
                        {appState with
                            Mode = GameOfLife (time, gol)
                            Ticks = 0
                        }
                    return (appState, [])

                | Menu.Quit ->
                    return (appState, [Quit])
            
            | _ -> return (appState, [])

        | Snake snake ->
            let SnakeUpdate (startTime, snake) (event) = StateM.m<Pcg.Pcg32> {
                let! (snake, commands) = Snake.Update snake (time - startTime) event

                let mode =
                    snake
                    |> Option.map (fun state -> Snake (startTime, state))
                    |> Option.defaultWith (fun () ->
                        Menu
                    )

                let commands =
                    commands
                    
                    |> List.map (function
                        | Snake.PlaySound path -> PlaySound path
                    )

                return (mode, commands)
            }

            match appEvent with
            | SnakeEvent event ->
                let! (mode, commands) = SnakeUpdate snake event
                let appState = {appState with Mode = mode}
                return (appState, commands)

            | Tick ->
                let! (mode, commands) = SnakeUpdate snake Snake.Tick
                let appState = {appState with Mode = mode}
                return (appState, commands)
            
            | _ -> return (appState, [])

        | GameOfLife (startTime, gol) ->
            match appEvent with
            | GameOfLifeEvent event ->
                let! snake = GameOfLife.Update gol (time - startTime) event
                let appState =
                    {appState with
                        Mode = GameOfLife (startTime, snake)
                    }
                return (appState, [])

            | (Tick) ->
                let! gol = GameOfLife.Update gol (time - startTime) GameOfLife.Tick
                let appState =
                    {appState with
                        Mode = GameOfLife (startTime, gol)
                    }
                return (appState, [])

            | _ -> return (appState, [])
}

let Draw (state:State) = Scene.graph {
    match state.Mode with
    | Mode.Menu ->
        yield! Menu.Draw state.MenuState
    | Mode.Snake (_, snake) ->
        yield! Snake.Draw snake
    | Mode.GameOfLife (_, gol) ->
        yield! GameOfLife.Draw gol
}
