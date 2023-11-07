#load "Input.fsx"
#load "Scene.fsx"
#load "Menu.fsx"
#load "Snake.fsx"
#load "GameOfLife.fsx"

type Mode =
    | Menu
    | Snake of (System.TimeSpan * Snake.State)
    | GameOfLife of (System.TimeSpan * GameOfLife.State)

type State' = {
    MenuState : Menu.State
    Mode : Mode
    Ticks : int
}

type State = Pcg.PcgV<State'>

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
    Pcg.PcgV.make (fun () -> Pcg.pcgM {
        return {
                MenuState = Menu.makeInitial ()
                Mode = Menu
                Ticks = 0
            }
    })

let Update (appState:State) (time:System.TimeSpan) (appEvent:Event) =
    appState |> Pcg.PcgV.apply (fun appState ->
    Pcg.pcgM {

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
                    let! value = Pcg.PcgM.raw
                    let snake = Snake.makeInitialState (uint64 value)
                    let appState =
                        {appState with
                            Mode = Snake (time, snake)
                            Ticks = 0
                        }
                    return (appState, [])

                | Menu.StartGameOfLife ->
                    let! value = Pcg.PcgM.raw
                    let gol = GameOfLife.makeInitial (uint64 value)
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
            let SnakeUpdate (startTime, snake) (event) =
                let (snake, commands) = Snake.Update snake (time - startTime) event

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

                (mode, commands)

            match appEvent with
            | SnakeEvent event ->
                let (mode, commands) = SnakeUpdate snake event
                let appState = {appState with Mode = mode}
                return (appState, commands)

            | Tick ->
                let (mode, commands) = SnakeUpdate snake Snake.Tick
                let appState = {appState with Mode = mode}
                return (appState, commands)
            
            | _ -> return (appState, [])

        | GameOfLife (startTime, gol) ->
            match appEvent with
            | GameOfLifeEvent event ->
                let appState =
                    {appState with
                        Mode = GameOfLife (startTime, GameOfLife.Update gol (time - startTime) event)
                    }
                return (appState, [])

            | (Tick) ->
                let appState =
                    {appState with
                        Mode = GameOfLife (startTime, GameOfLife.Update gol (time - startTime) GameOfLife.Tick)
                    }
                return (appState, [])

            | _ -> return (appState, [])
    })
    |> Pcg.PcgV.split id

let Draw (state:State) = Scene.graph {
    let state = state.Value

    match state.Mode with
    | Mode.Menu ->
        yield! Menu.Draw state.MenuState
    | Mode.Snake (_, snake) ->
        yield! Snake.Draw snake
    | Mode.GameOfLife (_, gol) ->
        yield! GameOfLife.Draw gol
}
