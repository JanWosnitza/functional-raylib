#load "App.fsx"

type State =
    {
        TimeOffset : System.TimeSpan
        App : App.State
        Snapshot : System.TimeSpan * App.State * Pcg.Pcg32
    }

type Event =
    | AppEvent of App.Event
    | StoreSnapshot
    | RestoreSnapshot
    | Tick

let InputBindings = [
    yield Input.pressed Input.Action.StoreSnapshot StoreSnapshot
    yield Input.pressed Input.Action.RestoreSnapshot RestoreSnapshot

    yield! App.InputBindings
        |> Input.map AppEvent
]

let makeInitial =  StateM.m<Pcg.Pcg32> {
    let! pcg = StateM.read
    let app = App.makeInitial
    return {
        TimeOffset = System.TimeSpan.Zero
        App = app
        Snapshot = (System.TimeSpan.Zero, app, pcg)
    }
}

let Update (state:State) (time:System.TimeSpan) (event:Event) = StateM.m<Pcg.Pcg32> {
    let appUpdate (event) = StateM.m<Pcg.Pcg32> {
        let! (app, commands) = App.Update state.App (time - state.TimeOffset) event
        let state = {state with App = app}
        return (state, commands)
    }

    match event with
    | AppEvent event ->
        return! appUpdate event

    | Tick ->
        return! appUpdate App.Tick

    | StoreSnapshot ->
        let! pcg = StateM.read
        let state =
            {state with
                Snapshot = (time - state.TimeOffset, state.App, pcg)
            }
        return (state, [])

    | RestoreSnapshot ->
        let (snapshotTime, app, pcg) = state.Snapshot
        do! StateM.write pcg
        let state =
            {state with
                TimeOffset = time - snapshotTime
                App = app
            }
        return (state, [])
}

let Draw (state:State) = App.Draw state.App
