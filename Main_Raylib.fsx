#r "nuget: Raylib-cs"
#load "App/Input.fsx"
#load "App/Scene.fsx"
#load "App/App.fsx"

open Raylib_cs
open type Raylib
open App

type CBool with member this.AsBool = this = CBool(true)

let Scale = 30
let FramesPerSecond = 60

let InputBindings =
    [
        Input.UpKey, [|KeyboardKey.KEY_W; KeyboardKey.KEY_UP|]
        Input.DownKey, [|KeyboardKey.KEY_S; KeyboardKey.KEY_DOWN|]
        Input.LeftKey, [|KeyboardKey.KEY_A; KeyboardKey.KEY_LEFT|]
        Input.RightKey, [|KeyboardKey.KEY_D; KeyboardKey.KEY_RIGHT|]
        Input.SelectKey, [|KeyboardKey.KEY_SPACE; KeyboardKey.KEY_ENTER|]
        Input.CancelKey, [|KeyboardKey.KEY_ESCAPE|]
    ]
    |> Map.ofList

let ReadInput =
    let baked =
        App.InputBindings
        |> Seq.choose (fun (condition, action, event) ->
            InputBindings
            |> Map.tryFind action
            |> Option.map (fun hwKeys ->
                match condition with
                | Input.Down ->
                    fun () ->
                        let any = hwKeys |> Array.exists (fun hwKey -> IsKeyDown(hwKey).AsBool)
                        if any then Some event else None
                | Input.Pressed ->
                    fun () ->
                        let any = hwKeys |> Array.exists (fun hwKey -> IsKeyPressed(hwKey).AsBool)
                        if any then Some event else None
            )
        )

    baked
    |> Seq.choose (fun f -> f ())

let Draw (scene) =
    let convertColor ((r, g, b):Scene.Color) =
        let toByte (v:float) = v * 255.0 |> int |> min 255 |> max 0 |> byte
        Color(toByte r, toByte g, toByte b, 255uy)

    scene
    |> Seq.iter (function
        | Scene.Rectangle data ->
            let (x, y) = data.Position
            let (width, height) = data.Size
            DrawRectangle(x, y, width, height, convertColor data.Color)

        | Scene.Text data ->
            let (x, y) = data.Position
            DrawText(data.Text, x, y, int data.Size, convertColor data.Color)

        | Scene.Ellipse data ->
            let (x, y) = data.Position
            let (w, h) = data.Size
            DrawEllipse(x + w / 2, y + h / 2, float32 w / 2.0f, float32 h / 2.0f, convertColor data.Color)
    )

let mutable State = App.makeInitial (uint64 System.Environment.TickCount64)

let mutable ActiveCommands = []

let AppUpdate (appState:App.State) (time) (event) =
    let (appState, commands) = App.Update appState time event
    ActiveCommands <- commands @ ActiveCommands
    appState

InitAudioDevice()

InitWindow(
    Scale * Snake.Config.FieldSizeX,
    Scale * Snake.Config.FieldSizeY,
    "Functional Fun")
SetTargetFPS(FramesPerSecond)
DisableEventWaiting()
SetExitKey(LanguagePrimitives.EnumOfValue 0)

let watch = System.Diagnostics.Stopwatch.StartNew()
let mutable LastDrawnState = None

let mutable Quit = false

let Sounds = System.Collections.Generic.Dictionary<string, Sound>()

while not (WindowShouldClose().AsBool || Quit) do
    PollInputEvents()

    System.Threading.Thread.Sleep(1)

    let activeCommands = ActiveCommands
    ActiveCommands <- []

    let time = watch.Elapsed

    for event in ReadInput do
        State <- AppUpdate State time event

    for command in activeCommands do
        match command with
        | App.PlaySound path ->
            let sound =
                match Sounds.TryGetValue(path) with
                | true, sound -> sound
                | false, _ ->
                    let sound = LoadSound("Content/" + path)
                    Sounds.Add(path, sound)
                    sound
            Raylib.PlaySound(sound)
            |> ignore

        | App.Quit -> Quit <- true

    State <- AppUpdate State time App.Tick

    if LastDrawnState <> Some State then
        LastDrawnState <- Some State

        let sceneGraph =
            App.Draw State
            |> Scene.scale Scale

        BeginDrawing()
        ClearBackground(Color.RAYWHITE)
        
        sceneGraph
        |> Scene.bake
        |> Draw
        
        EndDrawing()

CloseWindow()

for item in Sounds do UnloadSound(item.Value)
CloseAudioDevice()