#load "App/Input.fsx"
#load "App/Scene.fsx"
#load "App/App.fsx"

open App
open System

let Scale = (4, 2)

let ColorMappings =
    [
        Scene.Color.Black, ConsoleColor.Black
        Scene.Color.DarkBlue, ConsoleColor.DarkBlue
        Scene.Color.DarkGreen, ConsoleColor.DarkGreen
        Scene.Color.DarkCyan, ConsoleColor.DarkCyan
        Scene.Color.DarkRed, ConsoleColor.DarkRed
        Scene.Color.DarkMagenta, ConsoleColor.DarkMagenta
        Scene.Color.DarkYellow, ConsoleColor.DarkYellow
        Scene.Color.Gray, ConsoleColor.Gray
        Scene.Color.DarkGray, ConsoleColor.DarkGray
        Scene.Color.Blue, ConsoleColor.Blue
        Scene.Color.Green, ConsoleColor.Green
        Scene.Color.Cyan, ConsoleColor.Cyan
        Scene.Color.Red, ConsoleColor.Red
        Scene.Color.Magenta, ConsoleColor.Magenta
        Scene.Color.Yellow, ConsoleColor.Yellow
        Scene.Color.White, ConsoleColor.White
    ]
    |> Map.ofSeq

let InputBindings =
    [
        Input.Action.Up, [|ConsoleKey.W|]
        Input.Action.Down, [|ConsoleKey.S|]
        Input.Action.Left, [|ConsoleKey.A|]
        Input.Action.Right, [|ConsoleKey.D|]
        Input.Action.Select, [|ConsoleKey.Spacebar; ConsoleKey.Enter|]
        Input.Action.Cancel, [|ConsoleKey.Escape|]
    ]

let ReadInput =
    let keyBindings =
        InputBindings
        |> Seq.collect (fun (action, keys) ->
            keys
            |> Seq.map (fun key -> (key, action))
        )
        |> Map.ofSeq

    let actionBindings =
        App.InputBindings
        |> Seq.map (fun (_, action, event) -> (action, event))
        |> Seq.groupBy fst
        |> Seq.map (fun (action, events) -> (action, events |> Seq.map snd |> Array.ofSeq))
        |> Map.ofSeq

    seq { while Console.KeyAvailable do yield Console.ReadKey(true) }
    |> Seq.choose (fun keyInfo -> Map.tryFind keyInfo.Key keyBindings)
    |> Seq.choose (fun action -> actionBindings |> Map.tryFind action)
    |> Seq.concat
    |> Seq.distinct

let Draw (scene) =
    let convertColor color = Map.find color ColorMappings

    scene
    |> Seq.iter (function
        | Scene.Rectangle data ->
            let (x, y) = data.Position
            let (width, height) = data.Size
            let line = String('#', width)
            Console.ForegroundColor <- convertColor data.Color
            for y = y to y + height - 1 do
                Console.SetCursorPosition(x, y)
                Console.Write(line)

        | Scene.Text data ->
            let (x, y) = data.Position

            Console.ForegroundColor <- convertColor data.Color
            data.Text.Split('\n')
            |> Array.iteri (fun lineNumber text ->
                Console.SetCursorPosition(x, y + lineNumber)
                Console.Write(text)
            )

        | Scene.Ellipse data ->
            let (x, y) = data.Position
            let (width, height) = data.Size
            let line = String('@', width)
            Console.ForegroundColor <- convertColor data.Color
            for y = y to y + height - 1 do
                Console.SetCursorPosition(x, y)
                Console.Write(line)
    )

let mutable State = App.makeInitial

let mutable ActiveCommands = []

let mutable Pcg = Pcg.make (uint64 Environment.TickCount64)

let AppUpdate (appState:App.State) (time) (event) =
    let (pcg, (appState, commands)) = App.Update appState time event Pcg
    Pcg <- pcg
    ActiveCommands <- commands @ ActiveCommands
    appState

let SizeX = Snake.Config.FieldSizeX * fst Scale
let SizeY = Snake.Config.FieldSizeY * snd Scale

let ClearText =
    String.Join('\n',
        String(' ', SizeX)
        |> Seq.replicate SizeY
    )

let ConsoleState = {|
    TreatControlCAsInput = Console.TreatControlCAsInput
    CursorVisible = Console.CursorVisible
    ForegroundColor = Console.ForegroundColor
    BackgroundColor = Console.BackgroundColor
|}
Console.TreatControlCAsInput <- true
Console.CursorVisible <- false

let watch = System.Diagnostics.Stopwatch.StartNew()
let mutable LastDrawnState = None
let mutable Quit = false

while not Quit do
    System.Threading.Thread.Sleep(1)

    let activeCommands = ActiveCommands
    ActiveCommands <- []

    let time = watch.Elapsed

    for event in ReadInput do
        State <- AppUpdate State time event

    for command in activeCommands do
        match command with
        | App.PlaySound path -> printf "\a"

        | App.Quit -> Quit <- true

    State <- AppUpdate State time App.Tick

    if LastDrawnState <> Some State then
        LastDrawnState <- Some State

        let sceneGraph =
            Scene.graph {
                yield! App.Draw State
                if State.Mode = App.Mode.Menu then
                    yield Scene.Text {
                        Position = (1, 12)
                        Size = 1
                        Color = Scene.Color.DarkGray
                        Text = "[Space] Select\n[W] Up\n[S] Down\n[A] Left\n[D] Right"
                    }
            }
            |> Scene.scale Scale

        Console.BackgroundColor <- ConsoleColor.Black
        Console.Clear()
        Console.BackgroundColor <- ConsoleColor.White
        Console.SetCursorPosition(0, 0)
        Console.Write(ClearText)

        sceneGraph
        |> Scene.bake
        |> Draw

Console.TreatControlCAsInput <- ConsoleState.TreatControlCAsInput
Console.CursorVisible <- ConsoleState.CursorVisible
Console.ForegroundColor <- ConsoleState.ForegroundColor
Console.BackgroundColor <- ConsoleState.BackgroundColor
Console.Clear()
