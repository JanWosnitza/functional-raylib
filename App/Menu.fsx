#load "Scene.fsx"
#load "Input.fsx"

type Event =
    | Select
    | Next
    | Previous

type State = | ItemStartSnake | ItemStartGameOfLife | ItemQuit

type Result =
    | MenuState of State
    | StartSnake
    | StartGameOfLife
    | Quit

let InputBindings = [
    Input.pressed Input.UpKey Previous
    Input.pressed Input.DownKey Next
    Input.pressed Input.SelectKey Select
]

let makeInitial () = ItemStartSnake

let Items = [
    ItemStartSnake, StartSnake, "Severus Snake"
    ItemStartGameOfLife, StartGameOfLife, "Gone Ways"
    ItemQuit, Quit, "yeet"
]

let Update (state:State) (event) : Result =
    let move (add) =
        let idx =
            Items
            |> List.findIndex (fun (item, _, _) -> state = item)
            |> (+) add

        match Items |> List.tryItem idx with
        | Some (item, _, _) -> MenuState item
        | None -> MenuState state

    match event with
    | Select ->
        Items
        |> Seq.choose (fun (item, result, _) ->
            if item = state then
                Some result
            else
                None
        )
        |> Seq.head

    | Next -> move 1
    | Previous -> move (-1)

let Draw (state:State) = Scene.graph {
    yield Scene.Text {
        Position = (1, 13)
        Size = 0.8
        Color = Scene.Color.Gray
        Text = "[Space] Select\n[W] Up\n[S] Down\n[A] Left\n[D] Right"
    }

    yield Scene.Text {
        Position = (5, 5)
        Size = 1.5
        Color = Scene.Color.Black
        Text = System.String.Join(
            '\n',
            Items
            |> Seq.map (fun (item, _, text) ->
                sprintf "%s%s" (if state = item then "->" else "  ") text
            )
        )
    }
}
