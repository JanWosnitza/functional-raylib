
[<RequireQualifiedAccess>]
type Color =
    | Black
    | DarkBlue
    | DarkGreen
    | DarkCyan
    | DarkRed
    | DarkMagenta
    | DarkYellow
    | Gray
    | DarkGray
    | Blue
    | Green
    | Cyan
    | Red
    | Magenta
    | Yellow
    | White

type Rectangle = {
    Position : int * int
    Size : int * int
    Color : Color
}

module Rectangle =
    let scale (scaleX, scaleY) (data:Rectangle) =
        {data with
            Position =
                let (x, y) = data.Position
                (x * scaleX, y * scaleY)
            Size =
                let (w, h) = data.Size
                (w * scaleX, h * scaleY)
        }

type Ellipse = {
    Position : int * int
    Size : int * int
    Color : Color
}

module Ellipse =
    let scale (scaleX, scaleY) (data:Ellipse) =
        {data with
            Position =
                let (x, y) = data.Position
                (x * scaleX, y * scaleY)
            Size =
                let (w, h) = data.Size
                (w * scaleX, h * scaleY)
        }

type Text = {
    Position : int * int
    Size : int
    Color : Color
    Text : string
}

module Text =
    let scale (scaleX, scaleY) (data:Text) =
        {data with
            Position =
                let (x, y) = data.Position
                (x * scaleX, y * scaleY)
            Size = data.Size * min scaleX scaleY
        }

type Node =
    | Rectangle of Rectangle
    | Ellipse of Ellipse
    | Text of Text

module Node =
    let scale (scale) =
        function
        | Rectangle data -> Rectangle (Rectangle.scale scale data)
        | Ellipse data -> Ellipse (Ellipse.scale scale data)
        | Text data -> Text (Text.scale scale data)

[<Struct>]
type Graph = Graph of seq<Node>

let bake (Graph graph) = Seq.toArray graph

let scale (factor) (Graph graph) =
    graph
    |> Seq.map (Node.scale factor)
    |> Graph

type GraphBuilder() =
    member inline this.Run(nodes:seq<Node>) = Graph nodes

    member inline this.Delay(getNodes:unit -> seq<Node>) = getNodes ()

    member inline this.Combine(leftNodes, right) : seq<Node> =
        Seq.append leftNodes right

    member inline this.Yield(x) : seq<Node> = Seq.singleton x

    member inline this.YieldFrom(Graph nodes) = nodes

    member inline this.Zero() : seq<Node> = Seq.empty

    member inline this.For(xs:seq<_>, f:_ -> seq<_>) =
        xs
        |> Seq.map f
        |> Seq.concat

let graph = GraphBuilder()
