
type Color = float * float * float

let color r g b : Color =
    (r, g, b)

module Color =
    let Black = color 0 0 0
    let White = color 0.9 0.9 0.9
    let Gray = color 0.6 0.6 0.6
    let Red = color 0.9 0 0
    let DarkRed = color 0.6 0 0
    let Green = color 0 0.9 0
    let DarkGreen = color 0 0.6 0
    let Blue = color 0 0 0.9
    let DarkBlue = color 0 0 0.6

type Rectangle = {
    Position : int * int
    Size : int * int
    Color : Color
}

module Rectangle =
    let scale (factor:int) (data:Rectangle) =
        {data with
            Position =
                let (x, y) = data.Position
                (x * factor, y * factor)
            Size =
                let (w, h) = data.Size
                (w * factor, h * factor)
        }

type Ellipse = {
    Position : int * int
    Size : int * int
    Color : Color
}

module Ellipse =
    let scale (factor:int) (data:Ellipse) =
        {data with
            Position =
                let (x, y) = data.Position
                (x * factor, y * factor)
            Size =
                let (w, h) = data.Size
                (w * factor, h * factor)
        }

type Text = {
    Position : int * int
    Size : float
    Color : Color
    Text : string
}

module Text =
    let scale (factor:int) (data:Text) =
        {data with
            Position =
                let (x, y) = data.Position
                (x * factor, y * factor)
            Size = data.Size * float factor
        }

type Node =
    | Rectangle of Rectangle
    | Ellipse of Ellipse
    | Text of Text

module Node =
    let scale (factor) =
        function
        | Rectangle data -> Rectangle (Rectangle.scale factor data)
        | Ellipse data -> Ellipse (Ellipse.scale factor data)
        | Text data -> Text (Text.scale factor data)

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
