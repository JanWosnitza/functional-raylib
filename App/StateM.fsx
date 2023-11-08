
type M<'State, 'a> = 'State -> 'State * 'a

let unit (x) (state:'State) = (state, x)

let bind (binder) (m:M<'State, _>) (state) =
    let (state, x) = m state
    binder x state

let map (mapper) (m:M<'State, _>) (state) =
    let (state, x) = m state
    (state, mapper x)

let apply (binder:'a -> M<'State, 'b>) (state, x) = binder x state

let read state = (state, state)

let write (state:'State) (_:'State) = (state, ())

let left (left:M<'State, _>) (right:M<'State, _>) (state) =
    let (state, x) = left state
    let (state, _) = right state
    (state, x)

let right (left:M<'State, _>) (right:M<'State, _>) (state) =
    let (state, _) = left state
    right state

type StateMBuilder<'State>() =
    member inline this.Bind(m:M<'State, _>, binder) : M<'State, _> = bind binder m

    member inline this.Delay(f) : M<'State, _> = f ()

    member inline this.Run(m:M<'State, _>) = m

    member inline this.For(items, binder) =
        (unit (), items)
        ||> Seq.fold (fun m item -> right m (binder item))

    member inline this.Zero() : M<'State, _>= unit ()

    member inline this.Return(x) : M<'State, _> = unit x

    member inline this.ReturnFrom(m:M<'State, _>) = m

    member inline this.Combine(l:M<'State, _>, r:M<'State, _>) = right l r

let m<'State> = StateMBuilder<'State>()
