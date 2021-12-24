open System.Collections.Generic
open AoC.Magic

type Amphipod =
    | A
    | B
    | C
    | D

let amphipod =
    function
    | "A" -> A
    | "B" -> B
    | "C" -> C
    | "D" -> D

let unitCost =
    function
    | A -> 1
    | B -> 10
    | C -> 100
    | D -> 1000

type Room =
    | Initial of Amphipod list
    | Filling of int

type Position =
    { Hallway: Amphipod option * Amphipod option * Amphipod option * Amphipod option * Amphipod option * Amphipod option * Amphipod option
      Rooms: Room * Room * Room * Room }

let hasOnly xx = List.forall (fun x -> x = xx)

let solved n =
    { Hallway = None, None, None, None, None, None, None
      Rooms = Filling n, Filling n, Filling n, Filling n }

let initial x xs =
    if hasOnly x xs then
        xs |> List.length |> Filling
    else
        Initial xs

let cost a x = (unitCost a) * x

let movesFromA n p =
    let { Hallway = l2, l1, ab, bc, cd, r1, r2
          Rooms = a, b, c, d } =
        p

    match a with
    | Initial (x :: rest) ->
        let rms = initial A rest, b, c, d
        let exit = n - List.length rest

        seq {
            if l1 = None then
                yield
                    { Hallway = l2, Some x, ab, bc, cd, r1, r2
                      Rooms = rms },
                    cost x <| 1 + exit

                if l2 = None then
                    yield
                        { Hallway = Some x, None, ab, bc, cd, r1, r2
                          Rooms = rms },
                        cost x <| 2 + exit

            if ab = None then
                yield
                    { Hallway = l2, l1, Some x, bc, cd, r1, r2
                      Rooms = rms },
                    cost x <| 1 + exit

                if bc = None then
                    yield
                        { Hallway = l2, l1, None, Some x, cd, r1, r2
                          Rooms = rms },
                        cost x <| 3 + exit

                    if cd = None then
                        yield
                            { Hallway = l2, l1, None, None, Some x, r1, r2
                              Rooms = rms },
                            cost x <| 5 + exit

                        if r1 = None then
                            yield
                                { Hallway = l2, l1, None, None, None, Some x, r2
                                  Rooms = rms },
                                cost x <| 7 + exit

                            if r2 = None then
                                yield
                                    { Hallway = l2, l1, None, None, None, None, Some x
                                      Rooms = rms },
                                    cost x <| 8 + exit
        }
    | Filling _ -> []

let movesFromB n p =
    let { Hallway = l2, l1, ab, bc, cd, r1, r2
          Rooms = a, b, c, d } =
        p

    match b with
    | Initial (x :: rest) ->
        let rms = a, initial B rest, c, d
        let exit = n - List.length rest

        seq {
            if ab = None then
                yield
                    { Hallway = l2, l1, Some x, bc, cd, r1, r2
                      Rooms = rms },
                    cost x <| 1 + exit

                if l1 = None then
                    yield
                        { Hallway = l2, Some x, None, bc, cd, r1, r2
                          Rooms = rms },
                        cost x <| 3 + exit

                    if l2 = None then
                        yield
                            { Hallway = Some x, None, None, bc, cd, r1, r2
                              Rooms = rms },
                            cost x <| 4 + exit

            if bc = None then
                yield
                    { Hallway = l2, l1, ab, Some x, cd, r1, r2
                      Rooms = rms },
                    cost x <| 1 + exit

                if cd = None then
                    yield
                        { Hallway = l2, l1, ab, None, Some x, r1, r2
                          Rooms = rms },
                        cost x <| 3 + exit

                    if r1 = None then
                        yield
                            { Hallway = l2, l1, ab, None, None, Some x, r2
                              Rooms = rms },
                            cost x <| 5 + exit

                        if r2 = None then
                            yield
                                { Hallway = l2, l1, ab, None, None, None, Some x
                                  Rooms = rms },
                                cost x <| 6 + exit
        }
    | Filling _ -> []


let movesFromC n p =
    let { Hallway = l2, l1, ab, bc, cd, r1, r2
          Rooms = a, b, c, d } =
        p

    match c with
    | Initial (x :: rest) ->
        let rms = a, b, initial C rest, d
        let exit = n - List.length rest

        seq {
            if bc = None then
                yield
                    { Hallway = l2, l1, ab, Some x, cd, r1, r2
                      Rooms = rms },
                    cost x <| 1 + exit

                if ab = None then
                    yield
                        { Hallway = l2, l1, Some x, None, cd, r1, r2
                          Rooms = rms },
                        cost x <| 3 + exit

                    if l1 = None then
                        yield
                            { Hallway = l2, Some x, None, None, cd, r1, r2
                              Rooms = rms },
                            cost x <| 5 + exit

                        if l2 = None then
                            yield
                                { Hallway = Some x, None, None, None, cd, r1, r2
                                  Rooms = rms },
                                cost x <| 6 + exit

            if cd = None then
                yield
                    { Hallway = l2, l1, ab, bc, Some x, r1, r2
                      Rooms = rms },
                    cost x <| 1 + exit

                if r1 = None then
                    yield
                        { Hallway = l2, l1, ab, bc, None, Some x, r2
                          Rooms = rms },
                        cost x <| 3 + exit

                    if r2 = None then
                        yield
                            { Hallway = l2, l1, ab, bc, None, None, Some x
                              Rooms = rms },
                            cost x <| 4 + exit
        }
    | Filling _ -> []


let movesFromD n p =
    let { Hallway = l2, l1, ab, bc, cd, r1, r2
          Rooms = a, b, c, d } =
        p

    match d with
    | Initial (x :: rest) ->
        let rms = a, b, c, initial D rest
        let exit = n - List.length rest

        seq {
            if cd = None then
                yield
                    { Hallway = l2, l1, ab, bc, Some x, r1, r2
                      Rooms = rms },
                    cost x <| 1 + exit

                if bc = None then
                    yield
                        { Hallway = l2, l1, ab, Some x, None, r1, r2
                          Rooms = rms },
                        cost x <| 3 + exit

                    if ab = None then
                        yield
                            { Hallway = l2, l1, Some x, None, None, r1, r2
                              Rooms = rms },
                            cost x <| 5 + exit

                        if l1 = None then
                            yield
                                { Hallway = l2, Some x, None, None, None, r1, r2
                                  Rooms = rms },
                                cost x <| 7 + exit

                            if l2 = None then
                                yield
                                    { Hallway = Some x, None, None, None, None, r1, r2
                                      Rooms = rms },
                                    cost x <| 8 + exit

            if r1 = None then
                yield
                    { Hallway = l2, l1, ab, bc, cd, Some x, r2
                      Rooms = rms },
                    cost x <| 1 + exit

                if r2 = None then
                    yield
                        { Hallway = l2, l1, ab, bc, cd, None, Some x
                          Rooms = rms },
                        cost x <| 2 + exit
        }
    | Filling _ -> []


let moves n p =
    match p with
    // --- A in ---
    | { Hallway = (Some A, None, ab, bc, cd, r1, r2)
        Rooms = (Filling m, b, c, d) } ->
        [ { Hallway = (None, None, ab, bc, cd, r1, r2)
            Rooms = Filling(m + 1), b, c, d },
          cost A <| 2 + (n - m) ]
    | { Hallway = (l2, Some A, ab, bc, cd, r1, r2)
        Rooms = (Filling m, b, c, d) } ->
        [ { Hallway = (l2, None, ab, bc, cd, r1, r2)
            Rooms = Filling(m + 1), b, c, d },
          cost A <| 1 + (n - m) ]
    | { Hallway = (l2, l1, Some A, bc, cd, r1, r2)
        Rooms = (Filling m, b, c, d) } ->
        [ { Hallway = (l2, l1, None, bc, cd, r1, r2)
            Rooms = Filling(m + 1), b, c, d },
          cost A <| 1 + (n - m) ]
    | { Hallway = (l2, l1, None, Some A, cd, r1, r2)
        Rooms = (Filling m, b, c, d) } ->
        [ { Hallway = (l2, l1, None, None, cd, r1, r2)
            Rooms = Filling(m + 1), b, c, d },
          cost A <| 3 + (n - m) ]
    | { Hallway = (l2, l1, None, None, Some A, r1, r2)
        Rooms = (Filling m, b, c, d) } ->
        [ { Hallway = (l2, l1, None, None, None, r1, r2)
            Rooms = Filling(m + 1), b, c, d },
          cost A <| 5 + (n - m) ]
    | { Hallway = (l2, l1, None, None, None, Some A, r2)
        Rooms = (Filling m, b, c, d) } ->
        [ { Hallway = (l2, l1, None, None, None, None, r2)
            Rooms = Filling(m + 1), b, c, d },
          cost A <| 7 + (n - m) ]
    | { Hallway = (l2, l1, None, None, None, None, Some A)
        Rooms = (Filling m, b, c, d) } ->
        [ { Hallway = (l2, l1, None, None, None, None, None)
            Rooms = Filling(m + 1), b, c, d },
          cost A <| 8 + (n - m) ]
    // --- B in ---
    | { Hallway = (Some B, None, None, bc, cd, r1, r2)
        Rooms = (a, Filling m, c, d) } ->
        [ { Hallway = (None, None, None, bc, cd, r1, r2)
            Rooms = a, Filling(m + 1), c, d },
          cost B <| 4 + (n - m) ]
    | { Hallway = (l2, Some B, None, bc, cd, r1, r2)
        Rooms = (a, Filling m, c, d) } ->
        [ { Hallway = (l2, None, None, bc, cd, r1, r2)
            Rooms = a, Filling(m + 1), c, d },
          cost B <| 3 + (n - m) ]
    | { Hallway = (l2, l1, Some B, bc, cd, r1, r2)
        Rooms = (a, Filling m, c, d) } ->
        [ { Hallway = (l2, l1, None, bc, cd, r1, r2)
            Rooms = a, Filling(m + 1), c, d },
          cost B <| 1 + (n - m) ]
    | { Hallway = (l2, l1, ab, Some B, cd, r1, r2)
        Rooms = (a, Filling m, c, d) } ->
        [ { Hallway = (l2, l1, ab, None, cd, r1, r2)
            Rooms = a, Filling(m + 1), c, d },
          cost B <| 1 + (n - m) ]
    | { Hallway = (l2, l1, ab, None, Some B, r1, r2)
        Rooms = (a, Filling m, c, d) } ->
        [ { Hallway = (l2, l1, ab, None, None, r1, r2)
            Rooms = a, Filling(m + 1), c, d },
          cost B <| 3 + (n - m) ]
    | { Hallway = l2, l1, ab, None, None, Some B, r2
        Rooms = a, Filling m, c, d } ->
        [ { Hallway = l2, l1, ab, None, None, None, r2
            Rooms = a, Filling(m + 1), c, d },
          cost B <| 5 + (n - m) ]
    | { Hallway = l2, l1, ab, None, None, None, Some B
        Rooms = a, Filling m, c, d } ->
        [ { Hallway = l2, l1, ab, None, None, None, None
            Rooms = a, Filling(m + 1), c, d },
          cost B <| 6 + (n - m) ]
    // --- C in ---
    | { Hallway = (Some C, None, None, None, cd, r1, r2)
        Rooms = (a, b, Filling m, d) } ->
        [ { Hallway = (None, None, None, None, cd, r1, r2)
            Rooms = a, b, Filling(m + 1), d },
          cost C <| 6 + (n - m) ]
    | { Hallway = (l2, Some C, None, None, cd, r1, r2)
        Rooms = (a, b, Filling m, d) } ->
        [ { Hallway = (l2, None, None, None, cd, r1, r2)
            Rooms = a, b, Filling(m + 1), d },
          cost C <| 5 + (n - m) ]
    | { Hallway = l2, l1, Some C, None, cd, r1, r2
        Rooms = a, b, Filling m, d } ->
        [ { Hallway = l2, l1, None, None, cd, r1, r2
            Rooms = a, b, Filling(m + 1), d },
          cost C <| 3 + (n - m) ]
    | { Hallway = l2, l1, ab, Some C, cd, r1, r2
        Rooms = a, b, Filling m, d } ->
        [ { Hallway = l2, l1, ab, None, cd, r1, r2
            Rooms = a, b, Filling(m + 1), d },
          cost C <| 1 + (n - m) ]
    | { Hallway = l2, l1, ab, bc, Some C, r1, r2
        Rooms = a, b, Filling m, d } ->
        [ { Hallway = l2, l1, ab, bc, None, r1, r2
            Rooms = a, b, Filling(m + 1), d },
          cost C <| 1 + (n - m) ]
    | { Hallway = l2, l1, ab, bc, None, Some C, r2
        Rooms = a, b, Filling m, d } ->
        [ { Hallway = l2, l1, ab, bc, None, None, r2
            Rooms = a, b, Filling(m + 1), d },
          cost C <| 3 + (n - m) ]
    | { Hallway = l2, l1, ab, bc, None, None, Some C
        Rooms = a, b, Filling m, d } ->
        [ { Hallway = l2, l1, ab, bc, None, None, None
            Rooms = a, b, Filling(m + 1), d },
          cost C <| 4 + (n - m) ]
    // --- D in ---
    | { Hallway = Some D, None, None, None, None, r1, r2
        Rooms = a, b, c, Filling m } ->
        [ { Hallway = None, None, None, None, None, r1, r2
            Rooms = a, b, c, Filling(m + 1) },
          cost D <| 8 + (n - m) ]
    | { Hallway = l2, Some D, None, None, None, r1, r2
        Rooms = a, b, c, Filling m } ->
        [ { Hallway = l2, None, None, None, None, r1, r2
            Rooms = a, b, c, Filling(m + 1) },
          cost D <| 7 + (n - m) ]
    | { Hallway = l2, l1, Some D, None, None, r1, r2
        Rooms = a, b, c, Filling m } ->
        [ { Hallway = l2, l1, None, None, None, r1, r2
            Rooms = a, b, c, Filling(m + 1) },
          cost D <| 5 + (n - m) ]
    | { Hallway = l2, l1, ab, Some D, None, r1, r2
        Rooms = a, b, c, Filling m } ->
        [ { Hallway = l2, l1, ab, None, None, r1, r2
            Rooms = a, b, c, Filling(m + 1) },
          cost D <| 3 + (n - m) ]
    | { Hallway = l2, l1, ab, bc, Some D, r1, r2
        Rooms = a, b, c, Filling m } ->
        [ { Hallway = (l2, l1, ab, bc, None, r1, r2)
            Rooms = a, b, c, Filling(m + 1) },
          cost D <| 1 + (n - m) ]
    | { Hallway = l2, l1, ab, bc, cd, Some D, r2
        Rooms = a, b, c, Filling m } ->
        [ { Hallway = l2, l1, ab, bc, cd, None, r2
            Rooms = a, b, c, Filling(m + 1) },
          cost D <| 1 + (n - m) ]
    | { Hallway = l2, l1, ab, bc, cd, None, Some D
        Rooms = a, b, c, Filling m } ->
        [ { Hallway = l2, l1, ab, bc, cd, None, None
            Rooms = a, b, c, Filling(m + 1) },
          cost D <| 2 + (n - m) ]
    // --- out ---
    | _ ->
        Seq.concat [ movesFromA n p
                     movesFromB n p
                     movesFromC n p
                     movesFromD n p ]
        |> List.ofSeq

let parse s =
    let [ one; two ] =
        nonEmptyLines s
        |> Seq.skip 2
        |> Seq.take 2
        |> List.ofSeq

    let [| _; _; _; a1; b1; c1; d1; _; _; _ |] = one.Split('#')
    let [| _; a2; b2; c2; d2; _ |] = two.Split('#')

    { Hallway = None, None, None, None, None, None, None
      Rooms =
          initial A [ amphipod a1; amphipod a2 ],
          initial B [ amphipod b1; amphipod b2 ],
          initial C [ amphipod c1; amphipod c2 ],
          initial D [ amphipod d1; amphipod d2 ] }

let expandToPart2 pos =
    let expandRoom a2 a3 room =
        match room with
        | Initial room ->
            Initial
            <| (List.head room) :: a2 :: a3 :: (List.tail room)

    let a, b, c, d = pos.Rooms

    { pos with
          Rooms = a |> expandRoom D D, b |> expandRoom C B, c |> expandRoom B A, d |> expandRoom A C }

let dijkstra n pos =
    let target = solved n

    let seen = HashSet()
    let Q = PriorityQueue()
    let dist = Dictionary()
//    let prev = Dictionary()

    dist.Add(pos, 0)
    Q.Enqueue(pos, 0)

    while Q.Count > 0 do
        let p = Q.Dequeue()
        if p = target then Q.Clear() // improvised break

        if not <| seen.Contains(p) then
            seen.Add(p) |> ignore
            let d = dist.Item(p)

            let ps = moves n p

            for p', d' in ps do
                if not <| seen.Contains(p') then
                    let alt = d + d'
                    let found, oldD = dist.TryGetValue(p')

                    if not found || oldD > alt then
                        Q.Enqueue(p', alt)
                        dist.[p'] <- alt
//                        prev.[p'] <- p

//    let mutable trace = target
//    while prev.ContainsKey(trace) do
//        printfn "%A" trace
//        printfn "Price %d" dist.[trace]
//        trace <- prev.[trace] 
        
    dist.[target]

let task1 = dijkstra 2 

let task2 = expandToPart2 >> dijkstra 4

let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let testInput =
    "\
#############
#...........#
###B#C#B#D###
  #A#D#C#A#
  #########"

let testAnswer1 = 12521
let testAnswer2 = 44169
let result1 = testInput |> fullTask1
assert (result1 = testAnswer1)
let result2 = testInput |> fullTask2
assert (result2 = testAnswer2)

let realInput = taskInput 2021 23
let realResult1 = realInput |> fullTask1
assert (realResult1 = 10411)
printfn $"Day 23.1: {realResult1}"
printfn $"Day 23.2: {realInput |> fullTask2}"
