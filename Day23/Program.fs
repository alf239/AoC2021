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

type Position =
    { HallwayL2: Amphipod option
      HallwayL1: Amphipod option
      HallwayAB: Amphipod option
      HallwayBC: Amphipod option
      HallwayCD: Amphipod option
      HallwayR1: Amphipod option
      HallwayR2: Amphipod option
      RoomA: Amphipod list
      RoomB: Amphipod list
      RoomC: Amphipod list
      RoomD: Amphipod list }

let hasOnly xx = List.forall (fun x -> x = xx)

let solved n =
    let fullRoom x = List.init n (fun _ -> x)

    { HallwayL1 = None
      HallwayL2 = None
      HallwayAB = None
      HallwayBC = None
      HallwayCD = None
      HallwayR1 = None
      HallwayR2 = None
      RoomA = fullRoom A
      RoomB = fullRoom B
      RoomC = fullRoom C
      RoomD = fullRoom D }

let moves n p = [ p ]

let parse s =
    let [ one; two ] =
        nonEmptyLines s
        |> Seq.skip 3
        |> Seq.take 2
        |> List.ofSeq

    let [| _; a1; b1; c1; d1; _ |] = one.Split('#')
    let [| _; a2; b2; c2; d2; _ |] = two.Split('#')

    { HallwayL2 = None
      HallwayL1 = None
      HallwayAB = None
      HallwayBC = None
      HallwayCD = None
      HallwayR1 = None
      HallwayR2 = None
      RoomA = [ amphipod a1; amphipod a2 ]
      RoomB = [ amphipod b1; amphipod b2 ]
      RoomC = [ amphipod c1; amphipod c2 ]
      RoomD = [ amphipod d1; amphipod d2 ] }

let expandToPart2 pos =
    let expandRoom a2 a3 room =
        (List.head room) :: a2 :: a3 :: (List.tail room)

    { pos with
          RoomA = pos.RoomA |> expandRoom D D
          RoomB = pos.RoomB |> expandRoom C B
          RoomC = pos.RoomC |> expandRoom B A
          RoomD = pos.RoomD |> expandRoom A C }

let dijkstra n pos =
    let target = solved n
    let seen = HashSet()
    let Q = PriorityQueue()
    let dist = Dictionary()

    dist.Add(pos, 0)
    Q.Enqueue(pos, 0)
    seen.Add(target) |> ignore // to avoid going further from target

    while Q.Count > 0 do
        let p = Q.Dequeue()
        if p = target then Q.Clear() // improvised break

        if not <| seen.Contains(p) then
            seen.Add(p) |> ignore
            let d = dist.Item(p)

            for p', d' in moves n p do
                if not <| seen.Contains(p') then
                    let alt = d + d'
                    let found, prev = dist.TryGetValue(p')

                    if not found || prev > alt then
                        Q.Enqueue(p', alt)
                        dist.Add(p', alt)

    dist.Item(target)

let task1 data = -1

let task2 data = -2

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
let testAnswer2 = -2
let result1 = testInput |> fullTask1
assert (result1 = testAnswer1)
let result2 = testInput |> fullTask2
assert (result2 = testAnswer2)

let realInput = taskInput 2021 23
let realResult1 = realInput |> fullTask1
assert (realResult1 = 10411)
printfn $"Day 23.1: {realResult1}"
printfn $"Day 23.2: {realInput |> fullTask2}"
