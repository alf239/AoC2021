open System.Collections.Generic
open AoC.Magic
open FSharpx.Collections

let realInput = taskInput 2021 15

let testInput =
    "\
1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581"

let testAnswer1 = 40
let testAnswer2 = -2

let charToint (s: string) =
    s.ToCharArray()
    |> Array.map (fun c -> int c - int '0')

let parse =
    nonEmptyLines >> Seq.map charToint >> Array.ofSeq

let format data =
    data
    |> Seq.map (Array.map <| sprintf "%4d" >> String.concat " ")
    |> String.concat "\n"

let neighbours x y mxx mxy =
    [ x - 1 , y ; x , y - 1; x + 1, y; x, y + 1] |> Seq.filter (fun (x, y) -> x >= 0 && y >= 0 && x < mxx && y < mxy)

let task1 data =
    let H = data |> Array.length
    let W = data.[0] |> Array.length

    let seen = HashSet() 
    let Q = PriorityQueue()
    
    let dist = [| for _y in 0 .. H - 1 -> [| for _x in 0 .. W - 1 -> infinity |] |]
    let prev = [| for _y in 0 .. H - 1 -> [| for _x in 0 .. W - 1 -> (-1, -1) |] |]
    dist.[0].[0] <- 0
    for y in 0 .. H - 1 do
        for x in 0 .. W - 1 do
            Q.Enqueue((x, y), dist.[x].[y])

    while Q.Count > 0 do
        let x, y = Q.Dequeue()
        if not <| seen.Contains((x, y)) then
            seen.Add((x, y)) |> ignore
            let D = dist.[y].[x]
            for x1, y1 in neighbours x y W H do
                if not <| seen.Contains((x1, y1)) then
                    let alt = D + float data.[y1].[x1]
                    if alt < dist.[y1].[x1] then
                        Q.Enqueue((x1, y1), alt)
                        dist.[y1].[x1] <- alt
                        prev.[y1].[x1] <- (x, y)
    
    int <| dist.[H - 1].[W - 1]

let task2 data = -2

let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let result1 = testInput |> fullTask1
assert (result1 = testAnswer1)
let result2 = testInput |> fullTask2
assert (result2 = testAnswer2)

printfn $"Day 15.1: {realInput |> fullTask1}"
printfn $"Day 15.2: {realInput |> fullTask2}"
