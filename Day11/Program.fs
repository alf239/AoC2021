open System.Collections.Generic
open AoC.Magic

let realInput = taskInput 2021 11

let testInput =
    "5483143223\n\
2745854711\n\
5264556173\n\
6141336146\n\
6357385478\n\
4167524645\n\
2176841721\n\
6882881134\n\
4846848554\n\
5283751526"

let testAnswer1 = 1656

let testAnswer2 = 195

let ints (s: string) =
    s.ToCharArray()
    |> Array.map (fun c -> (int c) - (int '0'))

let parse =
    nonEmptyLines >> Seq.map ints >> Array.ofSeq

let step data i =
    let N = Array.length data
    let M = Array.length data.[0]
    let flashed = HashSet()
    let work = Queue()
    
    for y in 0 .. N - 1 do
        for x in 0 .. M - 1 do
            data.[y].[x] <- data.[y].[x] + 1

            if data.[y].[x] > 9 then
                work.Enqueue((x, y))

    while work.Count > 0 do
        let x, y = work.Dequeue()

        if not <| flashed.Contains((x, y)) then
            if data.[y].[x] > 9 then
                flashed.Add((x, y)) |> ignore
                for x1, y1 in
                    [ x - 1, y - 1
                      x - 1, y
                      x - 1, y + 1
                      x, y - 1
                      x, y + 1
                      x + 1, y - 1
                      x + 1, y
                      x + 1, y + 1 ] do
                    if x1 >= 0 && y1 >= 0 && x1 < M && y1 < N then
                        data.[y1].[x1] <- data.[y1].[x1] + 1
                        work.Enqueue((x1, y1))

    for x, y in flashed do
        data.[y].[x] <- 0

    flashed.Count

let task1 data =
    seq { 1 .. 100 }
    |> Seq.map (step data)
    |> Seq.sum

let task2 data =
    let N = Array.length data
    let M = Array.length data.[0]
    let size = N * M
    
    Seq.unfold (fun x -> Some (x, x + 1)) 1
    |> Seq.map (fun i -> (step data i), i )
    |> Seq.find (fun (x, _) -> x = size)
    |> snd

let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let result1 = testInput |> fullTask1
assert (result1 = testAnswer1)
let result2 = testInput |> fullTask2
assert (result2 = testAnswer2)

printfn $"Day 11.1: {realInput |> fullTask1}"
printfn $"Day 11.2: {realInput |> fullTask2}"
