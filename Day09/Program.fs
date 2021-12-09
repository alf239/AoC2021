open System.Collections.Generic
open AoC.Magic

let realInput = taskInput 2021 9

let testInput =
    "2199943210\n\
3987894921\n\
9856789892\n\
8767896789\n\
9899965678"

let testAnswer1 = 15
let testAnswer2 = 1134

let parse =
    nonEmptyLines
    >> Seq.map (fun (s: string) -> s.ToCharArray())
    >> Seq.map (Array.map int)
    >> Seq.map (Array.map (fun x -> x - (int '0')))
    >> Array.ofSeq

let isLowest (data: int [] []) x y =
    let u = if y = 0 then 10 else data.[x].[y - 1]

    let d =
        if y = Array.length data.[0] - 1 then
            10
        else
            data.[x].[y + 1]

    let r =
        if x = Array.length data - 1 then
            10
        else
            data.[x + 1].[y]

    let l = if x = 0 then 10 else data.[x - 1].[y]
    let v = data.[x].[y]

    let lowest = v < u && v < d && v < r && v < l
    lowest

let task data fn =
    seq {
        for y in 0 .. Array.length (Array.head data) - 1 do
            for x in 0 .. Array.length data - 1 do
                if isLowest data x y then yield fn x y
    }

let task1 data =
    task data (fun x y -> data.[x].[y] + 1) |> Seq.sum

let task2 data =
    let border x y =
        (x = -1)
        || (y = -1)
        || (x = Array.length data)
        || (y = Array.length data.[0])
        || (data.[x].[y] = 9)

    let basinSize x y =
        let mutable cnt = 0

        bfs
            (fun (a, b) ->
                if not <| border a b then
                    cnt <- cnt + 1

                    seq {
                        (a - 1, b)
                        (a, b - 1)
                        (a + 1, b)
                        (a, b + 1)
                    }
                else
                    Seq.empty)
            (x, y)

        cnt

    let sizes = task data basinSize

    sizes
    |> Seq.sort
    |> Seq.rev
    |> Seq.take 3
    |> Seq.fold (fun acc x -> acc * x) 1

let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let result1 = testInput |> fullTask1
assert (result1 = testAnswer1)
let result2 = testInput |> fullTask2
assert (result2 = testAnswer2)

printfn $"Day 9.1: {realInput |> fullTask1}"
printfn $"Day 9.2: {realInput |> fullTask2}"
