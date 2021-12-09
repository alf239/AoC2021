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
        if y = Array.length (Array.head data) - 1 then
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
                if isLowest data x y then
                    yield fn data x y
    }

let dfs (data: int [] []) x y =
    let seen = HashSet()
    let work = Queue()
    work.Enqueue(x, y)
    let mutable cnt = 0

    while work.Count > 0 do
        let a, b = work.Dequeue()

        if not <| seen.Contains((a, b)) then
            seen.Add((a, b)) |> ignore

            let border =
                (a = -1)
                || (b = -1)
                || (a = Array.length data)
                || (b = Array.length data.[0])
                || (data.[a].[b] = 9)

            if not border then
                cnt <- cnt + 1

                work.Enqueue((a - 1, b))
                work.Enqueue((a, b - 1))
                work.Enqueue((a + 1, b))
                work.Enqueue((a, b + 1))

    cnt


let task1 data = task data (fun d x y -> d.[x].[y] + 1) |> Seq.sum

let task2 data = 
    let sizes = task data dfs
    sizes |> Seq.sort |> Seq.rev |> Seq.take 3 |> Seq.fold (fun acc x -> acc * x) 1

let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let result1 = testInput |> fullTask1
assert (result1 = testAnswer1)
let result2 = testInput |> fullTask2
assert (result2 = testAnswer2)

printfn $"Day 9.1: {realInput |> fullTask1}"
printfn $"Day 9.2: {realInput |> fullTask2}"
