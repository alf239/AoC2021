open AoC.Magic

let forReal = true

let rawInput =
    if forReal then
        taskInput 2021 5
    else
        "0,9 -> 5,9\n\
8,0 -> 0,8\n\
9,4 -> 3,4\n\
2,2 -> 2,1\n\
7,0 -> 7,4\n\
6,4 -> 2,0\n\
0,9 -> 2,9\n\
3,4 -> 1,4\n\
0,0 -> 8,8\n\
5,5 -> 8,2"

let input = rawInput |> nonEmptyLines |> Seq.toList

let parse s =
    let [| f; _; t |] = words s
    let [| x1; y1 |] = csInts f
    let [| x2; y2 |] = csInts t
    (x1, y1), (x2, y2)

let ls = input |> Seq.map parse

let task1 ls =
    let horVer =
        ls
        |> Seq.filter (fun ((x1, y1), (x2, y2)) -> x1 = x2 || y1 = y2)

    let maxX =
        horVer
        |> Seq.collect (fun ((x1, y1), (x2, y2)) -> [ x1; x2 ])
        |> Seq.max

    let maxY =
        horVer
        |> Seq.collect (fun ((x1, y1), (x2, y2)) -> [ y1; y2 ])
        |> Seq.max

    let map =
        [| for x in 0 .. maxX -> [| for y in 0 .. maxY -> 0 |] |]

    for (x1, y1), (x2, y2) in horVer do
        if x1 = x2 then
            for y in (min y1 y2) .. (max y1 y2) do
                map.[x1].[y] <- map.[x1].[y] + 1
        else
            for x in (min x1 x2) .. (max x1 x2) do
                map.[x].[y1] <- map.[x].[y1] + 1

    map
    |> Seq.ofArray
    |> Seq.collect Seq.ofArray
    |> Seq.filter (fun c -> c > 1)
    |> Seq.length

let task2 ss = -2

printfn $"Day 5.1: {task1 ls}"
printfn $"Day 5.2: {task2 input}"
