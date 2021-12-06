open AoC.Magic

let forReal = true

let rawInput =
    if forReal then
        taskInput 2021 6
    else
        "3,4,3,1,2"

let input = rawInput |> nonEmptyLines |> Seq.toList

let task ages days =
    let ts = [| for _ in 0 .. 8 -> 0L |]
    let t = [| for _ in ts -> 0L |]

    for x in ages do
        ts.[x] <- ts.[x] + 1L

    for day in 1 .. days do
        for i in 0 .. 7 do
            t.[i] <- ts.[i + 1]

        t.[6] <- t.[6] + ts.[0]
        t.[8] <- ts.[0]

        for i in 0 .. 8 do
            ts.[i] <- t.[i]

    ts |> Array.sum

let ages = input |> Seq.head |> csInts

printfn $"Day 6.1: {task ages 80}"
printfn $"Day 6.2: {task ages 256}"
