open AoC.Magic

let forReal = true

let rawInput =
    if forReal then
        taskInput 2021 6
    else
        "3,4,3,1,2"

let input = rawInput |> nonEmptyLines |> Seq.toList


let task1 ls =
    let N = 2_000_000
    let ts = [| for _ in 1 .. N -> -1 |]
    let ages = ls |> Seq.head |> csInts
    let mutable n = ages |> Array.length

    for i, x in ages |> Array.indexed do
        ts.[i] <- x

    for day in 1 .. 80 do
        let watermark = n - 1

        for i in 0 .. watermark do
            match ts.[i] with
            | 0 ->
                ts.[i] <- 6
                ts.[n] <- 8
                n <- n + 1
            | x -> ts.[i] <- x - 1

    n

let task2 ls =
    let ts = [| for _ in 0 .. 8 -> 0L |]
    let t = [| for x in ts -> 0L |]
    let ages = ls |> Seq.head |> csInts

    for x in ages do
        ts.[x] <- ts.[x] + 1L

    for day in 1 .. 256 do
        for i in 0 .. 7 do
            t.[i] <- ts.[i + 1]

        t.[6] <- t.[6] + ts.[0]
        t.[8] <- ts.[0]

        for i in 0 .. 8 do
            ts.[i] <- t.[i]

    ts |> Array.sum


printfn $"Day 6.1: {task1 input}"
printfn $"Day 6.2: {task2 input}"
