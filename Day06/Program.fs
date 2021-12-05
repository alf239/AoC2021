open AoC.Magic

let forReal = true

let rawInput = if forReal then taskInput 2021 6 else ""

let input = rawInput |> nonEmptyLines |> Seq.toList


let task1 ls = -1

let task2 ls = -2

printfn $"Day 6.1: {task1 input}"
printfn $"Day 6.2: {task2 input}"
