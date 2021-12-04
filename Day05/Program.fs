open AoC.Magic

let forReal = true
let rawInput = if forReal then taskInput 2021 5 else ""
let input = rawInput |> nonEmptyLines |> Seq.toList

let task1 ss = -1
let task2 ss = -2

printfn $"Day 5.1: {task1 input}"
printfn $"Day 5.2: {task2 input}"
