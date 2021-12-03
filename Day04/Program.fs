open AoC.Magic
open System

let forReal = true
let rawInput = if forReal then taskInput 2021 4 else ""
let input = rawInput |> asStrings |> Seq.toList

let task1 (input: string seq) = -1

let task2 (input: string seq) = -2


printfn $"Day 4.1: {task1 input}"
printfn $"Day 4.2: {task2 input}"
