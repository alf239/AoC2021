open AoC.Magic

let parse = nonEmptyLines

let task1 data = -1

let task2 data = -2

let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let testInput = ""
let testAnswer1 = -1
let testAnswer2 = -2
let result1 = testInput |> fullTask1
assert (result1 = testAnswer1)
let result2 = testInput |> fullTask2
assert (result2 = testAnswer2)

let realInput = taskInput 2021 17
printfn $"Day 17.1: {realInput |> fullTask1}"
printfn $"Day 17.2: {realInput |> fullTask2}"