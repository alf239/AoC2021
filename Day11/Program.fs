open AoC.Magic

let realInput = taskInput 2021 10

let testInput = ""

let testAnswer1 = -1

let testAnswer2 = -2

let parse = nonEmptyLines

let task1 _ = -1

let task2 _ = -2

let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let result1 = testInput |> fullTask1
assert (result1 = testAnswer1)
let result2 = testInput |> fullTask2
assert (result2 = testAnswer2)

printfn $"Day 11.1: {realInput |> fullTask1}"
printfn $"Day 11.2: {realInput |> fullTask2}"
