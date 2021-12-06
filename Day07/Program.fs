open AoC.Magic

let realInput = taskInput 2021 7
let testInput = "3,4,3,1,2"
let testAnswer1 = -1
let testAnswer2 = -2

let parse = nonEmptyLines >> Seq.toList

let task1 data = -1

let task2 data = -2

let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

assert (testInput |> fullTask1 = testAnswer1)
assert (testInput |> fullTask2 = testAnswer2)

printfn $"Day 7.1: {realInput |> fullTask1}"
printfn $"Day 7.2: {realInput |> fullTask2}"
