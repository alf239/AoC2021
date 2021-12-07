open AoC.Magic

let realInput = taskInput 2021 7
let testInput = "16,1,2,0,4,2,7,1,2,14"
let testAnswer1 = 37
let testAnswer2 = 168

let parse = csInts

let cost1 (x: int) (y: int) : int = abs (x - y)

let cost2 (x: int) (y: int) : int =
    let l = abs (x - y)
    l * (l + 1) / 2

let task (fuel: int -> int -> int) (data: int []) : int =
    let mn = data |> Array.min
    let mx = data |> Array.max

    let sums =
        seq { for i in mn .. mx -> data |> Array.toSeq |> Seq.map (fuel i) |> Seq.sum }
        |> Seq.toArray

    sums |> Array.min

let task1 data = task cost1 data

let task2 data = task cost2 data

let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let result1 = testInput |> fullTask1
assert (result1 = testAnswer1)
let result2 = testInput |> fullTask2
assert (result2 = testAnswer2)

printfn $"Day 7.1: {realInput |> fullTask1}"
printfn $"Day 7.2: {realInput |> fullTask2}"
