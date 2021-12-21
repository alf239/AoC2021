open AoC.Magic

let parse =
    nonEmptyLines
    >> Seq.map (words >> (fun ws -> int ws.[4]))
    >> List.ofSeq
    >> (fun [ a; b ] -> a, b)

let task1 (a, b) =
    let mutable a = a - 1
    let mutable b = b - 1
    let mutable rolled = 0
    let mutable next = 0
    let mutable scoreA = 0
    let mutable scoreB = 0

    let roll () =
        rolled <- rolled + 1
        let result = next
        next <- (next + 1) % 10
        result + 1

    let fullRoll () = roll () + roll () + roll ()

    while scoreA < 1000 && scoreB < 1000 do
        let rolledA = fullRoll ()
        a <- (a + rolledA) % 10
        scoreA <- scoreA + a + 1

        if scoreA < 1000 then
            let rolledB = fullRoll ()
            b <- (b + rolledB) % 10
            scoreB <- scoreB + b + 1

    rolled * (min scoreA scoreB) |> int64


let task2 data = -2L

let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let testInput =
    "Player 1 starting position: 4\n\Player 2 starting position: 8"

let testAnswer1 = 739785L
let testAnswer2 = -2L
let result1 = testInput |> fullTask1
assert (result1 = testAnswer1)
let result2 = testInput |> fullTask2
assert (result2 = testAnswer2)

let realInput = taskInput 2021 21
let realAnswer1 = realInput |> fullTask1
assert (realAnswer1 = 742257L)
printfn $"Day 21.1: {realAnswer1}"
printfn $"Day 21.2: {realInput |> fullTask2}"
