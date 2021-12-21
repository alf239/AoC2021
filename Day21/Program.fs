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

let stats =
    let die = [ 1 .. 3 ]

    Seq.allPairs die (Seq.allPairs die die)
    |> Seq.countBy (fun (a, (b, c)) -> a + b + c)
    |> List.ofSeq

let task2 (a, b) =
    let dpA =
        [| for _step in 0 .. 9 -> [| for _pos in 0 .. 9 -> [| for _score in 0 .. 20 -> 0L |] |] |]

    let dpB =
        [| for _step in 0 .. 9 -> [| for _pos in 0 .. 9 -> [| for _score in 0 .. 20 -> 0L |] |] |]

    dpA.[0].[a - 1].[0] <- 1L
    dpB.[0].[b - 1].[0] <- 1L

    let collectOptions (dp: int64 [] [] []) step pos score =
        stats
        |> Seq.map
            (fun (dice, count) ->
                let prevScore = score - dice

                if prevScore > 20 || prevScore < 0 then
                    0L
                else
                    let prevPos = (pos + 10 - dice) % 10

                    dp.[step - 1].[prevPos].[prevScore]
                    * (int64 count))
        |> Seq.sum

    for step in [ 1 .. 9 ] do
        for pos in [ 0 .. 9 ] do
            for score in [ 0 .. 20 ] do
                dpA.[step].[pos].[score] <- collectOptions dpA step pos score
                dpB.[step].[pos].[score] <- collectOptions dpB step pos score

    let aWins =
        seq {
            for step in [ 3 .. 9 ] do
                for pos in [ 0 .. 9 ] do
                    for score in [ 21 .. 30 ] do
                        yield 
        } |> Seq.sum
    let bWins =
        seq {
            for step in [ 3 .. 9 ] do
                for pos in [ 0 .. 9 ] do
                    for score in [ 12 .. 20 ] do
        } |> Seq.sum
   
    max aWins bWins
let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let testInput =
    "Player 1 starting position: 4\n\Player 2 starting position: 8"

let testAnswer1 = 739785L
let testAnswer2 = 444356092776315L
let result1 = testInput |> fullTask1
assert (result1 = testAnswer1)
let result2 = testInput |> fullTask2
assert (result2 = testAnswer2)

let realInput = taskInput 2021 21
let realAnswer1 = realInput |> fullTask1
assert (realAnswer1 = 742257L)
printfn $"Day 21.1: {realAnswer1}"
printfn $"Day 21.2: {realInput |> fullTask2}"
