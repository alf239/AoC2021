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

type State =
    { APos: int
      BPos: int
      AScore: int
      BScore: int }

let aWon s = s.AScore >= 21
let bWon s = s.BScore >= 21

let stepA ss =
    ss
    |> Seq.collect
        (fun (s, count) ->
            seq {
                for d, cnt in stats ->
                    let p = (s.APos + d) % 10
                    let score = s.AScore + p + 1
                    { s with APos = p; AScore = score }, (int64 cnt) * count
            })
    |> Seq.groupBy fst
    |> Seq.map (fun (s, ss) -> s, ss |> Seq.map snd |> Seq.sum)
    |> List.ofSeq

let stepB ss =
    ss
    |> Seq.collect
        (fun (s, count) ->
            seq {
                for d, cnt in stats ->
                    let p = (s.BPos + d) % 10
                    let score = s.BScore + p + 1
                    { s with BPos = p; BScore = score }, (int64 cnt) * count
            })
    |> Seq.groupBy fst
    |> Seq.map (fun (s, ss) -> s, ss |> Seq.map snd |> Seq.sum)
    |> List.ofSeq

let task2 (a, b) =
    let mutable ss =
        [ { APos = a - 1
            BPos = b - 1
            AScore = 0
            BScore = 0 },
          1L ]

    let mutable aWins = 0L
    let mutable bWins = 0L

    let count (fn: State -> bool) (ss: (State * int64) list) =
        ss
        |> Seq.filter (fst >> fn)
        |> Seq.map snd
        |> Seq.sum

    while ss |> Seq.isEmpty |> not do
        ss <- stepA ss
        aWins <- aWins + count aWon ss
        ss <- ss |> List.filter (fst >> aWon >> not)

        ss <- stepB ss
        bWins <- bWins + count bWon ss
        ss <- ss |> List.filter (fst >> bWon >> not)

    max aWins bWins

let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let testInput =
    "Player 1 starting position: 4\n\Player 2 starting position: 8"

let testAnswer1 = 739785L
let testAnswer2 = 444_356_092_776_315L
let result1 = testInput |> fullTask1
assert (result1 = testAnswer1)
let result2 = testInput |> fullTask2
assert (result2 = testAnswer2)

let realInput = taskInput 2021 21
let realAnswer1 = realInput |> fullTask1
assert (realAnswer1 = 742257L)
printfn $"Day 21.1: {realAnswer1}"
printfn $"Day 21.2: {realInput |> fullTask2}"
