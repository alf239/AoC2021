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

let stepBack pos d = (pos + 10 - d) % 10 

let countWays (dp: int64 [] [] [] []) ap bp asc bsc =
    let prevAsc = asc - ap - 1
    let prevBsc = bsc - bp - 1

    if prevAsc < 0
       || prevBsc < 0
       || prevAsc > 20
       || prevBsc > 20 then
        0L
    else
        Seq.allPairs stats stats
        |> Seq.map
            (fun ((ad, ac), (bd, bc)) ->
                let prevAp = ap |> stepBack ad
                let prevBp = bp |> stepBack bd
                let count = int64 <| ac * bc

                dp.[prevAp].[prevBp].[prevAsc].[prevBsc] * count)
        |> Seq.sum

let countFirstWins (dp: int64 [] [] [] []) =
    seq {
        for ap in 0 .. 9 do
            for asc in 21 .. 29 do
                let prevAsc = asc - ap - 1
                if prevAsc >= 0 && prevAsc < 21 then
                    for ad, ac in stats do
                        let prevAp = ap |> stepBack ad
                        let count = int64 <| ac
                        for prevBp in 0 .. 9 do
                            for prevBsc in 0 .. 20 do
                                yield dp.[prevAp].[prevBp].[prevAsc].[prevBsc] * count
    }
    |> Seq.sum

let task2 (a, b) =
    let maxSteps = 21
    let dp =
        [| for _step in 0 .. maxSteps ->
               [| for _aPos in 0 .. 9 ->
                      [| for _bPos in 0 .. 9 -> [| for _aScore in 0 .. 20 -> [| for _bScore in 0 .. 20 -> 0L |] |] |] |] |]

    dp.[0].[a - 1].[b - 1].[0].[0] <- 1L

    let mutable aWon = 0L
    let mutable bWon = 0L

    for step in 1 .. maxSteps do
        aWon <- aWon + countFirstWins dp.[step - 1]

        for ap in 0 .. 9 do
            for bp in 0 .. 9 do
                for asc in 1 .. 20 do
                    for bsc in 1 .. 20 do
                        let ways = countWays dp.[step - 1] ap bp asc bsc
                        dp.[step].[ap].[bp].[asc].[bsc] <- ways

                    for bsc in 21 .. 29 do
                        let ways = countWays dp.[step - 1] ap bp asc bsc
                        bWon <- bWon + ways

    max aWon bWon

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
