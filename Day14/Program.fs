open AoC.Magic

let realInput = taskInput 2021 14

let testInput =
    "NNCB\n\
\n\
CH -> B\n\
HH -> N\n\
CB -> H\n\
NH -> C\n\
HB -> C\n\
HC -> B\n\
HN -> C\n\
NN -> C\n\
BH -> H\n\
NC -> B\n\
NB -> B\n\
BN -> B\n\
BB -> N\n\
BC -> B\n\
CC -> N\n\
CN -> C"

let testAnswer1 = 1588
let testAnswer2 = -2

let parse s =
    let ls = nonEmptyLines s
    let poly = ls |> Seq.head |> Array.ofSeq

    let rules =
        ls
        |> Seq.tail
        |> Seq.map
            (fun s ->
                let [| pair; _; insert |] = words s
                pair |> Array.ofSeq, insert |> Seq.head)
        |> Map.ofSeq

    poly, rules

let step (poly: char seq) (rules: Map<char [], char>) =
    let last = poly |> Seq.last

    let newPoly =
        poly
        |> Seq.windowed 2
        |> Seq.collect
            (fun pair ->
                let insert = rules |> Map.find pair
                [ pair |> Seq.head; insert ])

    Seq.append newPoly [ last ]

let task1 (poly, rules) =
    let poly10 =
        [ 1 .. 10 ]
        |> Seq.fold (fun acc _ -> step acc rules) poly

    let frequencies = poly10 |> Seq.countBy id
    let mx = frequencies |> Seq.maxBy snd |> snd
    let mn = frequencies |> Seq.minBy snd |> snd
    mx - mn

let task2 data = -2

let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let result1 = testInput |> fullTask1
assert (result1 = testAnswer1)
let result2 = testInput |> fullTask2
assert (result2 = testAnswer2)

printfn $"Day 14.1: {realInput |> fullTask1}"
printfn $"Day 14.2: {realInput |> fullTask2}"
