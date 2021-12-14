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

let testAnswer1 = 1588L
let testAnswer2 = 2188189693529L

let parse s =
    let ls = nonEmptyLines s
    let poly = ls |> Seq.head

    let preparedPoly =
        poly |> Seq.last,
        poly
        |> Seq.windowed 2
        |> Seq.countBy id
        |> Seq.map (fun (k, cnt) -> k, int64 cnt)

    let rules =
        ls
        |> Seq.tail
        |> Seq.map
            (fun s ->
                let [| pair; _; insert |] = words s
                let [| c1; c2 |] = pair |> Array.ofSeq
                let insert = insert |> Seq.head
                [| c1; c2 |], [ [| c1; insert |]; [| insert; c2 |] ])
        |> Map.ofSeq

    preparedPoly, rules

let step (poly: char * seq<char [] * int64>) (rules: Map<char [], #seq<char []>>) =
    let last, pairs = poly

    let newPairs =
        pairs
        |> Seq.collect
            (fun (pair, cnt) ->
                rules
                |> Map.find pair
                |> Seq.map (fun k -> k, int64 cnt))
        |> Seq.groupBy fst
        |> Seq.map (fun (k, cnts) -> k, cnts |> Seq.map snd |> Seq.sum)

    last, newPairs

let task (poly, rules) n =
    let last, pairs =
        [ 1 .. n ]
        |> Seq.fold (fun acc _ -> step acc rules) poly

    let frequencies =
        pairs
        |> Seq.map (fun (k, cnt) -> k |> Seq.head, cnt)
        |> Seq.append [ (last, 1L) ]
        |> Seq.groupBy fst
        |> Seq.map (fun (k, cnts) -> k, cnts |> Seq.map snd |> Seq.sum)

    let mx = frequencies |> Seq.maxBy snd |> snd
    let mn = frequencies |> Seq.minBy snd |> snd
    mx - mn

let task1 data = task data 10

let task2 data = task data 40

let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let result1 = testInput |> fullTask1
assert (result1 = testAnswer1)
let result2 = testInput |> fullTask2
assert (result2 = testAnswer2)

printfn $"Day 14.1: {realInput |> fullTask1}"
printfn $"Day 14.2: {realInput |> fullTask2}"
