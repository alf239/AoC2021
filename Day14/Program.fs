open AoC.Magic

let realInput = taskInput 2021 14

let testInput =
    "NNCB

CH -> B
HH -> N
CB -> H
NH -> C
HB -> C
HC -> B
HN -> C
NN -> C
BH -> H
NC -> B
NB -> B
BN -> B
BB -> N
BC -> B
CC -> N
CN -> C"

let testAnswer1 = 1588L
let testAnswer2 = 2188189693529L

let summarize<'a when 'a: equality> (data: ('a * int64) seq) =
    data
    |> Seq.groupBy fst
    |> Seq.map (fun (k, cnts) -> k, cnts |> Seq.map snd |> Seq.sum)

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

    let pairsStep =
        Seq.collect
            (fun (pair, cnt) ->
                rules
                |> Map.find pair
                |> Seq.map (fun k -> k, cnt))
        >> summarize

    let step (last, pairs) = last, pairsStep pairs

    preparedPoly, step

let task n (poly, step) =
    let last, pairs =
        [ 1 .. n ]
        |> Seq.fold (fun acc _ -> step acc) poly

    let frequencies =
        pairs
        |> Seq.map (fun (k, cnt) -> k |> Seq.head, cnt)
        |> Seq.append [ (last, 1L) ]
        |> summarize
        |> Seq.map snd

    let mx = frequencies |> Seq.max
    let mn = frequencies |> Seq.min
    mx - mn

let task1 = task 10

let task2 = task 40

let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let result1 = testInput |> fullTask1
assert (result1 = testAnswer1)
let result2 = testInput |> fullTask2
assert (result2 = testAnswer2)

printfn $"Day 14.1: {realInput |> fullTask1}"
printfn $"Day 14.2: {realInput |> fullTask2}"
