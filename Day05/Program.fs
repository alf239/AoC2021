open AoC.Magic

let forReal = true

let rawInput =
    if forReal then
        taskInput 2021 5
    else
        "0,9 -> 5,9\n\
8,0 -> 0,8\n\
9,4 -> 3,4\n\
2,2 -> 2,1\n\
7,0 -> 7,4\n\
6,4 -> 2,0\n\
0,9 -> 2,9\n\
3,4 -> 1,4\n\
0,0 -> 8,8\n\
5,5 -> 8,2"

let input = rawInput |> nonEmptyLines |> Seq.toList

let parse s =
    let [| f; _; t |] = words s
    let [| x1; y1 |] = csInts f
    let [| x2; y2 |] = csInts t
    (x1, y1), (x2, y2)

let ls = input |> Seq.map parse

let dots ((x1, y1), (x2, y2)) =
    let len = max (abs (x2 - x1)) (abs (y2 - y1))
    let dx = sign (x2 - x1)
    let dy = sign (y2 - y1)
    seq { for i in 0 .. len -> x1 + i * dx, y1 + i * dy }

let overlaps =
    Seq.collect dots
    >> Seq.groupBy id
    >> Seq.map (snd >> Seq.length) 
    >> Seq.filter (fun l -> l > 1)
    >> Seq.length

let task1 ls =
    let horVer =
        ls
        |> Seq.filter (fun ((x1, y1), (x2, y2)) -> x1 = x2 || y1 = y2)

    overlaps horVer

let task2 = overlaps

printfn $"Day 5.1: {task1 ls}"
printfn $"Day 5.2: {task2 ls}"
