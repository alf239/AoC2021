open System
open System.Collections.Generic
open AoC.Magic

let realInput = taskInput 2021 12

let testInput =
    "fs-end\n\
he-DX\n\
fs-he\n\
start-DX\n\
pj-DX\n\
end-zg\n\
zg-sl\n\
zg-pj\n\
pj-he\n\
RW-he\n\
fs-DX\n\
pj-RW\n\
zg-RW\n\
start-pj\n\
he-WI\n\
zg-he\n\
pj-fs\n\
start-RW"

let testAnswer1 = 226
let testAnswer2 = 3509

let parse =
    nonEmptyLines
    >> Seq.map
        (fun (s: string) ->
            let [| f; t |] = s.Split('-')
            f, t)
    >> (fun data ->
        data
        |> Seq.collect (fun (f, t) -> [ f, t; t, f ])
        |> Seq.filter (fun (f, t) -> f <> "end" && t <> "start")
        |> Seq.groupBy fst
        |> Seq.map (fun (k, xs) -> k, xs |> Seq.map snd)
        |> Seq.map (fun (k, xs) -> k, xs |> Set.ofSeq)
        |> Map.ofSeq)

let task (map: Map<string, string Set>) (repeat: int) =
    let paths = HashSet()
    let considered = HashSet()
    let work = Queue()
    work.Enqueue(([ "start" ], repeat))

    while work.Count > 0 do
        let path, budget = work.Dequeue()
        let current = path |> List.head

        if current <> "end" then
            for next in map.Item(current) do
                let isSmall = Seq.forall Char.IsLower

                let penalty =
                    if isSmall next && (List.contains next path) then
                        1
                    else
                        0

                let newBudget = budget - penalty

                if newBudget >= 0 then
                    let newPath = next :: path, newBudget

                    if considered.Add newPath then
                        work.Enqueue newPath
        else
            paths.Add(path) |> ignore

    paths.Count

let task1 (map: Map<string, string Set>) = task map 0

let task2 (map: Map<string, string Set>) = task map 1

let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let result1 = testInput |> fullTask1
assert (result1 = testAnswer1)
let result2 = testInput |> fullTask2
assert (result2 = testAnswer2)

printfn $"Day 12.1: {realInput |> fullTask1}"
printfn $"Day 12.2: {realInput |> fullTask2}"
