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

let task1 (map: Map<string, string Set>) =
    let paths = HashSet()
    let work = Queue()
    work.Enqueue(([ "start" ], []))

    while work.Count > 0 do
        let path, seen = work.Dequeue()
        let current = path |> List.head

        if current = "end" then
            paths.Add(path) |> ignore
        else
            for next in map.Item(current) do
                if seen |> List.contains next |> not then
                    let newSeen =
                        if current = current.ToLower() then
                            current :: seen
                        else
                            seen

                    work.Enqueue((next :: path, newSeen))

    paths.Count

let task2 (map: Map<string, string Set>) =
    let paths = HashSet()
    let considered = HashSet()
    let work = Queue()
    work.Enqueue(([ "start" ], 1))

    while work.Count > 0 do
        let path, budget = work.Dequeue()
        let current = path |> List.head

        if current = "end" then
            paths.Add(path) |> ignore
        else
            for next in map.Item(current) do
                let isSmall =
                    next.ToCharArray() |> Array.forall Char.IsLower

                let penalty = if isSmall && (List.contains next path) then 1 else 0
                let newBudget = budget - penalty

                let newPath = next :: path, newBudget

                if considered.Add(newPath) then
                    if newBudget >= 0 then
                        work.Enqueue(newPath)


    paths.Count

let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let result1 = testInput |> fullTask1
assert (result1 = testAnswer1)
let result2 = testInput |> fullTask2
assert (result2 = testAnswer2)

printfn $"Day 12.1: {realInput |> fullTask1}"
printfn $"Day 12.2: {realInput |> fullTask2}"
