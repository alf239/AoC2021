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

type Cave =
    | Large of string
    | Small of string
    | Start
    | End

let cave =
    function
    | "start" -> Start
    | "end" -> End
    | s when String.isLowercase s -> Small s
    | s -> Large s

let isSmall =
    function
    | Small _ -> true
    | _ -> false

let parse =
    nonEmptyLines
    >> Seq.collect
        (fun (s: string) ->
            let [| f; t |] = s.Split('-') |> Array.map cave
            [ f, t; t, f ])
    >> Seq.filter (fun (f, t) -> f <> End && t <> Start)
    >> Seq.groupBy fst
    >> Seq.map (fun (f, fts) -> f, fts |> Seq.map snd |> Set.ofSeq)
    >> Map.ofSeq

let task (map: Map<Cave, Cave Set>) (repeat: int) =
    let paths = HashSet()
    let considered = HashSet()
    let work = Queue()
    work.Enqueue(([ Start ], repeat))

    while work.Count > 0 do
        let path, budget = work.Dequeue()
        let current = path |> List.head

        if current <> End then
            for next in map.Item(current) do
                let penalty =
                    (isSmall next && (List.contains next path))
                    |> boolToInt

                let newBudget = budget - penalty

                if newBudget >= 0 then
                    let newPath = next :: path, newBudget

                    if considered.Add newPath then
                        work.Enqueue newPath
        else
            paths.Add(path) |> ignore

    paths.Count

let task1 map = task map 0

let task2 map = task map 1

let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let result1 = testInput |> fullTask1
assert (result1 = testAnswer1)
let result2 = testInput |> fullTask2
assert (result2 = testAnswer2)

printfn $"Day 12.1: {realInput |> fullTask1}"
printfn $"Day 12.2: {realInput |> fullTask2}"
