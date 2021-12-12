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
let testAnswer2 = -2

let parse =
    nonEmptyLines
    >> Seq.map
        (fun (s: string) ->
            let [| f; t |] = s.Split('-')
            f, t)

let task1 data =
    let map =
        data
        |> Seq.collect (fun (f, t) -> [ f, t; t, f ])
        |> Seq.groupBy fst
        |> Seq.map (fun (k, xs) -> k, xs |> Seq.map snd)
        |> Map.ofSeq

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
                        else seen

                    work.Enqueue((next :: path, newSeen))

    paths.Count

let task2 data = -2

let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let result1 = testInput |> fullTask1
assert (result1 = testAnswer1)
let result2 = testInput |> fullTask2
assert (result2 = testAnswer2)

printfn $"Day 12.1: {realInput |> fullTask1}"
printfn $"Day 12.2: {realInput |> fullTask2}"
