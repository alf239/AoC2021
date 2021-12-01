open System.IO

[<EntryPoint>]
let main args =
    let input = File.ReadLines args.[0] |> Seq.map int 

    let dx = input |> Seq.windowed 2
    let increases = dx |> Seq.filter (fun a -> a.[1] > a.[0]) |> Seq.toList
    printfn $"Hello from day 1.1: {List.length increases}"

    let dx = input |> Seq.windowed 3 |> Seq.map Array.sum |> Seq.toList
    let increases = dx |> Seq.windowed 2 |> Seq.filter (fun a -> a.[1] > a.[0]) |> Seq.toList
    printfn $"Hello from day 1.2: {List.length increases}"
    
    0