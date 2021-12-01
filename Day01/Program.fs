open System.IO

[<EntryPoint>]
let main args =
    let input = File.ReadLines args.[0] |> Seq.map int

    let increases =
        Seq.windowed 2
        >> Seq.filter (fun a -> a.[1] > a.[0])
        >> Seq.length

    printfn $"Day 1.1: {increases input}"

    let dx =
        input
        |> Seq.windowed 3
        |> Seq.map Array.sum

    printfn $"Day 1.2: {increases dx}"

    0
