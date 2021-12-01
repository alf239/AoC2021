open AoC.Magic

[<EntryPoint>]
let main _ =

    let input = taskInput 2021 1 |> asSingleInts

    let increases =
        Seq.windowed 2
        >> Seq.filter (fun a -> a.[1] > a.[0])
        >> Seq.length

    printfn $"Day 1.1: {increases input}"

    let slidingSum =
        input |> Seq.windowed 3 |> Seq.map Array.sum

    printfn $"Day 1.2: {increases slidingSum}"

    0
