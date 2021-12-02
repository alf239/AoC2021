open AoC.Magic

let input = taskInput 2021 2 |> asStrings

let commands =
    input
    |> Seq.map
        (fun s ->
            let parts = s.Split(' ')
            parts.[0], int parts.[1])

let x, y =
    commands
    |> Seq.fold
        (fun (x, y) cmd ->
            match cmd with
            | "forward", n -> x + n, y
            | "down", n -> x, y + n
            | "up", n -> x, y - n) (0, 0)

printfn $"Day 2.1: {x * y}"

let x2, y2, aim =
    commands
    |> Seq.fold
        (fun (x, y, aim) cmd ->
            match cmd with
            | "forward", n -> x + n, y + aim * n, aim
            | "down", n -> x, y, aim + n
            | "up", n -> x, y, aim - n) (0, 0, 0)

printfn $"Day 2.2: {x2 * y2}"
