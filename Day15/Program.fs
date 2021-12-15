open AoC.Magic

let realInput = taskInput 2021 15

let testInput =
    "\
1163751742
1381373672
2136511328
3694931569
7463417111
1319128137
1359912421
3125421639
1293138521
2311944581"

let testAnswer1 = 40
let testAnswer2 = -2

let charToint (s: string) =
    s.ToCharArray()
    |> Array.map (fun c -> int c - int '0')

let parse =
    nonEmptyLines >> Seq.map charToint >> Array.ofSeq

let format data =
    data
    |> Seq.map (Array.map <| sprintf "%4d" >> String.concat " ")
    |> String.concat "\n"

let task1 data =
    let H = data |> Array.length
    let W = data.[0] |> Array.length

    let dp =
        [| for _y in 0 .. H - 1 -> [| for _x in 0 .. W - 1 -> 0 |] |]

    for x in 1 .. W - 1 do
        dp.[0].[x] <- dp.[0].[x - 1] + data.[0].[x]

    for y in 1 .. H - 1 do
        dp.[y].[0] <- dp.[y - 1].[0] + data.[y].[0]

    for y in 1 .. H - 1 do
        for x in 1 .. W - 1 do
            dp.[y].[x] <- (min dp.[y - 1].[x] dp.[y].[x - 1]) + data.[y].[x]

//    printf "%s" <| format data
//    printf "\n\n"
//    printf "%s" <| format dp

    dp.[H - 1].[W - 1]

let task2 data = -2

let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let result1 = testInput |> fullTask1
assert (result1 = testAnswer1)
let result2 = testInput |> fullTask2
assert (result2 = testAnswer2)

printfn $"Day 15.1: {realInput |> fullTask1}"
printfn $"Day 15.2: {realInput |> fullTask2}"
