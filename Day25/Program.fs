open AoC.Magic

let parse =
    nonEmptyLines
    >> Seq.map Array.ofSeq
    >> Array.ofSeq

let task1 m =
    let H = Array.length m
    let W = Array.length m.[0]
    let mutable i = 0
    let mutable moves = true
    
    let finalizeMove () = 
        for y in 0 .. H - 1 do
            for x in 0 .. W - 1 do
                match m.[y].[x] with
                | 'R' -> m.[y].[x] <- '>'
                | 'D' -> m.[y].[x] <- 'v'
                | 'E' -> m.[y].[x] <- '.'
                | _ -> ()

    while moves do
        moves <- false

        for y in 0 .. H - 1 do
            for x in 0 .. W - 1 do
                if m.[y].[x] = '>' && m.[y].[(x + 1) % W] = '.' then
                    moves <- true
                    m.[y].[x] <- 'E'
                    m.[y].[(x + 1) % W] <- 'R'

        if moves then
            finalizeMove ()

        for y in 0 .. H - 1 do
            for x in 0 .. W - 1 do
                if m.[y].[x] =  'v' && m.[(y + 1) % H].[x] = '.' then
                    moves <- true
                    m.[y].[x] <- 'E'
                    m.[(y + 1) % H].[x] <- 'D'

        if moves then
            finalizeMove ()

        i <- i + 1

    i

let task2 data = -2

let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let testInput =
    "\
v...>>.vv>
.vv>>.vv..
>>.>v>...v
>>v>>.>.v.
v>v.vv.v..
>.>>..v...
.vv..>.>v.
v.v..>>v.v
....v..v.>"

let testAnswer1 = 58
let testAnswer2 = -2
let result1 = testInput |> fullTask1
assert (result1 = testAnswer1)
let result2 = testInput |> fullTask2
assert (result2 = testAnswer2)

let realInput = taskInput 2021 25
printfn $"Day 25.1: {realInput |> fullTask1}"
printfn $"Day 25.2: {realInput |> fullTask2}"
