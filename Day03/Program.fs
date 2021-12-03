open AoC.Magic
open System

let forReal = true

let input =
    if forReal then
        taskInput 2021 3
    else
        "00100\n\
             11110\n\
             10110\n\
             10111\n\
             10101\n\
             01111\n\
             00111\n\
             11100\n\
             10000\n\
             11001\n\
             00010\n\
             01010"
    |> asStrings
    |> Seq.toList

let task1 (input: string seq) =
    let m = input |> Seq.head |> String.length
    let n = input |> Seq.length

    let ns = [| for _ in 0 .. m - 1 -> 0 |]

    for s in input do
        for i, c in s |> Seq.indexed do
            if c = '1' then ns.[i] <- ns.[i] + 1

    let cutoff = n / 2

    let bits =
        ns
        |> Array.map (fun cnt -> if cnt > cutoff then '1' else '0')
        |> String.Concat

    let x = Convert.ToInt32(bits, 2)

    let mask = (1 <<< m) - 1

    let y = x ^^^ mask
    x * y

let count p = Seq.filter p >> Seq.length

let bit m k x =
    let mask = 1 <<< (m - 1 - k)
    (x &&& mask) <> 0

let mcb m k xs =
    let n = Seq.length xs
    let c = xs |> count (bit m k)
    c * 2 >= n

let task2 (input: string seq) =
    let m = input |> Seq.head |> String.length

    let numbers =
        input
        |> Seq.map (fun s -> Convert.ToInt32(s, 2))
        |> Seq.toArray

    let filter k v xs =
        xs |> Array.filter (bit m k >> fun x -> x = v)

    let rec rating (b: bool) k xs =
        if Array.length xs = 1 then
            Array.head xs
        else
            let v = mcb m k xs = b
            let filtered = filter k v xs
            rating b (k + 1) filtered

    let oxy = rating true 0 numbers
    let co2 = rating false 0 numbers
    oxy * co2


printfn $"Day 3.1: {task1 input}"
printfn $"Day 3.2: {task2 input}"
