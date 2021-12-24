open AoC.Magic

let parse = nonEmptyLines

let step z a b c w =
    if z = [] || ((w - b) <> (z |> List.head)) then
        assert (a = 1)
        (w + c) :: z
    else
        assert (a = 26)
        z |> List.tail

let program (input: int64) =
    let ds =
        input
        |> string
        |> Seq.map (fun c -> int c - int '0')

    let prg =
        [ 1, 15, 9
          1, 11, 1
          1, 10, 11
          1, 12, 3
          26, -11, 10
          1, 11, 5
          1, 14, 0
          26, -6, 7
          1, 10, 9
          26, -6, 15
          26, -6, 4
          26, -16, 10
          26, -4, 4
          26, -2, 9 ]

    let steps =
        ds
        |> Seq.zip prg
        |> Seq.scan (fun z ((a, b, c), w) -> step z a b c w) []
        |> List.ofSeq

    steps |> List.last |> List.length

let task1 _ =
    let guess = 29991993698469L
    let z = program guess
    assert (z = 0)
    guess

let task2 _ =
    let guess = 14691271141118L
    let z = program guess
    assert (z = 0)
    guess


let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let realInput = taskInput 2021 24
printfn $"Day 24.1: {realInput |> fullTask1}"
printfn $"Day 24.2: {realInput |> fullTask2}"
