open System.Collections.Generic
open AoC.Magic

let realInput = taskInput 2021 10

let testInput =
    "[({(<(())[]>[[{[]{<()<>>\n\
[(()[<>])]({[<{<<[]>>(\n\
{([(<{}[<>[]}>{[]{[(<()>\n\
(((({<>}<{<{<>}{[]{[]{}\n\
[[<[([]))<([[{}[[()]]]\n\
[{[{({}]{}}([{[{{{}}([]\n\
{<[[]]>}<{[{[{[]{()[[[]\n\
[<(<(<(<{}))><([]([]()\n\
<{([([[(<>()){}]>(<<{{\n\
<{([{{}}[<[[[<>{}]]]>[]]"

let testAnswer1 = 26397L

let testAnswer2 = 288957L

let parse = nonEmptyLines

let errorScore (s: string) : int64 =
    let pp =
        function
        | ')' -> '(', 3L
        | ']' -> '[', 57L
        | '}' -> '{', 1197L
        | '>' -> '<', 25137L

    let stack = Stack()
    let mutable penalty: int64 = 0L

    for c in s do
        match c with
        | '('
        | '['
        | '{'
        | '<' -> stack.Push c
        | ')'
        | ']'
        | '}'
        | '>' ->
            let pair, p = pp c
            let c1 = stack.Pop()

            if c1 <> pair && penalty = 0L then
                penalty <- p

    penalty

let completionScore s =
    let cs =
        function
        | '(' -> 1L
        | '[' -> 2L
        | '{' -> 3L
        | '<' -> 4L

    let stack = Stack()

    for c in s do
        match c with
        | '('
        | '['
        | '{'
        | '<' -> stack.Push c
        | ')'
        | ']'
        | '}'
        | '>' -> stack.Pop() |> ignore

    stack
    |> Seq.ofStack
    |> Seq.map cs
    |> Seq.fold (fun acc x -> acc * 5L + x) 0L

let median xs =
    let sorted = xs |> Array.ofSeq |> Array.sort
    sorted.[Array.length sorted / 2]

let task1 = Seq.map errorScore >> Seq.sum

let task2 =
    Seq.filter (fun s -> errorScore s = 0L)
    >> Seq.map completionScore
    >> median

let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let result1 = testInput |> fullTask1
assert (result1 = testAnswer1)
let result2 = testInput |> fullTask2
assert (result2 = testAnswer2)

printfn $"Day 10.1: {realInput |> fullTask1}"
printfn $"Day 10.2: {realInput |> fullTask2}"
