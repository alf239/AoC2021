open System.Collections.Generic
open AoC.Magic
open Microsoft.FSharp.Core

[<StructuredFormatDisplay("{DisplayString}")>]
type Pair =
    | Num of int64
    | Pair of Pair * Pair
    override this.ToString() =
        match this with
        | Num x -> string x
        | Pair (a, b) -> $"[{a.ToString()},{b.ToString()}]"

    member this.DisplayString = this.ToString()

let parseFish (s: string) =
    let stack = Stack()

    for c in s do
        match c with
        | '[' -> ()
        | ']' ->
            let b = stack.Pop()
            let a = stack.Pop()
            stack.Push(Pair(a, b))
        | ',' -> ()
        | x when x >= '0' && x <= '9' ->
            let n = int x - int '0'
            stack.Push(Num n)

    stack.Pop()

let parse =
    nonEmptyLines >> Seq.map parseFish >> List.ofSeq

let rec addRight x p =
    match p with
    | Num y -> Num(x + y)
    | Pair (a, b) -> Pair(a, b |> addRight x)

let rec addLeft x p =
    match p with
    | Num y -> Num(x + y)
    | Pair (a, b) -> Pair(addLeft x a, b)

let rec explode level =
    function
    | Pair (Num a, Num b) when level >= 4 -> Some(a, b, Num 0)
    | Pair (a, b) ->
        explode (level + 1) a
        |> Option.map (fun (cl, cr, a') -> cl, 0L, Pair(a', addLeft cr b))
        |> Option.orElseWith
            (fun () ->
                explode (level + 1) b
                |> Option.map (fun (cl, cr, b') -> 0L, cr, Pair(addRight cl a, b')))
    | _ -> None

let rec split =
    function
    | Pair (a, b) ->
        split a
        |> Option.map (fun a' -> Pair(a', b))
        |> Option.orElseWith (fun () -> split b |> Option.map (fun b' -> Pair(a, b')))
    | Num x when x >= 10 ->
        let a = x / 2L
        let b = x - a
        Some <| Pair(Num a, Num b)
    | _ -> None

let reduce p =
    explode 0 p
    |> Option.map (fun (_, _, p) -> p)
    |> Option.orElseWith (fun () -> split p)

let canonical p = Seq.iterate reduce p |> Seq.last

let rec render p = p.ToString()

let tCanonical = parseFish >> canonical >> render

let example1 = tCanonical "[[[[[9,8],1],2],3],4]"
assert (example1 = "[[[[0,9],2],3],4]")

let example2 =
    tCanonical "[[3,[2,[1,[7,3]]]],[6,[5,[4,[3,2]]]]]"

assert (example2 = "[[3,[2,[8,0]]],[9,[5,[7,0]]]]")

let add a b = Pair(a, b) |> canonical

let tAdd a b =
    add (parseFish a) (parseFish b) |> render

let example3 =
    tAdd "[[[[4,3],4],4],[7,[[8,4],9]]]" "[1,1]"

assert (example3 = "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")

let example4 =
    tAdd "[[[0,[4,5]],[0,0]],[[[4,5],[2,6]],[9,5]]]" "[7,[[[3,7],[4,3]],[[6,3],[8,8]]]]"

assert (example4 = "[[[[4,0],[5,4]],[[7,7],[6,0]]],[[8,[7,7]],[[7,9],[5,0]]]]")

let rec magnitude =
    function
    | Num x -> x
    | Pair (a, b) ->
        let ma = magnitude a
        let mb = magnitude b
        3L * ma + 2L * mb

let fishSum = List.reduce add

let task1 = fishSum >> magnitude

let task2 data =
    seq {
        for a in data do
            for b in data do
                yield a, b
    }
    |> Seq.map (fun (a, b) -> add a b |> magnitude)
    |> Seq.max

let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let testInput0 =
    "\
[1,1]
[2,2]
[3,3]
[4,4]"

let testSum0 = testInput0 |> parse |> fishSum |> render
assert (testSum0 = "[[[[1,1],[2,2]],[3,3]],[4,4]]")

let testInput =
    "\
[[[0,[5,8]],[[1,7],[9,6]]],[[4,[1,2]],[[1,4],2]]]
[[[5,[2,8]],4],[5,[[9,9],0]]]
[6,[[[6,2],[5,6]],[[7,6],[4,7]]]]
[[[6,[0,7]],[0,9]],[4,[9,[9,0]]]]
[[[7,[6,4]],[3,[1,3]]],[[[5,5],1],9]]
[[6,[[7,3],[3,2]]],[[[3,8],[5,7]],4]]
[[[[5,4],[7,7]],8],[[8,3],8]]
[[9,3],[[9,9],[6,[4,9]]]]
[[2,[[7,7],7]],[[5,8],[[9,3],[0,2]]]]
[[[[5,2],5],[8,[3,7]]],[[5,[7,5]],[4,4]]]"

let testSum1 = testInput |> parse |> fishSum |> render
assert (testSum1 = "[[[[6,6],[7,6]],[[7,7],[7,0]]],[[[7,7],[7,7]],[[7,8],[9,9]]]]")

let testAnswer1 = 4140L
let testAnswer2 = 3993L
let result1 = testInput |> fullTask1
assert (result1 = testAnswer1)
let result2 = testInput |> fullTask2
assert (result2 = testAnswer2)

let realInput = taskInput 2021 18
printfn $"Day 18.1: {realInput |> fullTask1}"
printfn $"Day 18.2: {realInput |> fullTask2}"
