open AoC.Magic

let forReal = true
let rawInput = if forReal then taskInput 2021 4 else ""
let input = rawInput |> asStrings |> Seq.toList

let drawn =
    input
    |> Seq.head
    |> (fun s -> s.Split(','))
    |> Seq.map int
    |> Seq.toList

let boards = input |> List.tail |> List.chunkBySize 5

type Board = { Board: int array array }

let read (ss: string list) : Board =
    let board =
        ss
        |> List.filter (fun s -> s <> "")
        |> Seq.map
            (fun s ->
                s.Split(' ')
                |> Array.filter (fun s -> s <> "")
                |> Array.map int)
        |> Seq.toArray

    { Board = board }

let bs =
    boards
    |> List.map read

type PreparedBoard =
    { Wins: int Set list
      Numbers: int Set }

let prepare (b: Board) =
    let { Board = board } = b

    let wins =
        seq {
            for i in 0 .. 4 do
                yield
                    seq { for j in 0 .. 4 -> board.[i].[j] }
                    |> Set.ofSeq

                yield
                    seq { for j in 0 .. 4 -> board.[j].[i] }
                    |> Set.ofSeq
        }
        |> Seq.toList

    let nrs = board |> Array.collect id |> Set.ofArray
    { Wins = wins; Numbers = nrs }

let partialScore ns board =
    let { Wins = wins; Numbers = nrs } = board

    if wins |> List.exists (Set.isSuperset ns) then
        Set.difference nrs ns |> Seq.sum |> Some
    else
        None


let step1 (drawn: int list) (boards: Board list) =
    let bs = boards |> List.map prepare

    let wins =
        drawn
        |> Seq.scan<int, int * int Set> (fun (_, acc) n -> (n, Set.add n acc)) (-1, Set.empty)
        |> Seq.map
            (fun (n, ns) ->
                bs
                |> List.choose (partialScore ns)
                |> List.map (fun s -> s * n))

    wins |> Seq.find (fun l -> not (List.isEmpty l))


let task1 (drawn: int list) (boards: Board list) = step1 drawn boards |> Seq.head

let task2 (drawn: int list) (boards: Board list) = -2

printfn $"Day 4.1: {task1 drawn bs}"
printfn $"Day 4.2: {task2 drawn bs}"
