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

let bs = boards |> List.map read

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

let wins (drawn: int list) (boards: Board list) =
    let bs =
        boards |> List.map prepare |> List.indexed

    drawn
    |> Seq.scan<int, int * int Set> (fun (_, acc) n -> (n, Set.add n acc)) (-1, Set.empty)
    |> Seq.map
        (fun (n, ns) ->
            bs
            |> List.choose (fun (i, b) -> partialScore ns b |> Option.map (fun x -> i, x))
            |> List.map (fun (i, s) -> (i, s * n)))

let step1 (drawn: int list) (boards: Board list) =
    wins drawn boards
    |> Seq.find (fun l -> not (List.isEmpty l))

let step2 (drawn: int list) (boards: Board list) =
    wins drawn boards
    |> Seq.windowed 2
    |> Seq.filter (fun [| a ; b |] -> (List.length a) <> (List.length b))
    |> Seq.last           

let task1 (drawn: int list) (boards: Board list) = step1 drawn boards |> Seq.head

let task2 (drawn: int list) (boards: Board list) =
    let [| before ; after |] = step2 drawn boards
    let prev = before |> List.map fst |> Set.ofList
    after |> List.filter (fun (b, _) -> not (Set.contains b prev)) 

printfn $"Day 4.1: {task1 drawn bs}"
printfn $"Day 4.2: {task2 drawn bs}"
