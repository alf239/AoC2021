namespace AoC

open System.Collections.Generic
open System.IO
open FSharp.Control.Tasks
open System.Net.Http
open System

module Magic =

    let aocRoot =
        Environment.CurrentDirectory
        |> DirectoryInfo
        |> Seq.unfold
            (fun dir ->
                if dir = dir.Root then
                    None
                else
                    Some(dir, dir.Parent))
        |> Seq.find (fun dir -> dir.Name.StartsWith("AoC"))

    let cookie () =
        [| aocRoot.FullName; "cookie.txt" |]
        |> Path.Combine
        |> File.ReadAllText

    let inputFile day =
        [| aocRoot.FullName
           "inputs"
           $"%02d{day}.txt" |]
        |> Path.Combine

    let private fullClient () =
        let client = new HttpClient()
        client.DefaultRequestHeaders.Add("cookie", cookie ())
        client

    let taskInput year day =
        let target = inputFile day

        if not <| File.Exists target then
            task {
                use client = fullClient ()

                let url =
                    $"https://adventofcode.com/{year}/day/{day}/input"

                printfn $"!!! Hitting {url}"

                let! response = client.GetStringAsync(url)
                do! File.WriteAllTextAsync(target, response)
            }
            |> Async.AwaitTask
            |> Async.RunSynchronously

        File.ReadAllText target

    let nonEmptyString s = s <> ""

    let lines (s: string) =
        s.Split('\n') |> Array.map (fun s -> s.Trim())

    let words (s: string) =
        s.Split(' ') |> Array.filter nonEmptyString

    let ints = words >> Array.map int

    let commaSeparated (s: string) = s.Split(',')
    let csInts = commaSeparated >> Array.map int

    let nonEmptyLines = lines >> Seq.filter nonEmptyString

    let asSingleInts = nonEmptyLines >> Seq.map int

    let search<'a>
        (plan: 'a -> unit)
        (next: unit -> 'a)
        (hasWork: unit -> bool)
        (gen: 'a -> 'a seq option)
        (seed: 'a)
        : 'a seq =
        let seen = HashSet()
        plan seed

        seq {
            while hasWork () do
                let item: 'a = next ()

                if not <| seen.Contains(item) then
                    seen.Add(item) |> ignore

                    match gen item with
                    | Some items ->
                        yield item

                        for i in items do
                            plan i
                    | None -> ()
        }


    let bfs<'a> (gen: 'a -> 'a seq option) (seed: 'a) : 'a seq =
        let work = Queue()
        search work.Enqueue work.Dequeue (fun () -> work.Count > 0) gen seed

    let dfs<'a> (gen: 'a -> 'a seq option) (seed: 'a) : 'a seq =
        let work = Stack()
        search work.Push work.Pop (fun () -> work.Count > 0) gen seed

    let boolToInt b = if b then 1 else 0

    module Seq =
        let ofStack<'a> (stack: Stack<'a>) =
            seq {
                while stack.Count > 0 do
                    yield stack.Pop()
            }

        let iterate f init =
            Seq.unfold (Option.map (fun state -> state, f state)) (Some init)

    module String =
        let isLowercase (s: string) = s |> Seq.forall Char.IsLower
