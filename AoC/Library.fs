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

    let lines (s: string) = s.Split('\n')

    let words (s: string) =
        s.Split(' ') |> Array.filter nonEmptyString

    let ints = words >> Array.map int

    let csInts (s: string) = s.Split(',') |> Array.map int

    let nonEmptyLines = lines >> Seq.filter nonEmptyString

    let asSingleInts = nonEmptyLines >> Seq.map int

    let bfs<'a> (next: 'a -> 'a seq) seed =
        let seen = HashSet()
        let work = Queue()
        work.Enqueue seed

        while work.Count > 0 do
            let item = work.Dequeue()

            if not <| seen.Contains(item) then
                seen.Add(item) |> ignore

                for i in next item do
                    work.Enqueue i

        ()
