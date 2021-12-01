namespace AoC

open System.IO
open FSharp.Control.Tasks
open System.Net.Http
open System

module Magic =
    let root =
        Environment.CurrentDirectory |> DirectoryInfo

    let aocRoot =
        root
        |> Seq.unfold
            (fun dir ->
                if dir = dir.Root then
                    None
                else
                    Some(dir, dir.Parent))
        |> Seq.find (fun dir -> dir.Name.EndsWith("AoC2021"))

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

    let asSingleInts (input: string) =
        input.Split('\n')
        |> Seq.filter (fun s -> (String.length s) > 0)
        |> Seq.map int
