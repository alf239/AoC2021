open AoC.Magic

let realInput = taskInput 2021 13

let testInput =
    "6,10
0,14
9,10
0,3
10,4
4,11
6,0
6,12
4,1
0,13
10,12
3,4
3,0
8,4
1,10
2,14
8,10
9,0

fold along y=7
fold along x=5"

let testAnswer1 = "17"
let testAnswer2 = "\n\
#####\n\
#...#\n\
#...#\n\
#...#\n\
#####\n"

type Fold =
    | X of int
    | Y of int

let parse all =
    let ls = all |> lines

    let mapLines = ls |> Seq.takeWhile nonEmptyString

    let map =
        mapLines
        |> Seq.map (csInts >> (fun [| x; y |] -> x, y))

    let folds =
        ls
        |> Seq.skip (map |> Seq.length)
        |> Seq.filter nonEmptyString
        |> Seq.map
            (fun s ->
                let [| _; _; fold |] = s.Split(' ')

                match fold.Split('=') with
                | [| "x"; x |] -> X <| int x
                | [| "y"; y |] -> Y <| int y)

    map |> Set.ofSeq, folds |> List.ofSeq

let doFold fold =
    match fold with
    | X xf -> fun (x, y) -> if x >= xf then 2 * xf - x, y else x, y
    | Y yf -> fun (x, y) -> if y >= yf then x, 2 * yf - y else x, y

let visualise map =
    let mxx = map |> Set.map fst |> Set.maxElement
    let mxy = map |> Set.map snd |> Set.maxElement

    let result =
        seq {
            for y in 0 .. mxy do
                for x in 0 .. mxx do
                    yield
                        if map |> Set.contains (x, y) then
                            "#"
                        else
                            "."

                yield "\n"
        }
        |> String.concat ""

    result

let task1 (map, folds) =
    let fold = folds |> Seq.head
    let fn = doFold fold

    map |> Set.map fn |> Set.count |> string

let task2 (map, folds) =
    let result =
        folds
        |> List.fold (fun acc fold -> acc |> Set.map (doFold fold)) map

    "\n" + (visualise result)

let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let result1 = testInput |> fullTask1
assert (result1 = testAnswer1)
let result2 = testInput |> fullTask2
assert (result2 = testAnswer2)

printfn $"Day 13.1: {realInput |> fullTask1}"
printfn $"Day 13.2: {realInput |> fullTask2 |> (fun s -> s.Replace('.', ' '))}"
