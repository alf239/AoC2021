open AoC.Magic

let parse s =

    let hashIndices =
        Seq.mapi (fun i x -> if x = '#' then i else -1)
        >> Seq.filter (fun x -> x >= 0)
        >> Set.ofSeq

    let ls = nonEmptyLines s |> Seq.map hashIndices

    let lookup = ls |> Seq.head

    let image =
        ls
        |> Seq.tail
        |> Seq.mapi (fun i idxs -> idxs |> Set.map (fun x -> x, i))
        |> Seq.collect id
        |> Set.ofSeq

    (fun code -> Set.contains code lookup), image

let valueAt x y image def =
    let value = image |> Set.contains (x, y)
    value <> def

let codeAt x y image def =
    let get x y = boolToInt <| valueAt x y image def

    get (x + 1) (y + 1)
    + 2 * get x (y + 1)
    + 4 * get (x - 1) (y + 1)
    + 8 * get (x + 1) y
    + 16 * get x y
    + 32 * get (x - 1) y
    + 64 * get (x + 1) (y - 1)
    + 128 * get x (y - 1)
    + 256 * get (x - 1) (y - 1)

let step algo (image: Set<int * int>) def =
    let xs = image |> Set.map fst
    let ys = image |> Set.map snd

    let emptySpace = if def then 512 - 1 else 0
    let newDef = algo emptySpace

    let dots =
        seq {
            for x in (Set.minElement xs - 1) .. (Set.maxElement xs + 1) do
                for y in (Set.minElement ys - 1) .. (Set.maxElement ys + 1) do
                    let code = codeAt x y image def

                    if (algo code) <> newDef then yield x, y
        }
        |> Set.ofSeq

    dots, newDef

let twoSteps algo image =
    let i1, d1 = step algo image false
    let i2, d2 = step algo i1 d1
    assert (not d2)
    i2

let task1 (algo, image) = twoSteps algo image |> Set.count

let task2 (algo, image) =
    Seq.iterate (fun img -> Some <| twoSteps algo img) image
    |> Seq.item (50 / 2)
    |> Set.count

let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let testInput =
    "\
..#.#..#####.#.#.#.###.##.....###.##.#..###.####..#####..#....#..#..##..##\
#..######.###...####..#..#####..##..#.#####...##.#.#..#.##..#.#......#.###\
.######.###.####...#.##.##..#..#..#####.....#.#....###..#.##......#.....#.\
.#..#..##..#...##.######.####.####.#.#...#.......#..#.#.#...####.##.#.....\
.#..#...##.#.##..#...##.#.##..###.#......#.#.......#.#.#.####.###.##...#..\
...####.#..#..#.##.#....##..#.####....##...##..#...#......#.#.......#.....\
..##..####..#...#.#.#...##..#.#..###..#####........#..####......#..#

#..#.
#....
##..#
..#..
..###"

let testAnswer1 = 35
let testAnswer2 = 3351
let result1 = testInput |> fullTask1
assert (result1 = testAnswer1)
let result2 = testInput |> fullTask2
assert (result2 = testAnswer2)

let realInput = taskInput 2021 20
printfn $"Day 20.1: {realInput |> fullTask1}"
printfn $"Day 20.2: {realInput |> fullTask2}"
