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

    lookup, image, false

let valueAt x y image def =
    let value = image |> Set.contains (x, y)
    value <> def

let codeAt x y image def =
    let get x y = boolToInt <| valueAt x y image def
    let b8 = get (x - 1) (y - 1)
    let b7 = get x (y - 1)
    let b6 = get (x + 1) (y - 1)
    let b5 = get (x - 1) y
    let b4 = get x y
    let b3 = get (x + 1) y
    let b2 = get (x - 1) (y + 1)
    let b1 = get x (y + 1)
    let b0 = get (x + 1) (y + 1)

    b0
    + 2 * b1
    + 4 * b2
    + 8 * b3
    + 16 * b4
    + 32 * b5
    + 64 * b6
    + 128 * b7
    + 256 * b8

let step lookup (image: Set<int * int>) def =
    let xs = image |> Set.map fst
    let ys = image |> Set.map snd

    let dots =
        seq {
            for x in (Set.minElement xs - 1) .. (Set.maxElement xs + 1) do
                for y in (Set.minElement ys - 1) .. (Set.maxElement ys + 1) do
                    let code = codeAt x y image def

                    if lookup |> Set.contains code then
                        yield x, y
        }
        |> Set.ofSeq

    dots, def

let task1 (lookup, image, def) =
    let i1, d1 = step lookup image def
    let i2, d2 = step lookup i1 d1
    assert (not d2)
    i2 |> Set.count

let task2 _ = -2

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
let testAnswer2 = -2
let result1 = testInput |> fullTask1
assert (result1 = testAnswer1)
let result2 = testInput |> fullTask2
assert (result2 = testAnswer2)

let realInput = taskInput 2021 20
printfn $"Day 20.1: {realInput |> fullTask1}"
printfn $"Day 20.2: {realInput |> fullTask2}"
