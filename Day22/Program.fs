open System.Collections.Generic
open AoC.Magic

let parseRange (s: string) =
    let [| _; range |] = s.Split('=')
    let [| mn; _; mx |] = range.Split('.')
    int mn, int mx

let parse =
    nonEmptyLines
    >> Seq.map
        (fun s ->
            let [| flip; coords |] = words s
            let [| xs; ys; zs |] = commaSeparated coords
            flip = "on", parseRange xs, parseRange ys, parseRange zs)

let task1 data =
    let map = HashSet()
    for flip, (xn, xx), (yn, yx), (zn, zx) in data do
        if xn <= 50 && xx >= -50 && yn <= 50 && yx >= -50 && zn <= 50 && zx >= -50 then
            for x in (max xn -50) ..(min xx 50) do 
                for y in (max yn -50) ..(min yx 50) do 
                    for z in (max zn -50) ..(min zx 50) do
                        if flip then
                            map.Add((x, y, z)) |> ignore
                        else
                            map.Remove((x, y, z)) |> ignore
    map.Count

let task2 data = -2

let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let testInput =
    "\
on x=-20..26,y=-36..17,z=-47..7
on x=-20..33,y=-21..23,z=-26..28
on x=-22..28,y=-29..23,z=-38..16
on x=-46..7,y=-6..46,z=-50..-1
on x=-49..1,y=-3..46,z=-24..28
on x=2..47,y=-22..22,z=-23..27
on x=-27..23,y=-28..26,z=-21..29
on x=-39..5,y=-6..47,z=-3..44
on x=-30..21,y=-8..43,z=-13..34
on x=-22..26,y=-27..20,z=-29..19
off x=-48..-32,y=26..41,z=-47..-37
on x=-12..35,y=6..50,z=-50..-2
off x=-48..-32,y=-32..-16,z=-15..-5
on x=-18..26,y=-33..15,z=-7..46
off x=-40..-22,y=-38..-28,z=23..41
on x=-16..35,y=-41..10,z=-47..6
off x=-32..-23,y=11..30,z=-14..3
on x=-49..-5,y=-3..45,z=-29..18
off x=18..30,y=-20..-8,z=-3..13
on x=-41..9,y=-7..43,z=-33..15
on x=-54112..-39298,y=-85059..-49293,z=-27449..7877
on x=967..23432,y=45373..81175,z=27513..53682"

let testAnswer1 = 590784
let testAnswer2 = -2
let result1 = testInput |> fullTask1
assert (result1 = testAnswer1)
let result2 = testInput |> fullTask2
assert (result2 = testAnswer2)

let realInput = taskInput 2021 22
printfn $"Day 22.1: {realInput |> fullTask1}"
printfn $"Day 22.2: {realInput |> fullTask2}"
