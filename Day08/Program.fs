open AoC.Magic

let realInput = taskInput 2021 8

let testInput =
    "be cfbegad cbdgef fgaecd cgeb fdcge agebfd fecdb fabcd edb |\
fdgacbe cefdb cefbgd gcbe
edbfga begcd cbg gc gcadebf fbgde acbgfd abcde gfcbed gfec |\
fcgedb cgb dgebacf gc
fgaebd cg bdaec gdafb agbcfd gdcbef bgcad gfac gcb cdgabef |\
cg cg fdcagb cbg
fbegcd cbd adcefb dageb afcb bc aefdc ecdab fgdeca fcdbega |\
efabcd cedba gadfec cb
aecbfdg fbg gf bafeg dbefa fcge gcbea fcaegb dgceab fcbdga |\
gecf egdcabf bgf bfgea
fgeab ca afcebg bdacfeg cfaedg gcfdb baec bfadeg bafgc acf |\
gebdcfa ecba ca fadegcb
dbcfg fgd bdegcaf fgec aegbdf ecdfab fbedc dacgb gdcebf gf |\
cefg dcbef fcge gbcadfe
bdfegc cbegaf gecbf dfcage bdacg ed bedf ced adcbefg gebcd |\
ed bcgafe cdgba cbgef
egadfb cdbfeg cegd fecab cgb gbdefca cg fgcdab egfdb bfceg |\
gbdfcae bgc cg cgb
gcafb gcf dcaebfg ecagb gf abcdeg gaef cafbge fdbac fegbdc |\
fgae cfgab fg bagce"

let testAnswer1 = 26
let testAnswer2 = 61229

let parseLine (s: string) =
    let [| input; output |] = s.Split('|')
    let ins = words input
    let outs = words output
    ins, outs

let parse = nonEmptyLines >> Seq.map parseLine

let lengthIn (ls: int list) (s: string) =
    let l = s |> String.length
    List.contains l ls

let ofSize (l: int) = Set.count >> (fun x -> x = l)

let task1 data =
    data
    |> Seq.map snd
    |> Seq.collect (Array.filter (lengthIn [ 2; 3; 4; 7 ]))
    |> Seq.length

let key ins =
    let all: Set<Set<char>> = ins |> Seq.map Set.ofSeq |> Set.ofSeq

    let _1 = all |> Seq.find (ofSize 2)
    let _7 = all |> Seq.find (ofSize 3)
    let _4 = all |> Seq.find (ofSize 4)
    let _8 = all |> Seq.find (ofSize 7)
    let _1478 = [ _1; _4; _7; _8 ] |> Set.ofList
    let _023569 = Set.difference all _1478
    let _039 = _023569 |> Set.filter (Set.isSubset _1)
    let _9 = Seq.find (Set.isSubset _4) _039
    let _03 = Set.remove _9 _039
    let _3 = _03 |> Seq.find (ofSize 5)
    let _0 = _03 |> Seq.find (ofSize 6)
    let _256 = Set.difference _023569 _039
    let _6 = _256 |> Seq.find (ofSize 6)
    let _25 = Set.remove _6 _256

    let _2 =
        _25 |> Seq.find (Set.intersect _4 >> ofSize 2)

    let _5 =
        _25 |> Seq.find (Set.intersect _4 >> ofSize 3)

    Map.ofList [ (_0, 0)
                 (_1, 1)
                 (_2, 2)
                 (_3, 3)
                 (_4, 4)
                 (_5, 5)
                 (_6, 6)
                 (_7, 7)
                 (_8, 8)
                 (_9, 9) ]

let decode (input, output) =
    let k = key input

    output
    |> Seq.map (fun x -> Map.find (Set.ofSeq x) k)
    |> Seq.fold (fun acc x -> acc * 10 + x) 0

let task2 data = data |> Seq.map decode |> Seq.sum

let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let result1 = testInput |> fullTask1
assert (result1 = testAnswer1)
let result2 = testInput |> fullTask2
assert (result2 = testAnswer2)

printfn $"Day 8.1: {realInput |> fullTask1}"
printfn $"Day 8.2: {realInput |> fullTask2}"
