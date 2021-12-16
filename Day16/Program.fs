open AoC.Magic

let realInput = taskInput 2021 16

let tests1 =
    [ "D2FE28", 6
      "EE00D40C823060", 14
      "8A004A801A8002F478", 16
      "620080001611562C8802118E34", 12
      "C0015000016115A2E0802F182340", 23
      "A0016C880162017C3686B18A3D4780", 31 ]

let tests2 =
    [ "D2FE28", 2021
      "C200B40A82", 3
      "04005AC33890", 54
      "880086C3E88112", 7
      "CE00C43D881120", 9
      "D8005AC2A8F0", 1
      "F600BC2D8F", 0
      "9C005AC2F8F0", 0
      "9C0141080250320F1802104A08", 1 ]

let rec consume nr (data: byte []) (pos: int) : uint64 =
    let shift = pos % 4
    let offset = pos / 4

    if (nr + shift) <= 4 then
        let hb = data.[offset]
        let masked = (hb <<< shift) &&& 15uy
        masked >>> (4 - nr) |> uint64
    else
        let hbits = 4 - shift
        let remaining = nr - hbits
        let hi = consume hbits data pos
        let lo = consume remaining data (pos + hbits)
        (hi <<< remaining) ||| lo

type Expression =
    | Sum
    | Product
    | Minimum
    | Maximum
    | GreaterThan
    | LessThan
    | EqualTo

type Packet =
    | Literal of byte * uint64
    | Operator of byte * Expression * Packet list

let expr typ =
    match int typ with
    | 0 -> Sum
    | 1 -> Product
    | 2 -> Minimum
    | 3 -> Maximum
    | 5 -> GreaterThan
    | 6 -> LessThan
    | 7 -> EqualTo

let rec readPackets data pos length =
    let mutable p = pos
    let eop = p + length

    let packets =
        seq {
            while (p + 6) < eop do
                let packet, newpos = firstPacket data p
                p <- newpos
                yield packet
        }
        |> List.ofSeq

    packets, p

and firstPacket data pos =
    let version = consume 3 data pos |> byte
    let ptype = consume 3 data (pos + 3) |> int

    match ptype with
    | 4 ->
        let mutable p = pos + 6
        let mutable five = consume 5 data p
        p <- p + 5
        let mutable x = five &&& 15UL

        while (five &&& 16UL) <> 0UL do
            five <- consume 5 data p
            p <- p + 5
            x <- (x <<< 4) ||| (five &&& 15UL)

        Literal(version, x), p
    | _ ->
        let ltype = consume 1 data (pos + 6) |> int

        if ltype = 0 then
            let length = consume 15 data (pos + 7) |> int
            let packets, p = readPackets data (pos + 7 + 15) length

            Operator(version, ptype |> expr, packets), p
        else
            let length = consume 11 data (pos + 7) |> int
            let mutable p = pos + 7 + 11

            let packets =
                seq {
                    for _ in 1 .. length do
                        let packet, newOff = firstPacket data p
                        p <- newOff
                        yield packet
                }
                |> List.ofSeq

            Operator(version, ptype |> expr, packets), p

let parse (s: string) =
    let data =
        s.Trim()
        |> Seq.map
            (function
            | x when x >= '0' && x <= '9' -> byte x - byte '0'
            | x when x >= 'A' && x <= 'F' -> byte x - byte 'A' + 10uy)
        |> Array.ofSeq

    firstPacket data 0 |> fst


let rec eval packet =
    match packet with
    | Literal (_, x) -> x
    | Operator (_, expr, packets) ->
        let terms = packets |> List.map eval

        match expr with
        | Sum -> Seq.sum terms
        | Product -> Seq.fold (*) 1UL terms
        | Minimum -> Seq.min terms
        | Maximum -> Seq.max terms
        | GreaterThan ->
            terms
            |> (fun [ a; b ] -> if a > b then 1UL else 0UL)
        | LessThan ->
            terms
            |> (fun [ a; b ] -> if a < b then 1UL else 0UL)
        | EqualTo ->
            terms
            |> (fun [ a; b ] -> if a = b then 1UL else 0UL)

let rec sumOfVersions p =
    match p with
    | Literal (version, _) -> version |> int
    | Operator (version, _, packets) ->
        (version |> int)
        + (packets |> Seq.map sumOfVersions |> Seq.sum)

let task1 = sumOfVersions >> uint64

let task2 = eval

let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let results1 = tests1 |> List.map (fst >> fullTask1)
assert (results1 = (tests1 |> List.map (snd >> uint64)))

let results2 = tests2 |> List.map (fst >> fullTask2)
assert (results2 = (tests2 |> List.map (snd >> uint64)))

printfn $"Day 16.1: {realInput |> fullTask1}"
printfn $"Day 16.2: {realInput |> fullTask2}"
