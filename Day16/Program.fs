open AoC.Magic

let realInput = taskInput 2021 16

let tests =
    [ "D2FE28", 6, -2
      "EE00D40C823060", 14, -2
      "8A004A801A8002F478", 16, -2
      "620080001611562C8802118E34", 12, -2
      "C0015000016115A2E0802F182340", 23, -2
      "A0016C880162017C3686B18A3D4780", 31, -2 ]

let parse (s: string) =
    s.Trim()
    |> Seq.map
        (function
        | x when x >= '0' && x <= '9' -> byte x - byte '0'
        | x when x >= 'A' && x <= 'F' -> byte x - byte 'A' + 10uy)
    |> Array.ofSeq

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

type Packet =
    | Literal of byte * uint64
    | Operator of byte * byte * Packet list

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

            Operator(version, ptype |> byte, packets), p
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

            Operator(version, ptype |> byte, packets), p

let rec sumOfVersions p =
    match p with
    | Literal (version, _) -> version |> int
    | Operator (version, _, packets) ->
        (version |> int)
        + (packets |> Seq.map sumOfVersions |> Seq.sum)

let task1 data =
    let packet, _ = firstPacket data 0
    sumOfVersions packet

let task2 data = -2

let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let results1 =
    tests
    |> Seq.map (fun (t, _, _) -> fullTask1 t)
    |> List.ofSeq

assert (results1 = (tests
                    |> Seq.map (fun (_, r, _) -> r)
                    |> List.ofSeq))

let results2 =
    tests
    |> Seq.map (fun (t, _, _) -> fullTask2 t)
    |> List.ofSeq

assert (results2 = (tests
                    |> Seq.map (fun (_, _, r) -> r)
                    |> List.ofSeq))

printfn $"Day 16.1: {realInput |> fullTask1}"
printfn $"Day 16.2: {realInput |> fullTask2}"
