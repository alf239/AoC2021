open AoC.Magic

/// Assuming ordered ranges
type Area = { X1: int; X2: int; Y1: int; Y2: int }
type Point = { X: int; Y: int }
type Velocity = { VX: int; VY: int }

let parse s =
    let a = words s
    let parseRange (r: string) = r.Split('=').[1].Split('.')
    let [| x1; _; x2 |] = parseRange a.[2]
    let [| y1; _; y2 |] = parseRange a.[3]

    { X1 = int x1
      X2 = int <| x2.TrimEnd(',')
      Y1 = int y1
      Y2 = int y2 }

let missed (area: Area) (point: Point) = point.X > area.X2 || point.Y < area.Y1

let within (area: Area) (point: Point) =
    point.X >= area.X1
    && point.X <= area.X2
    && point.Y >= area.Y1
    && point.Y <= area.Y2

let trajectory (velocity: Velocity) =
    Seq.unfold
        (fun (x, y, vx, vy) ->
            let nx = x + vx
            let ny = y + vy
            let nvx = max 0 (vx - 1)
            let nvy = vy - 1
            Some({ X = x; Y = y }, (nx, ny, nvx, nvy)))
        (0, 0, velocity.VX, velocity.VY)

let willHit (area: Area) (velocity: Velocity) =
    trajectory velocity
    |> Seq.takeWhile (not << missed area)
    |> Seq.exists (within area)

let maxHeight (velocity: Velocity) = (1 + velocity.VY) * velocity.VY / 2

let acceptableVelocities area =
    let minvx = int <| sqrt (2.0 * (double area.X1))

    seq {
        for vx in minvx .. area.X2 do
            for vy in area.Y1 .. (-area.Y1) do
                yield { VX = vx; VY = vy }
    }
    |> Seq.filter (willHit area)

let task1 =
    acceptableVelocities
    >> Seq.map maxHeight
    >> Seq.max

let task2 = acceptableVelocities >> Seq.length

let fullTask1 = parse >> task1
let fullTask2 = parse >> task2

let testInput = "target area: x=20..30, y=-10..-5"
let testAnswer1 = 45
let testAnswer2 = 112
let result1 = testInput |> fullTask1
assert (result1 = testAnswer1)
let result2 = testInput |> fullTask2
assert (result2 = testAnswer2)

let realInput = taskInput 2021 17
printfn $"Day 17.1: {realInput |> fullTask1}"
printfn $"Day 17.2: {realInput |> fullTask2}"
