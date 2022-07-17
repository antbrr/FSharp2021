type direction = North | East | South | West
type coord = C of int * int

//1.1

let move (dist: int) (dir: direction) (C(x,y): coord) =
    match dir with
    |North -> C(x,y-dist)
    |South -> C(x,y+dist)
    |West -> C(x-dist,y)
    |East -> C(x+dist,y)

let turnRight (dir: direction) =
    match dir with
    |North -> East
    |East -> South
    |South -> West
    |West -> North

let turnLeft (dir: direction) =
    match dir with
    |East -> North
    |South -> East
    |West -> South
    |North -> West
    
//1.2

type position = P of (coord * direction)
type move = TurnLeft | TurnRight | Forward of int


let step (P(c,d):position) (m: move) =
    match m with
    |TurnRight -> P(c,turnRight d)
    |TurnLeft -> P(c,turnLeft d)
    |Forward x -> P(move x d c,d)
    
//1.3
let walk (p:position) (ms: move list) =
    let rec aux acc ms' =
        match ms' with
        | [] -> acc
        | x :: xs -> aux (step acc x) xs
    aux p ms

let walk2 (p: position) (ms: move list) = List.fold(fun acc elem -> step acc elem) p ms

//1.4
let rec path (P(c,_) as p)(ms: move list) =
    match ms with
    | [] -> [c]
    | Forward x :: xs -> c :: (path (step p (Forward x)) xs)
    | x :: xs -> path (step p x) xs

let path2 p (ms: move list) =
    let rec aux acc (P (c, _) as p) ms' =
        match ms' with
        | [] -> List.rev (c::acc)
        | Forward x :: xs -> aux (c::acc) (step p (Forward x)) xs
        | x :: xs -> aux acc (step p x) xs
    aux [] p ms
        

//path3 tbd

            
let path3 _ = failwith "not implemented"
        
        