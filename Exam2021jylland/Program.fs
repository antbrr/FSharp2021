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




//OPG 2

let foo f =
 let mutable m = Map.empty
 let aux x =
     match Map.tryFind x m with
     | Some y when Map.containsKey x m -> y
     | None ->
     m <- Map.add x (f x) m; f x
 aux
let rec bar x =
 match x with
 | 0 -> 0
 | 1 -> 1
 | y -> baz (y - 1) + baz (y - 2)

and baz = foo bar


(* 2.1

 Q : What are the types of functions foo , bar , and baz ?

 A:
 foo: ('a -> 'b) -> 'a -> 'b
 bar: int -> int
 baz: int -> int
 
 Q : What do functions foo and baz do (skip bar )? Focus on what they do rather than how they do it.
 
 A: 
 
 foo applies function to element. It acts as a mapping function.
 
 baz takes an integer and outputs the fibonacci number of the integer input.
 
 
 
 
 
 Q: The function foo uses a mutable variable.
What function does it serve (why is it there)?
What would happen if you removed the mutable keyword from the line let mutable m = Map.empty ?
Would the function foo still work? If yes, why; if no, why not?
What would be appropriate names for functions foo , bar , and baz ?
 
 A: It is there because the variable needs to be reassigned/updated on line 81, and if we removed it, that line of code would not compile.
 
 A: Appropriate names would be foo: Map; bar: Fibonacci, baz: Fibonacci2
 
 
 
 
 *)
 
 (* 2.2
 
 It is used to define mutually recursive functions (functions that calls eachother)
 
 This line: y -> baz (y - 1) + baz (y - 2) does not compile because bar cant find baz now due to the fact that baz is defined later in the code (if we remove and)
 
 
 
 *)
 
 
let foo2 f =
 let mutable m = Map.empty
 let aux x =
     match Map.tryFind x m with
     | Some y -> y
     | None ->
     let y = f x
     m <- Map.add x y m; y
 aux

let bazSeq = Seq.initInfinite baz


let rec barbaz x =
 let baz = foo barbaz
 match x with
 | 0 -> 0
 | 1 -> 1
 | y -> baz (y - 1) + baz (y - 2)        
  
        