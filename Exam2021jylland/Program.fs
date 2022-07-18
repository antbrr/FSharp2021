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
    
//1.5
let path2 p (ms: move list) =
    let rec aux acc (P (c, _) as p) ms' =
        match ms' with
        | [] -> List.rev (c::acc)
        | Forward x :: xs -> aux (c::acc) (step p (Forward x)) xs
        | x :: xs -> aux acc (step p x) xs
    aux [] p ms

//1.6

(*
Lets make the following function call: 



C(0,0) :: path (P (C (0, 0), North) (Forward 5))
    [TurnRight; Forward 10;];

C(0,0) :: path (P(C(0,-5),North)) 
    [TurnRight; Forward 10;];

C(0,0) :: path P(C(0,-5),North) (TurnRight))
    [Forward 10];

C(0,0) :: path P(C(0,-5),East))
    [Forward 10];

C(0,0) :: C(0,-5) :: pathP(C(0,-5),East))
    [Forward 10];

C(0,0) :: C(0,-5) :: pathP(C(0,-5), Forward 10)) []

C(0,0) :: C(0,-5) :: path P(C(10,-5)) []

C(0,0) :: C(0, -5) :: C(10,-5)

[C(0,0); C(0,-5); C(10,-5)];






let rec path (P(c,_) as p)(ms: move list) =
    match ms with
    | [] -> [c]
    | Forward x :: xs -> c :: (path (step p (Forward x)) xs)
    | x :: xs -> path (step p x) xs

let move (dist: int) (dir: direction) (C(x,y): coord) =
    match dir with
    |North -> C(x,y-dist)
    |South -> C(x,y+dist)
    |West -> C(x-dist,y)
    |East -> C(x+dist,y)

    
    
*)
        

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
 
 //Question 3: Guess the next sequence element
 
 
 //3.1
type element = E of int list

//3.2
let elToString (el:element) = el.ToString()

let elFromString (s: string) =
    s.ToCharArray()
    |> List.ofArray
    |> List.map(fun x -> int x)
    |> E
 
//3.3

//OBS: Jespers kode

let nextElement =
        let rec numToList =
            function
            | 0 -> []
            | x -> (x % 10) :: numToList (x / 10)

        let rec aux num x acc =
            function
            | []                 -> x::(numToList num)@acc |> List.rev
            | y :: xs when x = y -> aux (num + 1) x acc xs
            | y :: xs            -> aux 1 y (x::(numToList num)@acc) xs

        function
        | [] -> []
        | x :: xs -> aux 1 x [] xs

//3.4

//OBS: Jespers kode

let elSeq = Seq.unfold (fun n -> let n' = nextElement n in Some(n, n'))
let rec elSeq2 n = seq {yield n; yield! elSeq2 (nextElement n)}

    (*

    Q: Why would Seq.initInfinite not be an appropriate choice to
       write a function like elSeq?

    A: Seq.initInfinite takes a funciton that expects an index in the
         sequence as an argument and computes the element at that index
         in the sequence. In our use case this is bad for two reasons
       
       1: We don't even have a function that would calculate 
           the `nth` element from a given starting element
           we'd have to create one.
          
       2: Every time we construct an element in the sequence we would
   have to recompute every element up until that element in the sequence. 
           As the sequence gets longer this becomes very expensive.

    *)

 //3.5
 
 (*
    open JParsec.TextParser

    (* Determining failure on this one is harder than intended. 
          If your parser only parsed a partial string, up until parsing failed, 
          you were given full credit. In this example we have added 
       a terminating newline to demonstrate how it could be done
       with the API at hand but leaving this out gave you full credit.
       
       Another option would be to compare the length of the resulting
       list with the length of the parsed string. *)

    let elParse = many (digit |>> (int >> (fun x -> x - int '0'))) .>> pchar '\n'
    let elFromString2 str =
        run elParse (str + "\n") |>
        getSuccess
 *)
 
 //4: Rings
 
 //4.1
 
type 'a ring = R of 'a list * 'a list

//4.2

let length (R(a,b)) = List.length a + List.length b

let rec ringFromList(lst: 'a list) = R([],lst)

let rec ringToList(R(xs,ys)) =
    ys @ List.rev xs

//4.3

let empty = R([],[])

let push (x: 'a) (R(xs,ys)) = R(xs, x::ys)

let peek (R(a,b)) =
    match R(a,b) with
    | R([],[]) -> None
    | R(a,[]) ->
        let arev = List.rev a
        Some(List.head arev)
    | R(_, y :: _) -> Some y

let pop (R(a,b)) =
    match R(a,b) with
    |R([],[]) -> None
    |R(a,[]) -> Some(R([],List.tail(List.rev a)))
    |R(a, _::b) -> Some(R(a,b))
    
let cw (R(a,b)) =
    match R(a,b) with
    | R([],[]) -> R([],[])
    | R([],_ :: xs) ->
        let brev = List.rev b
        let brevhead = List.head brev
        R(List.tail brev, [brevhead])
    | R(x:: xs,_) ->R(xs, x::b)

let ccw (R(a,b)) =
    match R(a,b) with
    | R([],[]) -> R([],[])
    | R(a,[]) ->
        let arev = List.rev a
        let arevhead = List.head arev
        R([arevhead],List.tail arev)
    | R(xs,y::ys) ->R(y::xs, ys)
        


    

    
  
        