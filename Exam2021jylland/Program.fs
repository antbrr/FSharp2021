module Exam2021
open System
open Microsoft.FSharp.Core

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
let elToString (E(lst)) =
    lst
    |> List.map(fun el -> string el)
    |> String.concat ""
    

let elFromString (s: string) =
     s
    |> List.ofSeq
    |> List.map(fun x -> (int x - int '0'))
    |> E

 //3.3
 
let nextElement el =
    let rec aux acc last n lst1 =
        match lst1 with
        | [] ->
            E(List.rev(last :: n :: acc))
        | x :: xs when x = last -> aux acc x (n+1) xs
        | x :: xs -> aux (last :: n :: acc) x 1 xs
    let (E lst1) = elToString el |> elFromString
    List.tryHead lst1
    |> Option.map (fun elem -> aux [] elem 0 lst1)
    |> Option.defaultValue (E[])

   
     
     

 
 //3.4
let elSeq (el: element) = Seq.unfold(fun st -> Some(st,nextElement st)) el
let rec elSeq2 e =
 seq {
     let next = nextElement e
     yield e
     yield! elSeq2 next
 }
   

// Why would Seq.initInfinite not be an appropriate choice to write a function like elSeq ?

 //3.5
 
open JParsec
 
open JParsec.TextParser

let elParse = many digit .>> pchar('\n') |>> (
    fun lst -> List.map(fun c -> (int c - int '0')) lst |> E
    )
let elFromString2(str: string) =
 run elParse str
 |> getSuccess
 
 


 

 (* Question 4.5 *)
    
  
   
    (*let pushParse = pstring "PUSH" >>. (many1 (pchar(' '))) >>. pint32 |>> Push
    
    let addParse = pstring "ADD" |>> fun _ -> Add
    
    let multParse = pstring "MULT" |>> fun _ -> Mult
        
    let parseStackProg =
        many ( (many (pchar(' '))) >>. (multParse <|> addParse <|> pushParse) .>> many (pchar(' ')) .>> pchar '\n')

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

//4.4

type StateMonad<'a, 'b> = SM of ('b ring -> ('a * 'b ring) option)
let ret x = SM (fun st -> Some (x, st))
let bind (SM m) f = SM (fun st ->
    match m st with
    | None -> None
    | Some (x, st') ->
    let (SM g) = f x
    g st')
let (>>=) m f = bind m f
let (>>>=) m n = m >>= (fun () -> n)
let evalSM (SM f) s = f s
        
let smLength = SM (fun st -> Some(length st, st))

let smPush x = SM (fun st -> Some((),(push x st)))

let smPop = SM(fun st ->
    match peek st with
    | None -> None
    | Some x -> Some(x,pop st |> Option.get))

let smCW = SM(fun st -> Some( (), cw st))

let smCCW = SM(fun st -> Some( (), ccw st))


//4.5

let ringStep =
    smLength >>= (fun length -> if length < 2 then ret () else
            smPop >>= (fun first ->
                    smPop >>= fun second ->
                               if (first + second) % 2 = 0 then ret ()
                               else smPush second >>>= smPush first >>>= smCCW))
    
                                
                                (*
                                else
                                    let first = smPop >>= (fun elem -> ret elem)
                                    let second = smPop >>= (fun elem -> ret elem)
                                    if first + second % 2 == 0 then smPop first smPop second
                                    else*)

let rec iterRemoveSumEven (x: uint32) =
    match x with
    | 0u -> ret ()
    | n -> ringStep >>>= iterRemoveSumEven (n - 1u) 


    

    
  
        