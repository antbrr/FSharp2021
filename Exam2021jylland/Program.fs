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

So as we can see, path is not tail recursive because it constructs the list at the very end, and not incrementally like an accumulator

*)
        

//path3 tbd

            
let path3 (p: position) (ms: move list) =
    let rec aux cont (P(c,_) as p) ms' =
        match ms' with
        | [] -> cont [c]
        | Forward x :: xs ->
            let p' = step p (Forward x)
            aux (fun result -> cont(c :: result)) p' xs
        | x :: xs -> aux cont (step p x) xs
    aux id p ms




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
 Q: What function does it serve (why is it there)?
 A: It is there because the variable needs to be reassigned/updated on line 81.

What would happen if you removed the mutable keyword from the line let mutable m = Map.empty ?
Would the function foo still work? If yes, why; if no, why not?

 If we removed it, the line of code would not compile. Because you cannot update an immutable variable.

 Q: What would be appropriate names for functions foo , bar , and baz ?
 
 A: Appropriate names would be:
 
 foo: foo could be called memoize (it does memoization), cache, remember, or something similar. 
 
 bar: fib
 
 baz: fibAux
 
 Note: A very common problem for this assignment was the confusion with what a function does with how it does it.
    The mutable state is squarely in how something is done not what is actually being computed, and the baz function in particular had a lot of creative suggestions for how it did things rather than what it did.
    All it does is compute fibonacci numbers. If I gave you baz as a black box you could not infer anything else from it. Your safest bet would most likely be that it was a tail recursive fibonacci function since it is fast.
    Keep this in mind. The distinction is important.


    
 
 
 
 
 *)
 
 (* 2.2
 
 Q: What function does this keyword serve in general (why would you use and when writing any program)?
 
 A: It is used to define mutually recursive functions (functions that calls eachother). We would use it, if this is a property we need to utilize.
 
 Q: What would happen if you removed it from this particular program and replaced it with a standard let (change
    the line and baz = foo bar to let baz = foo bar )?

 A: This line: y -> baz (y - 1) + baz (y - 2) does not compile because bar cant find baz now due to the fact that baz is defined later in the code (if we remove and)
 
 *)
 
 (* 2.3
 
 Q: The function foo generates a warning during compilation: Warning: Incomplete pattern matches on this expression.
 
 Why does this happen, and where?
 
 A: It happens at this line "match Map.tryFind x m with". It happens because we have a "when" guard on the line, 
 "|Some y when Map.containsKey x m -> y", therefore it gives this error since we dont have a pattern match for the case where Map.containskey returns false.
 
 
 Q: For these particular three functions will this incomplete pattern match ever cause problems for any possible
execution of baz ? If yes, why; if no, why not.

A: It will not cause a problem, because when there is Some y,it is always true that the map contains the x value thus making the when clause redundant.

Q: The function foo has two redundant computations and is hence not as efficient as it could be. What are these
two computations and why are they redundant?

A: The first redudant computation is the when clause, when Map.containsKey x m, as described above.
    The second redundant computation is the fx value which is calculated 2 times on the line m <- Map.add x (f x) m; f x

*)

//2.4
 
let foo2 f =
 let mutable m = Map.empty
 let aux x =
     match Map.tryFind x m with
     | Some y -> y
     | None ->
     let y = f x
     m <- Map.add x y m; y
 aux

// 2.5
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
        


    

    
  
        