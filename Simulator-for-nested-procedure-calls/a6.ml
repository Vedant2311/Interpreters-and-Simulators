(* 

1. The expression tree for the scoping design of the code. It is assumed to be a binary tree for the purpose of this code, but it can be extended to any generic tree!

   For eg. IF THE DESIGN OF THE PROGRAM IS LIKE:
   
   		program main;

		  var a: Tint; b: Tint; c: Tint;

		  procedure P(x: Tint; y: Tint);

			var z: Tint; a: Tint;

			procedure R(w: Tint; i: Tint);

				var j: Tint; b: Tint;

				{ body of R elided }

		       { body of P elided }

		  procedure Q(z: Tint; w: Tint);

			var x: Tint; b: Tint;

			{ body of Q elided }

		  { body of main elided }


	    THEN THE SCOPING TREE WILL BE: 
	    	
					 main
				      /       \
				     P          Q
				    / \       /   \
			           R   Leaf  Leaf  Leaf		
				 /   \ 
				Leaf Leaf

	    THEN THE EXPTREE WILL BE: Node("main", Node("P", Node("R", Leaf, Leaf, "P"), Leaf, "main"), Node("Q", Leaf, Leaf, "main"), "")

2. The Node is defined as: (X,Y,Z,W): Where 
					X -> The name of the procedure
					Y -> The left child 
					Z -> The right child
					W -> The name of the parent procedure for this node

*)
type exptree = Node of string * exptree * exptree * string
              | Leaf

type basictype = Var of string 
                | Int of int 

(* The commands that this language can compute *)
type commtree = Def of string * basictype
             | Call of string * (basictype list)
             | Return
             | Show

(* Data type for the stack frame. Contains three elements: 1. Name of a procedure
							   2. List of the internal variables and their corresponding types
							   3. List of the input parameters and their corresponding types
*)
type stackFrame = Proc of string * ((string * basictype) list) * ((string * basictype) list)

exception Myexp of string

(* Finding a procedure in the expTree*)
let rec findInTree tree str = match tree with 
	 Leaf -> Leaf
	| Node(p,e1,e2,q) -> if p=str then (tree) else (let q=(findInTree e1 str) in (if q=Leaf then (findInTree e2 str) else q))

(* Checking if the procedure is present in the expTree*)
let rec isPresentInTree tree str = match tree with 
	Leaf -> false
	| Node(p,e1,e2,q) -> (p=str) || (isPresentInTree e1 str) || (isPresentInTree e2 str)

(* A tuple element finding function *)
let rec findInList str localVar = match localVar with 

	  (x,l) :: xs -> if x=str then l else (findInList str xs)
	  | [] -> []

(* Returns List of variables accessible to the Procedure with the name str, but the elements might be repeated in that list *)
let rec accesibleVar tree str localVar inputPara = let temp = (findInTree tree str) in (

	match temp with 

	Leaf -> []
  	| Node(p,e1,e2,b) -> (findInList str localVar) @ (findInList str inputPara) @ (accesibleVar (findInTree tree b) b localVar inputPara)

)

(* List finding function*)
let rec isPresent x xs = match xs with 

	y :: xs' -> if x=y then true else (isPresent x xs')
	| _ -> false

(* List reversal*)
let rec revList l = match l with 
  	
  	 [] -> []
  	 | x :: xs -> (revList xs) @ [x]

(* Removing repetitions in list*)
let rec removeRep l = match l with 
	
	x :: xs  -> if (isPresent x xs) then (removeRep xs) else x :: (removeRep xs )
	| _ -> []

(*Returns List of variables accessible to the Procedure with the name str but with the change that this list doesn't contain repeating elements*)
let finallyAccesible tree str localVar inputPara = let b = (accesibleVar tree str localVar inputPara) in (revList (removeRep (revList b)))

(* Returns list of the procedures which can be called from the procedure with the name str*)
let rec procCall tree str = match tree with 

	Leaf -> []
	| Node(p,e1,e2,b) -> if (isPresentInTree tree str) then (

					if (isPresentInTree e1 str) then (

						let alpha = (

							match  e2 with
							 Leaf -> []
							| Node(a,_,_,_) -> [a]

						) and beta = (


							match e1 with
							  Leaf -> []
							| Node(c,e3,e4,_) -> [c] @ (procCall e1 str) 

						) in alpha @ beta

					) else if (isPresentInTree e2 str) then (


						let alpha = (

							match  e1 with
							 Leaf -> []
							| Node(a,_,_,_) -> [a]

						) and beta = (


							match e2 with
							  Leaf -> []
							| Node(c,e3,e4,_) -> [c] @ (procCall e2 str)

						) in alpha @ beta

				)

				else(

					let alpha = (

							match  e1 with
							 Leaf -> []
							| Node(a,_,_,_) -> [a]

					) and beta = (

							match e2 with
							  Leaf -> []
							| Node(c,e3,e4,_) -> [c]

					) in alpha @ beta

				)


			) else []

let finalProcCall tree str = revList (procCall tree str)

(* Printing functions*)

let rec printList l = match l with 

	  x :: [] -> x 
	| x :: xs -> x ^ " <= " ^ (printList xs)
	| _ ->  "\n"

let rec printList2 l = match l with 

	  x :: [] -> x 
        | x :: xs -> x ^ " => " ^ (printList2 xs)
	| _ ->  "\n"


let rec print_list l = match l with 

	   x :: [] -> x
	 | x :: xs ->  x ^ " ; " ^ (print_list xs)
	 | _ -> "\n"	

(*Finding parent in tree for the corresponding procedure*)
let findParent x tree = let y = findInTree tree x in (

	match y with 
	Node(a,_,_,c) -> c

)

(* Tuple functions corresponding to getting the values from the Bindings list*)

let rec findValue b bindings = match bindings with 

	(a,c) :: xs -> if a=b then c else (findValue b xs)

let findInBind y bindings = match y with 

	Int(a) -> Int(a)
	| Var(b) -> let q = (findValue b bindings) in q


(* Some basic list functions*)

let rec findParentInList x l tree= (findParent x tree)

let rec removeTill y l = match l with 

	x :: xs -> if x=y then l else (removeTill y xs)
	| [] -> []


(*Returns the Static link chain. Here, l is the Call stack*)
let rec staticLink l tree= let x = List.hd l in (

	match x with 

		"main" -> ["main"]
		| _ -> let y=(findParentInList x l tree) in x :: (staticLink (removeTill y l) tree)
	
	
)

let rec makePairs l = match l with 
	[] -> []
	| x :: xs -> (x, Var(x)) :: (makePairs xs)

let addPointer str localVar inputPara l bindings= (Proc(str, ([((List.hd (findInList str inputPara)),(findInBind (List.hd l) bindings));((List.hd (List.tl (findInList str inputPara))),(findInBind (List.hd (List.tl l)) bindings))]), (makePairs (findInList str localVar))))

let rec removeTillBindings y l = match l with 

	(Proc(a,l1,l2)) :: xs -> if a=y then l else (removeTillBindings y xs)
	| [] -> []

let rec getBindingsTemp l str tree= let x = List.hd l in (

	match x with 
	Proc("main", l1, l2 ) -> l1 @ l2
	| Proc(a,l1,l2) -> let y=(findParent a tree) in (l1 @ l2 @ (getBindingsTemp (removeTillBindings y l) str tree))

)

let rec isPresentInBindings x l = match l with 

	(a,_) :: xs -> (

		match x with 
		(b,_) -> if a=b then true else (isPresentInBindings x xs)

	)

	| [] -> false

let rec removeRepBindings l = match l with 

	x :: xs -> if (isPresentInBindings x xs) then (removeRepBindings xs) else x :: (removeRepBindings xs)
	| [] -> []

let getBindingsList l str tree = revList (removeRepBindings (revList (getBindingsTemp l str tree)))

let rec printValues y =

	match y with

	 (a,Var(b)) :: [] -> "[" ^ a ^ " |-> " ^ b ^ "]" 
	| (a,Int(b)) :: [] -> "[" ^ a ^ " |-> " ^ (string_of_int b) ^ "]"
	
	| (a,Var(b)) :: xs -> "[" ^ a ^ " |-> " ^ b ^ "]; " ^ (printValues xs)
	| (a,Int(b)) :: xs -> "[" ^ a ^ " |-> " ^ (string_of_int b) ^ "]; " ^ (printValues xs)
	| [] -> "" 

let rec updateList x y l bindings = match l with 

	(a,b) :: xs -> if a=x then (a,(findInBind y bindings)) :: xs else (a,b) :: (updateList x y xs bindings)
	| [] -> []

let rec isPresentInBindings1 x l = match l with 

	(a,b) :: xs -> if a=x then true else (isPresentInBindings1 x xs)
	| [] -> false

let rec updateFrame l x y stat bindings = (

	match l with 

	Proc(a,l1,l2) :: xs -> (if (a=(List.hd stat)) then (

		(if (isPresentInBindings1 x l1) then (let l1' = (updateList x y l1 bindings) in ((Proc(a,l1',l2)) :: xs)) else (if (isPresentInBindings1 x l2) then (let l2' = (updateList x y l2 bindings) in ((Proc(a,l1,l2')) :: xs)) else (Proc(a,l1,l2) ::(updateFrame xs x y (List.tl stat) bindings))))

	) else ((Proc(a,l1,l2)) :: (updateFrame xs x y stat bindings)))

	| [] -> []

)

let isPresentType x l = match x with 

			Int(a) -> true
			| Var(b) -> (isPresent b l)
