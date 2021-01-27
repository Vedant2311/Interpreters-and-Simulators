#directory "_build";; (* Consider this folder when looking for files *)
#load "a2.cmo";;
#load "a3.cmo";;
#load "a6.cmo";;
open A2;;
open A3;;
open A6;;

(*The expTree desribed in A6*)
let tree = Node("main", Node("P", Node("R", Node("V", Leaf, Leaf, "R"), Leaf, "P"), Node("S", Leaf, Leaf, "P"), "main"), Node("Q", Node("T", Node("W", Leaf, Leaf, "T"), Leaf, "Q"), Node("U", Leaf, Leaf, "Q"), "main"), "");;

(*The Local variables and input parameters list*)
let localVar = [("main", ["a"; "b"; "c"]);("P", ["z"; "a"]);("R", ["j"; "b"]);("V", ["c"]);("S", ["m"; "n"]);("Q", ["x"; "b"]);("T", ["i"; "f"]) ; ("W", ["j";"h"]) ; ("U",["p"; "g"])];;
let inputPara = [("main", []);("P", ["x"; "y"]);("R", ["w"; "i"]);("V", ["m"; "n"]);("S", ["c"; "k"]);("Q", ["z"; "w"]);("T", ["a"; "y"]) ; ("W", ["m";"p"]) ; ("U",["c"; "z"])];;

(*The pointer to the call stack*)
let callStack = ref ["main"];;

(*The pointer to the procedure frame*)
let framePointer = ref[(Proc("main",[],[("a", Int(6));("b",Int(8));("c",Int(15))]))];;

let execute s = A6.finallyAccesible tree s localVar inputPara
let exp_parser s = A3.comm_parser A2.read (Lexing.from_string s);;

(*The REPL*)
let _ =
    Printf.printf "==> ";flush stdout;
    try
        let lexbuf = Lexing.from_channel stdin in
            while true do
            let result = A3.comm_parser A2.read lexbuf in

            (* Call stack*)
            	let str_obt1 = (

            		match result with 

            			Call(x,l) -> (if List.length l = 2 then (

            				let accessList = (A6.finalProcCall tree (List.hd !callStack)) in 
            					(if ((A6.isPresent x accessList) && (A6.isPresentType (List.hd l) (A6.finallyAccesible tree (List.hd !callStack) localVar inputPara)) && (A6.isPresentType (List.hd (List.tl l)) (A6.finallyAccesible tree (List.hd !callStack) localVar inputPara))) then ((callStack := x :: !callStack);(framePointer:= ((A6.addPointer x localVar inputPara l (A6.getBindingsList (!framePointer) (List.hd (!callStack)) tree)) :: !framePointer));"\nThe call stack is: " ^ A6.printList (!callStack)) else ("The given function call can not happen"))

            				) else ("The input arguements should be two!")

            			)

			    |  Def(x,b) -> (

				let varlist = (A6.finallyAccesible tree (List.hd !callStack) localVar inputPara) in 
				(

					match b with
					Int(_)->	(if (A6.isPresent x varlist) then (framePointer:= (A6.updateFrame (!framePointer) x b (A6.staticLink (!callStack) tree)  (A6.getBindingsList (!framePointer) (List.hd (!callStack)) tree));"\nThe call stack is: " ^ A6.printList (!callStack)) else ("The given variable is not accesible from the current point"))
					| Var(c) -> (if (A6.isPresent x varlist) && (A6.isPresent c varlist) then (framePointer:= (A6.updateFrame (!framePointer) x b (A6.staticLink (!callStack) tree)  (A6.getBindingsList (!framePointer) (List.hd (!callStack)) tree));"\nThe call stack is: " ^ A6.printList (!callStack)) else ("The given variable is not accesible from the current point"))

			      )

			    )

			    | Return -> (if (List.hd (!callStack) = "main") then ("No call to be returned") else (

					framePointer:= (List.tl (!framePointer)); callStack:= (List.tl (!callStack)); "\nThe call stack is: " ^ A6.printList (!callStack)

			      		 )

		            )

			   
			   | Show -> "\nThe call stack is: " ^ A6.printList (!callStack)



            	) in

            	(* Procedures accesible*)
            	(

            		let str_obt2 = (

            		if (str_obt1 = "No call to be returned" || str_obt1 = "The given function call can not happen" || str_obt1 = "The input arguements should be two!" || str_obt1 = "The given variable is not accesible from the current point") then ("") else (

            			"\nProcedures accesible: " ^ A6.print_list (A6.finalProcCall tree (List.hd (!callStack))) 
           			  
            		)

            	  )

            	 in  

            		(*Static link*)

            		(let str_obt3 = (

            				if (str_obt1 = "No call to be returned" || str_obt1 = "The given function call can not happen" || str_obt1 = "The input arguements should be two!" || str_obt1 = "The given variable is not accesible from the current point") then ("") else (

            				"\nStatic Link: " ^ A6.printList2 (A6.staticLink (!callStack) tree) 
		           			  
	            		)

            	       )

            	 in 	(

            	 	(* Accesible Variables and their values*)
            		 	let str_obt4 = (
            		 		if (str_obt1 = "No call to be returned" || str_obt1 = "The given function call can not happen" || str_obt1 = "The input arguements should be two!" || str_obt1 = "The given variable is not accesible from the current point") then "" else
            		 		("\nAccesible Variables: " ^ (let q = (A6.getBindingsList (!framePointer) (List.hd (!callStack)) tree) in (printValues q)))

            		 	) in 	print_string str_obt1 ; print_string str_obt2; print_string str_obt4; print_string str_obt3; Printf.printf "\n"; Printf.printf "\n==>"; flush stdout)

            		)


            	) 

             done
        with A2.Eof ->
            exit 0


(* TESTING INFORMTATION

=> show; -> Shows the four outputs mentioned.
=> P(4,5); -> Will call P with the parameters as: x|-> 4 and y|-> 5
=> return; => Will return from the call and give the updated registers accordingly!
=> x:=4; => Will set the Variable "x" to 4, if accesible!

 A SAMPLE PROGRAM TESTED

 show;
 a:=b;
 P(a,b);
 a:=7;
 z:=11;
 return;
 c:=z;
 show;
 P(3,4);
 z:=x;
 a:=y;
 Q(z,a);
 x:=c;
 b:=a;
 P(x,b);
 R(b,c);
 z:=19;
 return;
 return;
 return;
 T(x,y);
 T(x,y,a);
 a:=d;
 return;
 return;
 show;
 


*)
