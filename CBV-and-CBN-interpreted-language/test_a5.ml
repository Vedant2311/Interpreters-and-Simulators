#directory "_build";; (* Consider this folder when looking for files *)
#load "a0.cmo";;
#load "a1.cmo";;
#load "a2.cmo";;
#load "a3.cmo";;
open A0;;
open A1;;
open A2;;
open A3;;

(* The gamma table and bindings to be used*)
let g = [("Y", Tint)];;
let binding = [("Y", Num(mk_big 5))];;
let binding1 = [("Y",ValueClosure(NumVal(5),[]))];;

let exp_parser s = A3.exp_parser A2.read (Lexing.from_string s);;
let exp_compile s g= let y = exp_parser s in A1.compile y g

(*For SECD*)
let stackmc stk binding s d g = let p = (exp_compile s g) in (A1.secd stk binding p d) 

(* For Krivine*)
let execute g s binding stk stk1 = let p = exp_parser s in (A1.krivine g p binding stk stk1)

(*

SAMPLE TEST CASES -> For both: Krivine and SECD

1. if T then \\X:Tint.(X+5) else \\Y:Tint.(Y*5) fi (5) -> 10
2. if Y then \\X:Tint.(X+5) else \\Y:Tint.(Y*5) fi (5) -> Type_Error
3. \\X:Tint.(X)(5) -> 5
4. if T then 
    if T then \\X:Tbool.(if X then 5 else 6 fi) 
    else \\X:Tbool.(7)
    fi 
  else \\Y:Tbool.(if Y then 7 else 6 fi) 
  fi (T) -> 5 
 

5. \\X:Tbool.(if X then \\Y:Tint.(Y) else \\Z:Tint.(Z*5) fi)(F)(5) -> 25
6.  if cmp(0) then \\Y:Tint.(Y) else \\Z:Tint.(Z*5) fi(5) -> 25
 

*)
