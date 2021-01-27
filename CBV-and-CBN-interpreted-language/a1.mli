open A0

(* The possible types of expressions in the language of expressions *)
type exptype = Tint | Tunit | Tbool | Tfunc of (exptype * exptype) 

type  exptree =
 V of string 
 | Lambda of (exptree * exptype * exptree) 
 | App of (exptree * exptree) 
 | Plus of (exptree * exptree) 
 | Mult of (exptree * exptree) 
 | And of (exptree * exptree) 
 | Or of (exptree * exptree) 
 | Bool of bool 
 | Integer of int 
 | Cmp of exptree 
 | If_Then_Else of (exptree * exptree * exptree)


(* opcodes of the stack machine (in the same sequence as above) *)
type opcode = VAR of string | NCONST of bigint | BCONST of bool | RET | PLUS | MULT | CONJ | DISJ | CMP | COND of opcode list * opcode list | APP | CLOS of string * opcode list 

(* The type of value returned by the definitional interpreter. *)
type value = NumVal of int | BoolVal of bool 

(* Answer type*)
type answer = Num of bigint | Bool of bool | ValClosure of string * opcode list * (string * answer) list 


type closure = Clos of exptree * (string * closure) list | ValueClosure of  value * (string * closure) list

(* the Krivine machine *)
val krivine : ((string * exptype) list) -> exptree -> ((string * closure) list) ->  closure list -> string list -> closure

(*The compile function for post-order traversal*)
val compile : exptree -> ((string * exptype) list) -> (opcode list)

(* The SECD machine*)
val secd : (answer list) -> ((string * answer) list) -> (opcode list) -> (((answer list) * ((string * answer) list) * (opcode list)) list) -> answer

(* Type checking function*)
val obtainType : ((string * exptype) list) -> exptree -> exptype
