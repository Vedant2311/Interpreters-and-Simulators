{
  open A3
  exception Not_implemented
   exception InvalidToken of string
}

(*
  Please note the following
  - Tokens are defined in A3.mly
  - Return type is token and not token list
  - End of buffer is indicated by EOF token below
  - There is no trailer. The scanner function is written in the wrapper file (test_a4.ml)
  
*)

(*The certain regular expressions*)

let whitespace = [' ' '\t']

let digiet = ['0'-'9']
let digit1 = ['1' - '9']
let zero = '0'
let sign = '-'


let integer = zero | (sign? digit1 digiet*) 
let errorInteger1 = sign? zero digiet+ 

let boolValTrue = 'T' 
let boolValFalse = 'F'


let lowerCases = ['a' - 'z']
let upperCases = ['A' - 'Z']
let other = ''' | '_'
let alphaNum = upperCases | lowerCases | digiet | other

let identifier = upperCases alphaNum*

let errorIdentifier = lowerCases (lowerCases | upperCases)*


let addV = '+' 
let multV = '*' 

let left = '/'
let right = '\\'

let orr = right left
let ande = left right



let leftParen = '('
let rightParen = ')'	


let ifVal = "if"
let thenVal = "then"
let elseVal = "else"
let fiVal = "fi"
let cmpVal = "cmp"


(* Rules section*)

rule read = parse
  
  errorInteger1 as s {raise ( InvalidToken (String.concat " " ["Invalid integer";s]))}
| integer as i {INT (int_of_string i) }


| boolValTrue { BOOL(true) }
| boolValFalse { BOOL(false) }

| "Tint" {TINT} 
| "Tbool" {TBOOL}
| "Tunit" {TUNIT}


| identifier as s {ID (s) }
| errorIdentifier as s{ if s= "if" then (IF )
			else (
									
				if s= "then" then (THEN )
				else(

					if s= "else" then (ELSE )
					else(

						if s= "fi" then (FI ) else(

							if s = "cmp" then (CMP) else raise (InvalidToken (String.concat " " ["Invalid identifier";s]))

							)  
					)
				)

			  )

			}


(*| abs {ABS }*)


| addV {PLUS }
| multV {TIMES }

| '-' {MINUS}
| '>' {GT}

| ande {CONJ}
| orr {DISJ}

| right {BACKSLASH}


| "." {DOT}
| ":" {COLON}

| leftParen {LP}
| rightParen {RP}





(*| eof {[]}*)
| whitespace                    {read lexbuf}
| eof   {EOF}
| _  as s{raise (InvalidToken (String.concat " " ["Invalid character";String.make 1 s]))}


