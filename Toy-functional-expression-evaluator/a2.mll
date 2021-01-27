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

(*The regular expressions*)

let whitespace = [' ' '\t']

let digiet = ['0'-'9']
let digit1 = ['1' - '9']
let zero = '0'

let integer = zero | ( digit1 digiet*) 
let errorInteger1 = zero digiet+ 

let boolValTrue = 'T' 
let boolValFalse = 'F'


let lowerCases = ['a' - 'z']
let upperCases = ['A' - 'Z']
let other = ''' | '_'
let alphaNum = upperCases | lowerCases | digiet | other

let identifier = upperCases alphaNum*

let errorIdentifier = lowerCases (lowerCases | upperCases)*

let abs = 'a' 'b' 's'
let tempTilda = '~'

let note = 'n' 'o' 't'

let addV = '+' whitespace*
let multV = '*' whitespace*
let subV = '-'

let div = 'd' 'i' 'v'
let modu = 'm' 'o' 'd'
let left = '/'
let right = '\\'

let orr = right left
let ande = left right

let equal = '='
let greaterThan = '>'
let lessThan = '<'

let leftParen = '('
let rightParen = ')'	


let ifVal = "if"
let thenVal = "then"
let elseVal = "else"
let fiVal = "fi"



let commaVal = ','
















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
| errorIdentifier as s{ if s="abs" then (ABS ) 
			else (

				if s="not" then (NOT ) 
				else (	


					if s= "div" then (DIV )
					else(

						if s="mod" then (REM )
						else(

							if s= "if" then (IF )
							else (
													
								if s= "then" then (THEN )
								else(

									if s= "else" then (ELSE )
									else(

										if s= "fi" then (FI ) else (

											if s="proj" then (PROJ ) else(  

												if s="let" then (LET) else(	

																	
													if s = "in" then (IN) else(

													       if s= "end" then (END) else(

															if s= "def" then (DEF) else(

																if s="local" then (LOCAL) else(

																     		
																     	raise (InvalidToken (String.concat " " ["Invalid identifier";s]))

																 )


															)

														)


													)
												)	


										)


									)													


								)



							)


						)


					)



				)



			)
			
		)
		
	}




| tempTilda {TILDA }

| addV {PLUS }
| subV {MINUS }
| multV {TIMES }

| ande {CONJ}
| orr {DISJ}

| right {BACKSLASH}


| "." {DOT}

| ";" {SEMICOLON}
| "||" {PARALLEL}
| ":"  {COLON}

| greaterThan {GT}
| lessThan {LT}
| equal {EQ}


| leftParen {LP}
| rightParen {RP}

| commaVal {COMMA }

| whitespace                    {read lexbuf}
| eof   {EOF}
| _  as s{raise (InvalidToken (String.concat " " ["Invalid character";String.make 1 s]))}


