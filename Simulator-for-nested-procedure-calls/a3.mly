%{
    open A6
%}

/* Tokens are defined below.  */

%token <int> INT
%token <string> PROC
%token <string> VAR
%token EQ LP RP RET COLON COMMA EOF EOL SHOW
%start comm_parser
%type <A6.commtree> comm_parser /* Returns expression */
%%

comm_parser: 
  parser EOL {$1}
;

parser:

  VAR COLON EQ VAR {Def($1,Var($4))}
  | VAR COLON EQ INT {Def($1,Int($4))}
  | PROC LP side_parser RP {Call($1,$3)}
  | RET {Return}
  | SHOW {Show}

side_parser:

  VAR COMMA side_parser {Var($1) :: $3}
  | INT COMMA side_parser {Int($1) :: $3}
  | VAR {[Var($1)]}
  | INT {[Int($1)]}
