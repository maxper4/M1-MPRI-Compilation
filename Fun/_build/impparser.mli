
(* The type of tokens. *)

type token = 
  | WHILE
  | VAR
  | STAR
  | SLASH
  | SET
  | SEMI
  | SBRK
  | RPAR
  | RETURN
  | RBRACKET
  | PUTCHAR
  | PRCT
  | PLUS
  | OR
  | NOT
  | NEQ
  | MINUS
  | MAIN
  | LT
  | LSR
  | LSL
  | LPAR
  | LE
  | LBRACKET
  | IF
  | IDENT of (string)
  | GT
  | GE
  | FUNCTION
  | EQ
  | EOF
  | END
  | ELSE
  | CST of (int)
  | COMMA
  | BOOL of (bool)
  | BEGIN
  | AND
  | AMPERSAND

(* This exception is raised by the monolithic API functions. *)

exception Error

(* The monolithic API. *)

val program: (Lexing.lexbuf -> token) -> Lexing.lexbuf -> (Imp.program)
