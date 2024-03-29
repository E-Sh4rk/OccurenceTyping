{ (* Emacs, use -*- tuareg -*- to open this file. *)
  open Parser

  let enter_newline lexbuf =
    Lexing.new_line lexbuf;
    lexbuf
}

let newline = ('\010' | '\013' | "\013\010")

let blank   = [' ' '\009' '\012']

let id = ['a'-'z']['a'-'z''A'-'Z''0'-'9''_']*

let type_id = ['A'-'Z']['a'-'z''A'-'Z''0'-'9''_']*

let decimal = ['0'-'9']+

let int = '-'? decimal

let fn = (int "." decimal?) | (int? "." decimal)

let char = '\'' ['a'-'z''A'-'Z''0'-'9''_'' ''-'] '\''

let string = '"' ['a'-'z''A'-'Z''0'-'9''_'' ''-']* '"'

rule token = parse
| newline { enter_newline lexbuf |> token }
| blank   { token lexbuf }
| "#"     { HASH }
| "atoms" { ATOMS }
| "atom"  { ATOMS }
| "type"  { TYPE }
| "and"   { TYPE_AND }
| "(*"    { comment 0 lexbuf }
| "->"    { ARROW }
| "&"     { AND  }
| "|"     { OR  }
| "\\"    { DIFF }
| "~"     { NEG  }
| ":"     { COLON }
| ","     { COMMA }
| "."     { POINT }
| "="     { EQUAL }
| "=?"    { EQUAL_OPT }
| "if"    { IF }
| "is"    { IS }
| "then"  { THEN }
| "else"  { ELSE }
| "with"  { WITH }
| "fun"   { FUN }
| "let"   { LET }
| "in"    { IN }
| "fst"   { FST }
| "snd"   { SND }
| "debug" { DEBUG }
| "Any"   { ANY }
| "Empty" { EMPTY }
| "Bool"  { BOOL }
| "Char"  { CHAR }
(*| "Float" { FLOAT }*)
| "Int"   { INT }
| "Unit"  { UNIT }
| "True"  { TRUE }
| "False" { FALSE }
| "("     { LPAREN }
| ")"     { RPAREN }
| "{"     { LBRACE }
| "}"     { RBRACE }
(*| "["     { LBRACKET }
| "]"     { RBRACKET }
| ";"     { SEMICOLON }*)
| "*"     { TIMES }
| "--"    { DOUBLEDASH }
| ".."    { DOUBLEPOINT }
(*| "-"     { MINUS }
| "+"     { PLUS  }*)
| "magic" { MAGIC }
| int as i { LINT (int_of_string i) }
(*| fn as f { LFLOAT (float_of_string f) }*)
| "true"  { LBOOL true }
| "false" { LBOOL false }
| "()"    { LUNIT }
| string as s { LSTRING (String.sub s 1 ((String.length s) - 2)) }
| char as c { LCHAR (c.[1]) }
| id as s { ID s }
| type_id as s { TID s }
| eof     { EOF }
| _ as c  {
  Printf.eprintf "Lexing error at %d: Unexpected `%c'."
    lexbuf.Lexing.lex_curr_pos c;
  exit 1
}

and comment depth = parse
| "*)" {
  if depth = 0 then token lexbuf else comment (depth - 1) lexbuf
}
| "(*" {
  comment (depth + 1) lexbuf
}
| eof {
  Printf.eprintf "Unexpected EOF inside comments.";
  exit 1
}
| _ {
  comment depth lexbuf
}