{ (* Emacs, use -*- tuareg -*- to open this file. *)
  open Parser

  let enter_newline lexbuf =
    Lexing.new_line lexbuf;
    lexbuf
}

let newline = ('\010' | '\013' | "\013\010")

let blank   = [' ' '\009' '\012']

let id = ['a'-'z''0'-'9''_']+

let decimal = ['0'-'9']+

let fn = (decimal "." decimal?) | (decimal? "." decimal)

let char = '\'' ['a'-'z''0'-'9''_'' '] '\''

rule token = parse
| newline { enter_newline lexbuf |> token }
| blank   { token lexbuf }
| "(*"    { comment 0 lexbuf }
| "->"    { ARROW }
| "&"     { AND  }
| "|"     { OR  }
| "~"     { NEG  }
| ":"     { COLON }
| ","     { COMMA }
| "="     { EQUAL }
| "if"    { IF }
| "is"    { IS }
| "then"  { THEN }
| "else"  { ELSE }
| "fun"   { FUN }
| "let"   { LET }
| "in"    { IN }
| "fst"   { FST }
| "snd"   { SND }
| "Any"   { ANY }
| "Empty" { EMPTY }
| "Bool"  { BOOL }
| "Char"  { CHAR }
(*| "Float" { FLOAT }*)
| "Int"   { INT }
| "True"  { TRUE }
| "False" { FALSE }
| "("     { LPAREN }
| ")"     { RPAREN }
| "*"     { TIMES }
(*| "-"     { MINUS }
| "+"     { PLUS  }*)
| "magic" { MAGIC }
| decimal as i { LINT (int_of_string i) }
(*| fn as f { LFLOAT (float_of_string f) }*)
| "true"  { LBOOL true }
| "false" { LBOOL false }
| char as c { LCHAR (c.[1]) }
| id as s { ID s }
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