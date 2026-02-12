%token <string> STRING
%token EOF
%start <Entry.value option> prog
%%

prog:
  | v = list_tokens { Some v }
  | EOF       { None   } ;

list_tokens:
  | vl = nonempty_list(STRING) { vl }