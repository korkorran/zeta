%token <string> STRING
%token EOF
%start <Entry.value option> prog
%%

prog:
  | v = list_tokens; EOF { Some v }
  | EOF       { None   } ;

list_tokens:
  | vl = nonempty_list(STRING) { vl }