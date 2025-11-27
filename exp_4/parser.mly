%{
open Ast
%}

%token INT          "int"
%token FLOAT        "float"
%token VOID         "void"
%token CONST        "const"
%token IF           "if"
%token ELSE         "else"
%token WHILE        "while"
%token BREAK        "break"
%token CONTINUE     "continue"
%token RETURN       "return"
%token <string> ID
%token <int> INT_LIT
%token PLUS         "+"
%token MINUS        "-"
%token MULT         "*"
%token DIV          "/"
%token MOD          "%"
%token ASSIGN       "="
%token EQ           "=="
%token NEQ          "!="
%token LESS         "<"
%token LEQ          "<="
%token GREATER      ">"
%token GEQ          ">="
%token AND          "&&"
%token OR           "||"
%token NOT          "!"
%token SEMICOLON    ";"
%token COMMA        ","
%token LPAREN       "("
%token RPAREN       ")"
%token LBRACKET     "["
%token RBRACKET     "]"
%token LBRACE       "{"
%token RBRACE       "}"
%token EOF          "$"

%left OR
%left AND
%left EQ NEQ
%left LESS LEQ GREATER GEQ
%left PLUS MINUS
%left MULT DIV MOD
%nonassoc NOT UPLUS UMINUS

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

%start <comp_unit> comp_unit

%%

%inline array_dim:
  | "["; e = const_exp; "]"
    { e }
  ;
%inline array_acc:
  | "["; e = exp; "]"
    { e }
  ;

comp_unit:
  | items = comp_unit_item*; "$" { items }
  ;

comp_unit_item:
  | d = decl     { DeclItem d }
  | f = func_def { FuncDefItem f }
  ;

decl:
  | tree = const_decl { tree }
  | tree = var_decl   { tree }
  ;

const_decl:
  | "const"; t = b_type; defs = separated_nonempty_list(",", const_def); ";"
    { ConstDecl (t, defs) }
  ;

b_type:
  | "int"   { Int }
  | "float" { Float }
  ;

const_def:
  | name = ID; dims = list(array_dim); "="; init = const_init_val
    { { const_name = name; const_dims = dims; const_init = init } }
  ;

const_init_val:
  | e = const_exp
    { ConstExp e }
  | "{"; vals = separated_list(",", const_init_val); "}"
    { ConstArray vals }
  ;

var_decl:
  | t = b_type; first = var_def; rest = preceded(",", var_def)*; ";"
    { VarDecl (t, first :: rest) }
  ;

var_def:
  | name = ID; dims = list(array_dim)
    { { var_name = name; var_dims = dims; var_init = None } }
  | name = ID; dims = list(array_dim); "="; init = init_val
    { { var_name = name; var_dims = dims; var_init = Some init } }
  ;

init_val:
  | e = exp
    { InitExp e }
  | "{"; vals = separated_list(",", init_val); "}"
    { InitArray vals }
  ;

func_def:
  | t = b_type; name = ID; "("; params = separated_list(",", func_f_param); ")"; body = block
    { { func_ret_type = Some t; func_name = name; func_params = params; func_body = body } }
  | "void"; name = ID; "("; params = separated_list(",", func_f_param); ")"; body = block
    { { func_ret_type = None; func_name = name; func_params = params; func_body = body } }
  ;

func_f_param:
  | t = b_type; name = ID
    { { param_type = t; param_name = name; param_dims = None } }
  | t = b_type; name = ID; "["; "]"; dims = list(array_acc)
    { { param_type = t; param_name = name; param_dims = Some dims } }
  ;

block:
  | "{"; items = list(block_item); "}"
    { items }
  ;

block_item:
  | d = decl { Decl d }
  | s = stmt { Stmt s }
  ;

stmt:
  | name = ID; indices = list(array_acc); "="; e = exp; ";"
    { Assign (name, indices, e) }
  | e = exp?; ";"
    { ExprStmt e }
  | b = block
    { Block b }
  | "if"; "("; c = cond; ")"; then_s = stmt; %prec LOWER_THAN_ELSE
    { If (c, then_s, None) }
  | "if"; "("; c = cond; ")"; then_s = stmt; "else"; else_s = stmt
    { If (c, then_s, Some else_s) }
  | "while"; "("; c = cond; ")"; s = stmt
    { While (c, s) }
  | "break"; ";"
    { Break }
  | "continue"; ";"
    { Continue }
  | "return"; e = exp?; ";"
    { Return e }
  ;

exp:
  | n = INT_LIT
    { IntLit n }
  | lv = lval
    { let (name, indices) = lv in Var (name, indices) }
  | "("; e = exp; ")"
    { e }
  | name = ID; "("; args = separated_list(",", exp); ")"
    { Call (name, args) }
  | "+"; e = exp %prec UPLUS
    { Unary (Pos, e) }
  | "-"; e = exp %prec UMINUS
    { Unary (Neg, e) }
  | "!"; e = exp %prec NOT
    { Unary (Not, e) }
  | e1 = exp; "+"; e2 = exp
    { Binary (Add, e1, e2) }
  | e1 = exp; "-"; e2 = exp
    { Binary (Sub, e1, e2) }
  | e1 = exp; "*"; e2 = exp
    { Binary (Mul, e1, e2) }
  | e1 = exp; "/"; e2 = exp
    { Binary (Div, e1, e2) }
  | e1 = exp; "%"; e2 = exp
    { Binary (Mod, e1, e2) }
  | e1 = exp; "<"; e2 = exp
    { Binary (Lt, e1, e2) }
  | e1 = exp; "<="; e2 = exp
    { Binary (Leq, e1, e2) }
  | e1 = exp; ">"; e2 = exp
    { Binary (Gt, e1, e2) }
  | e1 = exp; ">="; e2 = exp
    { Binary (Geq, e1, e2) }
  | e1 = exp; "=="; e2 = exp
    { Binary (Eq, e1, e2) }
  | e1 = exp; "!="; e2 = exp
    { Binary (Neq, e1, e2) }
  | e1 = exp; "&&"; e2 = exp
    { Binary (And, e1, e2) }
  | e1 = exp; "||"; e2 = exp
    { Binary (Or, e1, e2) }
  ;

cond:
  | e = exp { e }
  ;

lval:
  | name = ID; indices = list(array_acc)
    { (name, indices) }
  ;

const_exp:
  | e = exp { e } (* Note: all identifiers must be constant. *)
  ;
