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

%nonassoc LOWER_THAN_ELSE
%nonassoc ELSE

%left OR
%left AND
%left EQ NEQ
%left LESS LEQ GREATER GEQ
%left PLUS MINUS
%left MULT DIV MOD
%right NOT

%start <Ast.comp_unit> comp_unit

%%

%inline array_dim:
  | "[" e = const_exp "]" { e }
  ;
%inline array_acc:
  | "[" e = exp "]" { e }
  ;

comp_unit:
  | items = comp_unit_item* "$" { items }
  ;

comp_unit_item:
  | d = decl     { Ast.DeclItem d }
  | f = func_def { Ast.FuncDefItem f }
  ;

decl:
  | tree = const_decl { tree }
  | tree = var_decl   { tree }
  ;

const_decl:
  | "const" t = b_type defs = separated_nonempty_list(",", const_def) ";"
    { Ast.ConstDecl (t, defs) }
  ;

b_type:
  | "int"   { Ast.Int }
  | "float" { Ast.Float }
  ;

const_def:
  | name = ID dims = list(array_dim) "=" init = const_init_val
    { { Ast.const_name = name; const_dims = dims; const_init = init } }
  ;

const_init_val:
  | e = const_exp
    { Ast.ConstExp e }
  | "{" vals = separated_list(",", const_init_val) "}"
    { Ast.ConstArray vals }
  ;

var_decl:
  | t = b_type first = var_def rest = preceded(",", var_def)* ";"
    { Ast.VarDecl (t, first :: rest) }
  ;

var_def:
  | name = ID dims = list(array_dim)
    { { Ast.var_name = name; var_dims = dims; var_init = None } }
  | name = ID dims = list(array_dim) "=" init = init_val
    { { Ast.var_name = name; var_dims = dims; var_init = Some init } }
  ;

init_val:
  | e = exp
    { Ast.InitExp e }
  | "{" vals = separated_list(",", init_val) "}"
    { Ast.InitArray vals }
  ;

func_def:
  | t = b_type name = ID "(" params = separated_list(",", func_f_param) ")" body = block
    {
      {
        Ast.func_ret_type = Some t;
        func_name = name;
        func_params = params;
        func_body = body
      }
    }
  | "void" name = ID "(" params = separated_list(",", func_f_param) ")" body = block
    {
      {
        Ast.func_ret_type = None;
        func_name = name;
        func_params = params;
        func_body = body
      }
    }
  ;

func_f_param:
  | t = b_type name = ID
    {
      {
        Ast.param_type = t;
        param_name = name;
        param_dims = None
      }
    }
  | t = b_type name = ID "[" "]" dims = list(array_acc)
    {
      {
        Ast.param_type = t;
        param_name = name;
        param_dims = Some(dims)
      }
    }
  ;

block:
  | "{" items = list(block_item) "}" { items }
  ;

block_item:
  | d = decl { Ast.Decl d }
  | s = stmt { Ast.Stmt s }
  ;

%inline stmt_exp_else:
  | "else" s = stmt { s }
  ;

stmt:
  | name = ID indices = list(array_acc) "=" e = exp ";"
    { Ast.Assign (name, indices, e) }
  | e = exp? ";"
    { Ast.ExprStmt e }
  | b = block
    { Ast.Block b }
  | "if" "(" c = cond ")" then_s = stmt %prec LOWER_THAN_ELSE
    { Ast.If (c, then_s, None) }
  | "if" "(" c = cond ")" then_s = stmt "else" else_s = stmt
    { Ast.If (c, then_s, Some else_s) }
  | "while" "(" c = cond ")" s = stmt
    { Ast.While (c, s) }
  | "break" ";"
    { Ast.Break }
  | "continue" ";"
    { Ast.Continue }
  | "return" e = exp? ";"
    { Ast.Return e }
  ;

exp:
  | e = add_exp { e }
  ;

cond:
  | e = l_or_exp { e }
  ;

lval:
  | name = ID indices = list(array_acc)
    { (name, indices) }
  ;

primary_exp:
  | "(" e = exp ")"
    { e }
  | lv = lval
    { let (name, indices) = lv in Ast.Var (name, indices) }
  | n = number
    { n }
  ;

number:
  | n = INT_LIT { Ast.IntLit n }
  ;

unary_exp:
  | e = primary_exp
    { e }
  | name = ID "(" args = func_r_params? ")"
    { Ast.Call (name, Option.value args ~default:[]) }
  | op = unary_op e = unary_exp
    { Ast.Unary (op, e) }
  ;

unary_op:
  | "+" { Ast.Pos }
  | "-" { Ast.Neg }
  | "!" { Ast.Not }
  ;

func_r_params:
  | args = separated_nonempty_list(",", exp) { args }
  ;

%inline mul_op:
  | "*" { Ast.Mul }
  | "/" { Ast.Div }
  | "%" { Ast.Mod }
  ;

mul_exp:
  | e = unary_exp
    { e }
  | e1 = mul_exp op = mul_op e2 = unary_exp
    { Ast.Binary (op, e1, e2) }
  ;

%inline add_op:
  | "+" { Ast.Add }
  | "-" { Ast.Sub }
  ;

add_exp:
  | e = mul_exp
    { e }
  | e1 = add_exp op = add_op e2 = mul_exp
    { Ast.Binary (op, e1, e2) }
  ;

%inline rel_op:
  | "<"  { Ast.Lt }
  | ">"  { Ast.Gt }
  | "<=" { Ast.Leq }
  | ">=" { Ast.Geq }
  ;

rel_exp:
  | e = add_exp
    { e }
  | e1 = rel_exp op = rel_op e2 = add_exp
    { Ast.Binary (op, e1, e2) }
  ;

%inline eq_op:
  | "==" { Ast.Eq }
  | "!=" { Ast.Neq }
  ;

eq_exp:
  | e = rel_exp
    { e }
  | e1 = eq_exp op = eq_op e2 = rel_exp
    { Ast.Binary (op, e1, e2) }
  ;

l_and_exp:
  | e = eq_exp
    { e }
  | e1 = l_and_exp "&&" e2 = eq_exp
    { Ast.Binary (Ast.And, e1, e2) }
  ;

l_or_exp:
  | e = l_and_exp
    { e }
  | e1 = l_or_exp "||" e2 = l_and_exp
    { Ast.Binary (Ast.Or, e1, e2) }
  ;

const_exp:
  | e = add_exp { e }
  ;
