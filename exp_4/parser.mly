%{
open Ast
%}

%token INT VOID CONST
%token IF ELSE WHILE BREAK CONTINUE RETURN
%token <string> ID
%token <int> INT_LIT
%token PLUS MINUS MULT DIV MOD
%token ASSIGN
%token EQ NEQ LESS LEQ GREATER GEQ
%token AND OR NOT
%token SEMICOLON COMMA
%token LPAREN RPAREN
%token LBRACKET RBRACKET
%token LBRACE RBRACE
%token EOF

%left OR
%left AND
%left EQ NEQ
%left LESS LEQ GREATER GEQ
%left PLUS MINUS
%left MULT DIV MOD
%right NOT

%start <unit> program

%%

program:
  | comp_unit EOF { () }
  ;

comp_unit:
  | EOF { failwith "todo" }
  ;
