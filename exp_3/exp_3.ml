type symbol =
  | Terminal of string
  | NonTerminal of int
  | Epsilon

type production = {
  lhs : int;
  rhs : symbol list;
}

type grammar = {
  start : int;
  rules : production list;
}
