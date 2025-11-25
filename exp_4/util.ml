type source_location = {
  lineno : int;
  colno : int;
}

let split_position (pos : Lexing.position) : source_location =
  { lineno = pos.pos_lnum; colno = pos.pos_cnum - pos.pos_bol + 1 }
