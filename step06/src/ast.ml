type t =
  | Int of int
  | Add of t * t
  | Sub of t * t
  | Mul of t * t
  | Div of t * t
  | Pos of t
  | Neg of t
  | Eq  of t * t
  | Ne  of t * t
  | Lt  of t * t
  | Le  of t * t
  | Gt  of t * t
  | Ge  of t * t
