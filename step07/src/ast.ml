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
  | LVar of int        (* ローカル変数: オフセット値を持つ *)
  | Assign of t * t    (* 代入: 左辺(LVar) = 右辺 *)
  | Return of t        (* return文 *)

(* プログラムは文のリスト *)
type program = t list
