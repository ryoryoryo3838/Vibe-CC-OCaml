type t =
  | Int of int
  | Str of int * string (* 文字列リテラル: ラベルID * 文字列内容 *)
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
  | LVar of int
  | Assign of t * t
  | Return of t
  | If of t * t * t option
  | While of t * t
  | For of t option * t option * t option * t
  | Block of t list
  | Call of string * t list

type func = {
  name: string;
  args: string list;
  body: t list;
  stack_size: int;
}

type program = {
  funcs: func list;
  globals: (int * string) list; (* グローバルデータ (文字列リテラル): ラベルID * 文字列内容 *)
}
