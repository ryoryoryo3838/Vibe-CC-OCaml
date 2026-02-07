open Ast

(* スタックマシンのコード生成関数 *)
(* ASTを受け取り、対応するx86-64アセンブリを出力する *)
let rec gen = function
  | Int i ->
      (* 整数リテラル: スタックにプッシュする *)
      Printf.printf "  push %d\n" i
  | Add (lhs, rhs) ->
      (* 加算: 左辺と右辺を評価し、結果をスタックに積む *)
      gen lhs;
      gen rhs;
      (* スタックから2つの値を取り出す *)
      Printf.printf "  pop rdi\n"; (* 右辺 *)
      Printf.printf "  pop rax\n"; (* 左辺 *)
      (* 加算を実行 (rax = rax + rdi) *)
      Printf.printf "  add rax, rdi\n";
      (* 結果をスタックにプッシュ *)
      Printf.printf "  push rax\n"
  | Sub (lhs, rhs) ->
      (* 減算: 左辺と右辺を評価 *)
      gen lhs;
      gen rhs;
      Printf.printf "  pop rdi\n"; (* 右辺 *)
      Printf.printf "  pop rax\n"; (* 左辺 *)
      (* 減算を実行 (rax = rax - rdi) *)
      Printf.printf "  sub rax, rdi\n";
      Printf.printf "  push rax\n"
  | Mul (lhs, rhs) ->
      (* 乗算: 左辺と右辺を評価 *)
      gen lhs;
      gen rhs;
      Printf.printf "  pop rdi\n"; (* 右辺 *)
      Printf.printf "  pop rax\n"; (* 左辺 *)
      (* 乗算を実行 (rax = rax * rdi) *)
      (* imul は符号付き乗算 *)
      Printf.printf "  imul rax, rdi\n";
      Printf.printf "  push rax\n"
  | Div (lhs, rhs) ->
      (* 除算: 左辺と右辺を評価 *)
      gen lhs;
      gen rhs;
      Printf.printf "  pop rdi\n"; (* 右辺 *)
      Printf.printf "  pop rax\n"; (* 左辺 *)
      (* cqo: RAXの符号ビットをRDXに拡張する (idivの準備) *)
      Printf.printf "  cqo\n";
      (* idiv: 符号付き除算 (RDX:RAX / RDI) *)
      (* 商はRAX、剰余はRDXに格納される *)
      Printf.printf "  idiv rdi\n";
      Printf.printf "  push rax\n"
  | Pos e ->
      (* 単項プラス: 何もしないが、オペランドのコード生成は行う *)
      gen e
  | Neg e ->
      (* 単項マイナス: オペランドを評価 *)
      gen e;
      Printf.printf "  pop rax\n";
      (* 符号反転 (rax = -rax) *)
      Printf.printf "  neg rax\n";
      Printf.printf "  push rax\n"
  | Eq (lhs, rhs) ->
      (* 等価比較 (==) *)
      gen lhs;
      gen rhs;
      Printf.printf "  pop rdi\n";
      Printf.printf "  pop rax\n";
      Printf.printf "  cmp rax, rdi\n"; (* 比較 *)
      Printf.printf "  sete al\n";      (* 等しければALを1に、そうでなければ0にセット *)
      Printf.printf "  movzb rax, al\n";(* AL (8bit) を RAX (64bit) にゼロ拡張 *)
      Printf.printf "  push rax\n"
  | Ne (lhs, rhs) ->
      (* 非等価比較 (!=) *)
      gen lhs;
      gen rhs;
      Printf.printf "  pop rdi\n";
      Printf.printf "  pop rax\n";
      Printf.printf "  cmp rax, rdi\n";
      Printf.printf "  setne al\n";     (* 等しくなければAL=1 *)
      Printf.printf "  movzb rax, al\n";
      Printf.printf "  push rax\n"
  | Lt (lhs, rhs) ->
      (* 小なり (<) *)
      gen lhs;
      gen rhs;
      Printf.printf "  pop rdi\n";
      Printf.printf "  pop rax\n";
      Printf.printf "  cmp rax, rdi\n";
      Printf.printf "  setl al\n";      (* 小さければAL=1 *)
      Printf.printf "  movzb rax, al\n";
      Printf.printf "  push rax\n"
  | Le (lhs, rhs) ->
      (* 小なりイコール (<=) *)
      gen lhs;
      gen rhs;
      Printf.printf "  pop rdi\n";
      Printf.printf "  pop rax\n";
      Printf.printf "  cmp rax, rdi\n";
      Printf.printf "  setle al\n";     (* 小さいか等しければAL=1 *)
      Printf.printf "  movzb rax, al\n";
      Printf.printf "  push rax\n"
  | Gt (lhs, rhs) ->
      (* 大なり (>) *)
      gen lhs;
      gen rhs;
      Printf.printf "  pop rdi\n";
      Printf.printf "  pop rax\n";
      Printf.printf "  cmp rax, rdi\n";
      Printf.printf "  setg al\n";      (* 大きければAL=1 *)
      Printf.printf "  movzb rax, al\n";
      Printf.printf "  push rax\n"
  | Ge (lhs, rhs) ->
      (* 大なりイコール (>=) *)
      gen lhs;
      gen rhs;
      Printf.printf "  pop rdi\n";
      Printf.printf "  pop rax\n";
      Printf.printf "  cmp rax, rdi\n";
      Printf.printf "  setge al\n";     (* 大きいか等しければAL=1 *)
      Printf.printf "  movzb rax, al\n";
      Printf.printf "  push rax\n"
