# OCamlLex と Menhir の使い方ガイド

このプロジェクトでは、C言語サブセットのコンパイラを作成するために、OCamlの標準的なパーサジェネレータである **OCamlLex** (字句解析) と **Menhir** (構文解析) を使用しています。

このドキュメントでは、`src/lexer.mll` と `src/parser.mly` の具体的な記述内容を通して、これらのツールの基本的な使い方を解説します。

## 1. 全体の流れ

コンパイル処理は以下の流れで行われます。

1.  **ソースコード** (文字列)
2.  **Lexer (OCamlLex)**: 文字列を意味のある単位（トークン）の列に分解します。
    *   例: `"1 + 2"` -> `INT(1)`, `PLUS`, `INT(2)`
3.  **Parser (Menhir)**: トークンの列を解析して、抽象構文木 (AST) を構築します。
    *   例: `INT(1)`, `PLUS`, `INT(2)` -> `Add(Int 1, Int 2)`
4.  **Codegen**: ASTをトラバースしてアセンブリコードを出力します。

## 2. OCamlLex (`src/lexer.mll`)

`ocamllex` は、正規表現に基づいて入力文字列をトークンに変換するツールです。ファイル拡張子は `.mll` です。

### 構造

```ocaml
{
  (* ヘッダー部分: 必要なモジュールのopenや補助関数の定義 *)
  open Parser (* Parserで定義されたトークンを使うために必要 *)
}

(* 定義部分: 正規表現に名前を付ける *)
let digit = ['0'-'9']
let space = [' ' '\t' '\n' '\r']

(* ルール部分: エントリーポイントの定義 *)
rule read = parse
  | space+ { read lexbuf }        (* アクション: 再帰呼び出しでスキップ *)
  | digit+ as n { INT (int_of_string n) } (* アクション: INTトークンを返す *)
  | '+' { PLUS }                  (* アクション: PLUSトークンを返す *)
  | eof { EOF }
  | _ { failwith "Unknown character" }
```

### ポイント

*   **`rule read = parse ...`**: `read` という名前の関数を定義します。この関数は引数として `lexbuf` (入力バッファ) を受け取ります。
*   **パターンマッチ**: `| パターン { アクション }` の形式で記述します。上から順にマッチングが行われます。
*   **`as n`**: マッチした文字列を変数 `n` に束縛します。
*   **再帰呼び出し**: `read lexbuf` を呼び出すことで、次のトークンの読み込みを行います（空白スキップなどで使用）。
*   **トークン**: `INT`, `PLUS` などは、後述する `parser.mly` で定義されたコンストラクタです。

## 3. Menhir (`src/parser.mly`)

`menhir` は、LR(1) 文法に基づいてトークン列を解析するパーサジェネレータです。ファイル拡張子は `.mly` です。`ocamlyacc` の強力な代替ツールです。

### 構造

```ocaml
%{
  (* ヘッダー部分: AST定義のopenなど *)
  open Ast
%}

(* トークン定義: Lexerが返すトークンの型を定義 *)
%token <int> INT        (* 値を持つトークン *)
%token PLUS MINUS       (* 値を持たないトークン *)
%token EOF

(* 優先順位と結合性: 下に行くほど優先順位が高い *)
%left PLUS MINUS        (* 左結合: 1+2+3 は (1+2)+3 *)
%left TIMES DIV
%nonassoc UMINUS        (* 単項演算子 *)

(* 開始記号の定義: <型> 名前 *)
%start <Ast.t> program

%%

(* 文法ルール部分 *)

program:
  | e = expr; EOF { e } (* 式の後にEOFが来たら、その式を返す *)

expr:
  | e1 = expr; PLUS; e2 = expr { Add (e1, e2) } (* 足し算のルール *)
  | i = INT { Int i }                           (* 整数のルール *)
  | LPAREN; e = expr; RPAREN { e }              (* 括弧のルール *)
```

### ポイント

*   **`%token`**: 使用するトークンを宣言します。`<int>` のように型を指定すると、値を保持するトークンになります。
*   **`%left`, `%right`, `%nonassoc`**: 演算子の優先順位と結合性を定義します。
    *   記述順が下のものほど優先順位が高くなります（`TIMES` > `PLUS`）。
    *   `%left` は左結合 (`1-2-3` -> `(1-2)-3`)。
*   **`program:`**: 文法のルールを定義します。
    *   `| 要素1; 要素2 { OCamlのコード }` の形式です。
    *   `e = expr` のように変数に束縛して、`{ }` 内で使用します。
    *   `{ }` 内の式の結果が、このルールの評価値（ASTのノード）になります。

## 4. Dune との連携

`src/dune` ファイルで以下のように設定することで、ビルド時に自動的にコード生成が行われます。

```lisp
(ocamllex
 (modules lexer)
)

(menhir
 (modules parser)
)
```

また、`dune-project` に `(using menhir 2.1)` の記述が必要です。

## 5. 9cc との対応

9cc（C言語で書かれたコンパイラ）では、トークナイザと再帰下降パーサを手書きしていますが、本プロジェクトではこれらをツールに任せています。

*   **9ccの `tokenize`** ⇔ **OCamlLex (`lexer.mll`)**
*   **9ccの `expr`, `mul`, `primary` 等の関数** ⇔ **Menhir (`parser.mly`)**

ツールを使うことで、文法の変更（演算子の追加など）が宣言的に記述でき、可読性と保守性が向上します。
