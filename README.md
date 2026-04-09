# Sudoku Solver in Racket

A simple Sudoku solver written in Racket.  
It combines constraint propagation, MRV (Minimum Remaining Values), and backtracking with `call/cc`.

Racketで書かれたシンプルな数独ソルバーです。  
制約伝播、MRV（候補数最少ヒューリスティック）、そして `call/cc` を使ったバックトラックを組み合わせています。

---

## Features / 特徴

### English

- Solves Sudoku boards represented as 9x9 lists
- Uses **constraint propagation** to repeatedly fill cells with only one possible value
- Uses **MRV heuristic** to choose the next empty cell with the fewest candidates
- Uses **backtracking** when deterministic propagation is not enough
- Uses **`call/cc`** to escape immediately when a solution is found
- Includes unit tests with `rackunit`
- Includes a small REPL interface for loading and solving boards from files

### 日本語

- 9x9 のリストで表現された数独盤面を解きます
- **制約伝播**により、候補が1つしかないマスを繰り返し埋めます
- **MRV ヒューリスティック**により、候補数が最少の空きマスを優先して選びます
- 伝播だけでは解けない場合、**バックトラック**で探索します
- 解が見つかった瞬間に **`call/cc`** で探索全体から脱出します
- `rackunit` によるテストを含みます
- ファイルから問題を読み込んで解く簡単な REPL を備えています

---

## Algorithm / アルゴリズム

### English

The solver works in three stages:

1. **Constraint propagation**  
   For each empty cell, compute candidates from:
   - the row
   - the column
   - the 3x3 square

   If only one candidate remains, fill it in.

2. **Fixed-point iteration**  
   Repeat propagation until the board no longer changes.

3. **Search with MRV + backtracking**  
   If the board is still incomplete:
   - choose the empty cell with the fewest candidates
   - try each candidate recursively
   - stop immediately when a solution is found

### 日本語

このソルバーは大きく3段階で動きます。

1. **制約伝播**  
   各空きマスについて、以下から候補を計算します。
   - 行
   - 列
   - 3x3 ブロック

   候補が1つしかない場合はその値を確定します。

2. **不動点まで反復**  
   盤面に変化がなくなるまで制約伝播を繰り返します。

3. **MRV + バックトラック探索**  
   まだ未完成なら、
   - 候補数が最少の空きマスを選び
   - 候補を順に試し
   - 解が見つかったら即座に探索を終了します

---

## Requirements / 必要環境

### English

- [Racket](https://racket-lang.org/)

### 日本語

- [Racket](https://racket-lang.org/)

---

## Running / 実行方法

### English

Run the program:

```bash
racket main.rkt
