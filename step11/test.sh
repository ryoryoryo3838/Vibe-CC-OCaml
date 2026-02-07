#!/bin/bash
# step11/test.sh

# Get the directory of this script
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# Build inside the step directory
pushd "$DIR" > /dev/null
dune build src/main.exe || exit 1
popd > /dev/null

# Helper C functions
cat <<EOC > tmp_helper.c
#include <stdio.h>
#include <stdlib.h>

int foo() { printf("OK\n"); return 0; }
int bar(int x, int y) { printf("%d, %d\n", x, y); return x + y; }
int alloc4(int **p, int a, int b, int c, int d) {
  int *q = malloc(sizeof(int) * 4);
  q[0] = a; q[1] = b; q[2] = c; q[3] = d;
  *p = q;
  return 0;
}
EOC
cc -c tmp_helper.c

assert() {
  expected="$1"
  input="$2"

  # Run dune exec with the correct root
  # Use printf to handle escaped newlines in input correctly
  printf "$input" | dune exec --root "$DIR" src/main.exe > tmp.s || exit 1
  cc -o tmp tmp.s tmp_helper.o || exit 1
  ./tmp > tmp.out
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

assert 0 "main() { return 0; }"
assert 42 "main() { return 42; }"
assert 21 "main() { return 5+20-4; }"
assert 41 "main() { return  12 + 34 - 5 ; }"
assert 47 "main() { return 5+6*7; }"
assert 15 "main() { return 5*(9-6); }"
assert 4 "main() { return (3+5)/2; }"
assert 10 "main() { return -10+20; }"
assert 10 "main() { return - -10; }"
assert 10 "main() { return - - +10; }"

assert 0 "main() { return 0==1; }"
assert 1 "main() { return 42==42; }"
assert 1 "main() { return 0!=1; }"
assert 0 "main() { return 42!=42; }"

assert 1 "main() { return 0<1; }"
assert 0 "main() { return 1<1; }"
assert 0 "main() { return 2<1; }"
assert 1 "main() { return 0<=1; }"
assert 1 "main() { return 1<=1; }"
assert 0 "main() { return 2<=1; }"

assert 1 "main() { return 1>0; }"
assert 0 "main() { return 1>1; }"
assert 0 "main() { return 1>2; }"
assert 1 "main() { return 1>=0; }"
assert 1 "main() { return 1>=1; }"
assert 0 "main() { return 1>=2; }"

assert 3 "main() { a=3; return a; }"
assert 8 "main() { a=3; z=5; return a+z; }"
assert 6 "main() { a=b=3; return a+b; }"
assert 3 "main() { foo=3; return foo; }"
assert 8 "main() { foo123=3; bar=5; return foo123+bar; }"

assert 1 "main() { return 1; 2; 3; }"
assert 2 "main() { 1; return 2; 3; }"
assert 3 "main() { 1; 2; return 3; }"

assert 3 "main() { if (0) return 2; return 3; }"
assert 2 "main() { if (1) return 2; return 3; }"
assert 2 "main() { if (1) return 2; else return 3; }"
assert 3 "main() { if (0) return 2; else return 3; }"

assert 3 "main() { { 1; 2; return 3; } }"
assert 3 "main() { { { 1; return 3; } return 5; } }"

assert 10 "main() { i=0; while(i<10) i=i+1; return i; }"
assert 55 "main() { i=0; j=0; while(i<=10) {j=j+i; i=i+1;} return j; }"

assert 55 "main() { j=0; for (i=0; i<=10; i=i+1) j=j+i; return j; }"
assert 3 "main() { for (;;) return 3; return 5; }"

# Function Call Tests
assert 0 "main() { foo(); return 0; }"
assert 7 "main() { return bar(3, 4); }"
assert 14 "main() { return bar(3, 4) + bar(3, 4); }"

# Function Definition Tests
assert 55 "fib(n) { if (n<=1) return 1; return fib(n-1) + fib(n-2); } main() { return fib(9); }"
assert 8 "add(a, b) { return a+b; } main() { return add(3, 5); }"

# Comment and Preprocessor Tests
# Use escaped newline \\n because test.sh uses printf
assert 2 "main() { // return 1; \\n return 2; }"
assert 3 "main() { /* return 1; */ return 3; }"
assert 5 "#include <stdio.h>\\nmain() { return 5; }"

echo OK
rm -f tmp tmp.s tmp_helper.c tmp_helper.o tmp.out
