#!/bin/bash
# step08/test.sh

# Get the directory of this script
DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

# Build inside the step directory
pushd "$DIR" > /dev/null
dune build src/main.exe || exit 1
popd > /dev/null

assert() {
  expected="$1"
  input="$2"

  # Run dune exec with the correct root
  echo "$input" | dune exec --root "$DIR" src/main.exe > tmp.s || exit 1
  cc -o tmp tmp.s || exit 1
  ./tmp
  actual="$?"

  if [ "$actual" = "$expected" ]; then
    echo "$input => $actual"
  else
    echo "$input => $expected expected, but got $actual"
    exit 1
  fi
}

assert 0 "0;"
assert 42 "42;"
assert 21 "5+20-4;"
assert 41 " 12 + 34 - 5 ;"
assert 47 "5+6*7;"
assert 15 "5*(9-6);"
assert 4 "(3+5)/2;"
assert 10 "-10+20;"
assert 10 "- -10;"
assert 10 "- - +10;"

assert 0 "0==1;"
assert 1 "42==42;"
assert 1 "0!=1;"
assert 0 "42!=42;"

assert 1 "0<1;"
assert 0 "1<1;"
assert 0 "2<1;"
assert 1 "0<=1;"
assert 1 "1<=1;"
assert 0 "2<=1;"

assert 1 "1>0;"
assert 0 "1>1;"
assert 0 "1>2;"
assert 1 "1>=0;"
assert 1 "1>=1;"
assert 0 "1>=2;"

assert 3 "a=3; a;"
assert 8 "a=3; z=5; a+z;"
assert 6 "a=b=3; a+b;"
assert 3 "foo=3; foo;"
assert 8 "foo123=3; bar=5; foo123+bar;"

assert 1 "return 1; 2; 3;"
assert 2 "1; return 2; 3;"
assert 3 "1; 2; return 3;"

assert 3 "if (0) return 2; return 3;"
assert 2 "if (1) return 2; return 3;"
assert 2 "if (1) return 2; else return 3;"
assert 3 "if (0) return 2; else return 3;"

assert 3 "{ 1; 2; return 3; }"
assert 3 "{ { 1; return 3; } return 5; }"

assert 10 "i=0; while(i<10) i=i+1; return i;"
assert 55 "i=0; j=0; while(i<=10) {j=j+i; i=i+1;} return j;"

assert 55 "j=0; for (i=0; i<=10; i=i+1) j=j+i; return j;"
assert 3 "for (;;) return 3; return 5;"

echo OK
rm -f tmp tmp.s
