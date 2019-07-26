#!/usr/bin/env bash

DIR=`dirname $0`

run_dytype() {
  guile --no-auto-compile $DIR/dytype.scm "$@"
}

assert_unspecified() {
  output=`run_dytype "$@"`
  if grep --quiet ': #<unspecified>' <<< "$output"; then
    printf '.'
  else
    echo
    echo "Expected"
    echo "  $@"
    echo "to be"
    echo "  : unspecified"
    echo "but it was"
    echo "  $output"
    exit 1
  fi
}

assert_n() {
  output=`run_dytype "${@:2}"`
  if grep --quiet ": $1" <<< "$output"; then
    printf '.'
  else
    echo
    echo "Expected"
    echo "  ${@:2}"
    echo "to be"
    echo "  : $1"
    echo "but it was"
    echo "  $output"
    exit 1
  fi
}

assert_unspecified "(42 42)"

assert_n 1 42
assert_n 1 '"foo"'
assert_n 2 '(lambda (x) x)'
assert_n 7 '(lambda (a b c d e f) f)'

assert_n 1 '(+ 1 2 3 4 5 6)'

assert_n 1 '(if 1 1)'
assert_unspecified '(if (lambda (x) x) 1)'
assert_n 1 '(if #t 1 1)'
assert_n 2 '(if #t (lambda (x) 42) (lambda (y) 99))'
assert_unspecified '(if #t (lambda (x) 42) 99)'
assert_unspecified '(if 1 2 3 4)'
assert_unspecified '(if 1 (lambda (x)))'

assert_n 1 "`cat $DIR/examples/fizzbuzz.dy`"

echo
