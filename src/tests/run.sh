#!/usr/bin/env bash

os=$(uname -s)
indent="  "
quiet=0
exit_status=0
compile_error=0

green() {
  printf "${indent}\033[0;32m%s\033[0m %s\n" "$1" "$2"
}

yellow() {
  printf "${indent}\033[0;33m%s\033[0m %s\n" "$1" "$2"
}

red() {
  printf "${indent}\033[0;31m%s\033[0m %s\n" "$1" "$2"
}

error () {
  {
    printf "${indent}%s\n" "$@"
  } >&2
}

usage() {
  echo
  echo "${indent}Description: run all integration tests"
  echo
  echo "${indent}Usage: run.sh [-qh]"
  echo
  echo "${indent}Options:"
  echo
  echo "${indent}  -q, --quiet           suppress error messages"
  echo "${indent}  -h, --help            output help and usage"
  echo
}

evaluate() {
  # Stop execution exceeding 10 seconds to prevent infinite loops
  ulimit -t 10

  ../apollo $2 < "$1" 2> /dev/null

  if test $? -eq 1; then
    echo "<compilation error>"
  fi
}

compare() {
  diff -Bw <(echo "${1}") <(echo "${2}")
}

printr() {
  while read -r line; do
      red "  $line"
  done <<< "$1"
}

check() {
  if test "$os" == "Darwin"; then
    local pass="✔︎"
    local warn="⦸"
    local fail="✗"
  else
    local pass="PASS "
    local warn="WARN "
    local fail="FAIL "
  fi

  local test=${1}
  local answ=${test/.ap/.ans}
  local name=${1/.ap/}
  local ast=0

  if test ! -e "$answ"; then
    local answ=${test/.ap/.ast}
    local ast=1
  fi

  if test ! -e "$answ"; then
    yellow "$warn $name" "(no answer file)"
    return 0
  fi

  if test ! -s "$answ"; then
    yellow "$warn $name" "(answer file empty)"
    return 0
  fi

  if test "$ast" -eq 1; then
    local interp=$(evaluate $test --ast)
  else
    local interp=$(evaluate $test)
  fi

  local answer=$(cat $answ)
  local result=$(compare "$interp" "$answer")
  local divider=$(printf '~%.0s' {1..68})

  if test -n "$result"; then
    red "$fail $name" "($test $answ)"
    if test $quiet -eq 0; then
      red
      red "  $divider"
      printr "$result"
      red "  $divider"
      red
    fi
    return 1
  else
    green "$pass $name" "($test $answ)"
    return 0
  fi
}

handle_flags() {
  case "$1" in
    -q|--quiet)
      quiet=1
      ;;

    -h|--help)
      usage
      exit 0
      ;;

    *)
      if test ! -z "$1"; then
        error "Unknown option: $1"
        exit 1
      fi
      ;;
  esac
}

change_dir_if_necessary() {
  local cwd=$(basename $(pwd))
  local test_dir="tests"

  if test ! "$cwd" = "$test_dir"; then
    cd "$test_dir"
  fi
}

run_tests() {
  local tests=$(ls *.ap)

  echo

  for test in $tests; do
    check $test
    if test $? -eq 1; then
      exit_status=1
    fi
  done

  echo
}

main() {
  handle_flags $@
  change_dir_if_necessary
  run_tests
}

main $@
exit $exit_status

