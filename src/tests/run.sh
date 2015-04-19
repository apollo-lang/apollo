#!/usr/bin/env bash

## TODO: set timer with `ulimit`
## TODO: error on `compare` if both args not present
## TODO: make function for coloring a line

c_gre="\033[0;32m"
c_yel="\033[0;33m"
c_red="\033[0;31m"
c_def="\033[0m"

error=0
quiet=0
compile_error=0

usage() {
    echo ""
    echo "  Description: tests apollo against all available test cases"
    echo ""
    echo "  Usage: run [-qh]"
    echo ""
    echo "  Options:"
    echo ""
    echo "    -q, --quiet           supresses error messages"
    echo "    -h, --help            output help and usage"
    echo ""
}

evaluate() {
  ../apollo < ${1} 2> /dev/null
  
  if test $? -eq 1 ; then
    echo "Compilation error!"
  fi
}

compare() {
  diff -Bw <(echo "${1}") <(echo "${2}")
}

printr() {
  while read -r line; do
      printf "    $line\n"
  done <<< "$1"
}

check() {
  local test=${1}
  local answ=${2}
  local name=${1/.ap/}

  if test ! -e "$answ"; then
    echo -e "  ${c_yel}WARN ${c_def} $name (no answer file)"
    return 0
  fi

  if test ! -s "$answ"; then
    echo -e "  ${c_yel}WARN ${c_def} $name (answer file empty)"
    return 0
  fi

  local interp=$(evaluate $test)
  local answer=$(cat $answ)
  local result=$(compare "$interp" "$answer")
  local divider=$(printf '~%.0s' {1..68})

  if test -n "$result"; then
    echo -e "  ${c_red}FAIL ${c_def} $name ($test $answ)"
    if test $quiet -eq 0; then
      echo -e "${c_red}"
      echo -e "    $divider"
      printr "${result}"
      echo -e "    $divider"
      echo -e "${c_def}"
    fi
    return 1
  else
    echo -e "  ${c_gre}PASS ${c_def} $name ($test $answ)"
    return 0
  fi
}

main() {
  local arg="$1"
  shift

  case "${arg}" in
    # flags
    -q|--quiet)
      quiet=1
      ;;

    -h|--help)
      usage
      return 0
      ;;
  esac

  local cwd=$(basename $(pwd))
  local test_dir="tests"

  if test ! "$cwd" = "$test_dir"; then
    cd "$test_dir"
  fi

  local tests=$(ls *.ap)

  echo -e ""

  for test in $tests; do
    local answer=${test/.ap/.ans}
    check $test $answer
    if test $? -eq 1 ; then
      error=1
    fi
  done

  echo -e ""
}

if [[ ${BASH_SOURCE[0]} != $0 ]]; then
  export -f run-integrations
else
  main "${@}"
  exit $error
fi

