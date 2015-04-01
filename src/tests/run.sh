#!/usr/bin/env bash

## TODO: return proper exit status (0/1)
## TODO: set timer with `ulimit`
## TODO: error on `compare` if both args not present

c_gre="\033[0;32m"
c_red="\033[0;31m"
c_def="\033[0m"

evaluate() {
  cat ${1} | ../apollo
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

  local interp=$(evaluate $test)
  local answer=$(cat $answ)
  local result=$(compare "$interp" "$answer")
  local divider=$(printf '~%.0s' {1..68})

  if test -n "$result"; then
    echo -e "  ${c_red}✗ $name ($test $answ)${c_def}"
    echo -e "${c_red}"
    echo -e "    $divider"
    printr "${result}"
    echo -e "    $divider"
    echo -e "${c_def}"
  else
    echo -e "  ${c_gre}✔︎ $name ($test $answ)${c_def}"
  fi
}

main() {
  local tests="*.ap"

  echo -e ""

  for test in $tests
  do
    local answer=${test/.ap/.ans}
    check $test $answer
  done

  echo -e ""
}

if [[ ${BASH_SOURCE[0]} != $0 ]]; then
  export -f run-integrations
else
  main "${@}"
  exit $?
fi

