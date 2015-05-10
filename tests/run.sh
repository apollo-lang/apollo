#!/usr/bin/env bash

os=$(uname -s)
indent="  "
quiet=0
exit_status=0
compile_error=0

# Several functions to print strings with colors
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

# Simple help interface
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

# Eval for normal AP files: redirects the file into apollo
evaluate() {
  # Stop execution exceeding 10 seconds to prevent infinite loops
  ulimit -t 10

  ../apollo $2 < "$1" 2> /dev/null

  # if Apollo exits with 1 something severe happened!
  if test $? -eq 1; then
    echo "<compilation error>"
  fi
}

# Eval for APS: it echos the line to be tested and pipes it into apollo
evalAPS() {
  # Stop execution exceeding 10 seconds to prevent infinite loops
  ulimit -t 10

  echo "$1" | ../apollo - $2 2> /dev/null

  if test $? -eq 1; then
    echo "<compilation error>"
  fi
}

# Function that runs diff and compares the apollo result against the expected result
compare() {
  diff -Bw <(echo "${1}") <(echo "${2}")
}

printr() {
  while read -r line; do
      red "  $line"
  done <<< "$1"
}

# The main function that deals with checking files and treating them according to their extensions:
# .ap: normal Apollo source file
# .aps: source file where each input line must be tested separately
# .ans: normal answer file (file containing the expected answer)
# .ast: file containing a program's AST instead of the normal answer
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
  local ast=0
  local aps=0
  if [[ $test == *".aps" ]]; then
    local answ=${test/.aps/.ans}
    local name=${1/.aps/}
    if test ! -e "$answ"; then
      # If in .ast mode (the answer file has an extension .ast) we compare the source against its AST
      local answ=${test/.aps/.ast}
      local ast=1
    fi
    # Flag sets .aps mode
    local aps=1
  else
    local answ=${test/.ap/.ans}
    local name=${1/.ap/}
    if test ! -e "$answ"; then
      local answ=${test/.ap/.ast}
      local ast=1
    fi
  fi

  if test ! -e "$answ"; then
    yellow "$warn $name" "(no answer file)"
    return 0
  fi

  if test $aps -eq 1; then
    # In .aps mode, each line is read and interpreted individually.
    local lineno=0
    while read -r line; do
      if test -z "$line"; then
        # If a line in the test file is empty, it is ignored (as well as the respective line in the ans file)
        let lineno=lineno+1
        continue
      fi
      # Keep track of line numbers to extract them from the answer file
      let lineno=lineno+1

      if test "$ast" -eq 1; then
        local interp=$(evalAPS "$line" --ast)
      else
        local interp=$(evalAPS "$line")
      fi

      local answer=$(awk "NR==$lineno" "$answ") # awk helps us grab the appropriate line from the ans file
      local result=$(compare "$interp" "$answer")
      if test -n "$result"; then
      # If one of the lines generates an error, that test stops and no further lines are tested
        break
      fi
    done < "$test"
  else
    # Much simpler testing when we deal with the whole source file
    if test "$ast" -eq 1; then
      local interp=$(evaluate $test --ast)
    else
      local interp=$(evaluate $test -)
    fi

    local answer=$(cat $answ)
    local result=$(compare "$interp" "$answer")
  fi

  local divider=$(printf '~%.0s' {1..68})

  if test -n "$result"; then
    red "$fail $name" "($test $answ)"
    if test $quiet -eq 0; then
    # If the script is run with -q (quiet) the error message is not printed
      red
      if test $aps -eq 1; then
        # If we're running in aps mode, we print the line where the error happened
        red "  Error in line: $lineno"
      fi
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

# cd to test directory if not already there
change_dir_if_necessary() {
  local cwd=$(basename $(pwd))
  local test_dir="tests"

  if test ! "$cwd" = "$test_dir"; then
    cd "$test_dir"
  fi
}

# Function that lists files with appropriate extensions in tests directory and send them to be processed
run_tests() {
  local tests=$(ls *.ap *.aps)
  echo

  for test in $tests; do
    check $test
    if test $? -eq 1; then
      # If the exit status is ever != 0, we store it so that we can later exit the script in error
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


