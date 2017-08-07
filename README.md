# coverish

Test coverage for shell scripts.

## Installation

1. Install [`stack`](https://docs.haskellstack.org/en/stable/README/)
1. Ensure `~/.local/bin` is on `$PATH`
1. Run

   ```console
   git clone https://github.com/pbrisbin/coverish
   cd coverish
   stack install
   ```

## Usage (Bash)

1. Add something like the following as early as possible in your script:

   ```bash
   if [ -n "$COVERAGE" ]; then
     exec 9>>"$COVERAGE"
     export BASH_XTRACEFD=9
     export PS4='_coverage:$BASH_SOURCE:$LINENO:'
     set -x
   fi
   ```

1. Run your tests

   Are you not testing your shell scripts? I recommend [cram](https://bitheap.org/cram/).

   ```console
   COVERAGE=/tmp/coverage.txt cram ./test
   ```

1. Run `coverish` on the output

   ```console
   coverish < /tmp/coverage.txt | less -R
   ```

## Usage (Dash)

*TODO*

## Usage (ZSH)

*TODO*

## Usage (POSIX sh)

Probably impossible.
