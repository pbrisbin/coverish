# coverish

Test coverage for shell scripts.

![](./files/rich.png)

Are you not testing your shell scripts? I recommend [cram](https://bitheap.org/cram/).

## Installation

1. Install [`stack`](https://docs.haskellstack.org/en/stable/README/)
1. Ensure `~/.local/bin` is on `$PATH`
1. Run

   ```console
   git clone https://github.com/pbrisbin/coverish
   cd coverish
   stack install
   ```

## Usage

1. Add `coverish-eval` as early as possible in your script:

   ```bash
   eval "$(coverish-eval bash)"
   ```

1. Wrap your tests in `coverish-exec`, passing `coverish` options if desired:

   ```console
   coverish-exec --format rich -- cram ./test
   ```

## Options

```console
Usage: coverish [-f|--format FORMAT] [-e|--exclude PATTERN]
                [-i|--include PATTERN] [-o|--output PATH] [PATH]

Available options:
  -f,--format FORMAT       Output in the given FORMAT
  -e,--exclude PATTERN     Exclude paths matching glob PATTERN
  -i,--include PATTERN     Re-include excluded paths matching glob PATTERN
  -o,--output PATH         Output to PATH (defaults to stdout)
  PATH                     Read from PATH (defaults to stdin)
  -h,--help                Show this help text
```

## Details

1. [`coverish-eval`](./app/Eval.hs) outputs a snippet of shell meant to be
   `eval`d in your shell script. It does the following:

   - Check for the presence of a `COVERISH_TRACE` variable, making this snippet
     safe to be present always: it does nothing if this variable is not defined
   - Redirect the output of `set -x` (execution trace) to this file
   - Set an explicit format for the trace (via `PS4`)
   - Enable `set -x`

   The only currently-supported argument is `bash`, which outputs a
   Bash-specific snippet. See the *Portability* section below for more details.

1. `coverish` parses the trace file produced during your test suite, then
   calculates and outputs coverage-related information in various formats.

1. For convenience, [`coverish-exec`](./app/Exec.hs) handles declaring a
   temporary file as `COVERISH_TRACE`, running your test suite, then invoking
   `coverish` on said temporary file.

## Portability Beyond Bash

To use `coverish`, a shell needs to support a custom `set -x` format (i.e. `PS4`)
which can include file and line information (e.g. `BASH_SOURCE` and `LINENO`).

To use `coverish-exec`, a shell needs to support redirecting `set -x` output to
a file (e.g. `BASH_XTRACEFD`).

If not redirecting the `set -x` output, you'll have to read the trace from
`stderr`. This may work, but if your test(s) or script(s) emit other content on
`stderr`, it will likely cause unexpected and incorrect behavior in coverish.

## Use with Code Climate Test Reporter

*Coming Soon*. `coverish --format json` is meant to be easily translatable to
the [Code Climate Test Reporter][test-reporter] format.

[test-reporter]: https://github.com/codeclimate/test-reporter

## Development & Testing

```console
stack build --pedantic
```

```console
stack test
```

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
