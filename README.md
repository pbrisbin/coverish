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

*TODO*

## Portability Beyond Bash

*TODO*

## Use with Code Climate Test Reporter

*TODO*

## Development & Testing

```console
stack build --pedantic
```

```console
stack test
```

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
