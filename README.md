This is a chess-engine implementation in swi-prolog. 

## How to run

```bash
swipl -t halt -f -q -O ./src/main.pl -- {pgn_file}
```

will predict the next best move for the given pgn file. For example:

```bash
swipl -t halt -f -q -O ./src/main.pl -- src/fool.pgn
```

---

```bash
swipl -t halt -f -q -O src/main.pl -- {pgn_file} TEST
```

will output all possible moves in the last position of the given pgn file. For example:

```bash
swipl -t halt -f -q -O src/main.pl -- tests/examples/london.pgn TEST
```