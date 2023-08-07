# Quake Log Parser

## Dependencies
To build and run this project you will need a proper ocaml compiler with dune installed, you can check it out at the official ocaml docs: [https://ocaml.org/docs/up-and-running](https://ocaml.org/docs/up-and-running)

After installing the ocaml toolchain, you need to install this dependencies with opam
```shell
opam install angstrom batteries ppx_inline_test ppx_compare
```

## Running the project
If you don't wanna install anything, you can just run it in docker with the following commands:
```shell
docker buildx build . -t quake-log-parser
docker run quake-log-parser
```

to run it natively, you can just run
```shell
dune exec bin/main.exe
```
and to run the unit tests you shall use
```shell
dune build @install @runtest
```

## Additional notes
Since you can't extract from the logs unique player identifiers(some kind of UUID) within the scope outside a match, you can't exacly tell which player executed the event, resulting to a falsy report. with this limitations, this program tries to get the most acurate information as possible.

### Exemple where this can result into a falsy evaluation
- player 1 connects to the game
- player 1 quits
- player 1 changes the nickname outside the match
- player 1 reconnects and receives a different identifier (in the log, identifiers are just simple integers given to the player based on the order they have joined the match, so they can changee on reconnect)

Now a player that has some history in the match, is being identified as a different player, since we can't properly identify them.

### Future considerations
1. the `ShutdownGame` event may not be needed, so we may consider its removal
2. parsers may need performance tests and improvements
