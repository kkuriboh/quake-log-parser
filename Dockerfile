FROM ocaml/opam:alpine

WORKDIR /home/opam
COPY . .

USER root:root
# RUN apk update && apk add dune
RUN chown -R opam:opam /home/opam

USER opam:opam

RUN opam install angstrom batteries ppx_inline_test ppx_compare dune
RUN eval $(opam env) && dune build @install @runtest && dune build

ENTRYPOINT ["./_build/default/bin/main.exe"]
