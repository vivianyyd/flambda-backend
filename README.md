# The Flambda backend project for OCaml

This repository is for more experimental work, of production quality, on the OCaml compiler.
This is also the home of the Flambda 2 optimiser.

The following gives basic instructions for getting set up.  Please see
[`HACKING.md`](HACKING.md) for more detailed instructions if you want to develop in this repo.

## One-time setup for dev work or installation

Only currently tested on Linux/x86-64 and macOS/x86-64.

One-time setup:
```
$ opam switch 4.11.1  # or "opam switch create 4.11.1" if you haven't got that switch already
$ eval $(opam env)
$ git clone https://github.com/ocaml-flambda/dune
$ cd dune  # We'll refer to this "dune" directory below as $DUNE_DIR
$ git checkout origin/special_dune
$ make release
```

You probably then want to fork the `ocaml-flambda/flambda-backend` repo to your own Github org.

## Branching and configuring

Use normal commands to make a branch from the main upstream branch (currently `4.11`), e.g.:
```
$ git clone https://github.com/ocaml-flambda/flambda-backend
$ cd flambda-backend
$ git checkout -b myfeature origin/4.11
```

The Flambda backend tree has to be configured before building.  The configure script is not checked
in; you have to run `autoconf`.  For example:
```
$ autoconf
$ ./configure --prefix=/path/to/install/dir --enable-middle-end=closure --with-dune=$DUNE_DIR/dune.exe
```
You can also specify `--enable-middle-end=flambda` or `--enable-middle-end=flambda2`.  (The Flambda 2
compiler is not yet ready for production use.)

## Building and installing

To build and install the Flambda backend, which produces a compiler installation directory whose
layout is compatible with upstream, run:
```
$ make  # or e.g. make -j16
$ make install
```
