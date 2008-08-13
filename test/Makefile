default: run

# Building

clean:
	cabal clean

configure:
	cabal configure

build: configure
	cabal build --ghc-options="-Wall -Werror"

# switch off intermediate file deletion
.SECONDARY:

.PHONY: default configure build

run: build
	dist/build/Test/Test

