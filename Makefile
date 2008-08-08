default: build

# Building

clean:
	cabal clean

configure:
	cabal configure

build: configure
	cabal build --ghc-options="-Wall -Werror"

haddock: configure
	cabal haddock

install: build haddock
	cabal install --user

sdist: configure
	cabal sdist

# switch off intermediate file deletion
.SECONDARY:

.PHONY: default configure build haddock install test sdist



SOURCES=Object.hs Unsafe.hs Main.hs
run: Test
	./Test

Test: ${SOURCES}
	ghc -hide-all-packages -package base -package mtl -package witness -package open-witness -fglasgow-exts -o Test ${SOURCES}

Test-clean:
	rm -f *.o *.hi Test
