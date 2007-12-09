default: build

# Building

clean:
	runhaskell Setup.hs clean

configure:
	runhaskell Setup.hs configure

build: configure
	runhaskell Setup.hs build

haddock: configure
	runhaskell Setup.hs haddock

install: build haddock
	sudo runhaskell Setup.hs install

# switch off intermediate file deletion
.SECONDARY:

.PHONY: default configure build haddock install test



SOURCES=Object.hs Unsafe.hs Main.hs
run: Test
	./Test

Test: ${SOURCES}
	ghc -hide-all-packages -package base -package mtl -package witness -package open-witness -fglasgow-exts -o Test ${SOURCES}

Test-clean:
	rm -f *.o *.hi Test
