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



#SOURCES=SameType.hs Witness.hs UniqueWitness.hs UniqueDict.hs ST.hs Object.hs Typeable.hs Dynamic.hs Unsafe.hs Main.hs

#run: Test
#	./Test

#Test: ${SOURCES}
#	ghc -package mtl -package witness -o Test ${SOURCES}

#clean:
#	rm -f *.o *.hi Test
