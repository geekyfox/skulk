
.PHONY: test
test:
	cabal configure --enable-tests
	cabal test

coverage:
	cabal configure --enable-tests
	cabal configure --enable-coverage
	cabal test

docs:
	cabal haddock

clean:
	cabal clean

