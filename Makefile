all:
	@cabal sandbox init
	@cabal install --only-dependencies
	@cabal build

run:
	@cabal --verbose=0 run

name:
	@echo "11001011"
