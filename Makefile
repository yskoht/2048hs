build:
	@stack build

run: build
	@stack exec -- 2048

install:
	stack install --local-bin-path=./bin
