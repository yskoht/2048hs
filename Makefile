build:
	@stack build

run: build
	@stack exec -- 2048

install:
	stack install --local-bin-path=./bin

docker-build:
	docker image build -t yskoht/2048hs:0.1.0.0 .
