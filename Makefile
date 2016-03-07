.PHONY: build
build:
	stack --jobs 2 setup
	stack --jobs 2 build

.PHONY: install
install:
	stack install

.PHONY: test
test:
	@./tests/run.sh

