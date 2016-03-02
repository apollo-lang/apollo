.PHONY: all
all: build

# TODO uninstall command
.PHONY: install
install:
	stack install

.PHONY: test
test: $(EXE)
	@./tests/run.sh

build:
	stack setup
	stack build

