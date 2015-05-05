EXE     := apollo
SOURCES := $(wildcard src/*)

.PHONY: all
all: $(EXE)

.PHONY: config
config:
	cabal sandbox init

.PHONY: test
test: $(EXE)
	@./tests/run.sh

$(EXE): $(SOURCES)
	cabal build
	ln -s -f ./dist/build/$(EXE)/$(EXE) ./$(EXE)

