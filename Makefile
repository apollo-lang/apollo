EXE     := apollo
BUILD   := ./dist/build/$(EXE)
SOURCES := $(wildcard src/*)
PREFIX  ?= /usr/local


.PHONY: all
all: $(EXE)

.PHONY: config
config:
	cabal sandbox init
	cabal install --only-dependencies

.PHONY: install
install: $(EXE)
	cp $(BUILD)/$(EXE) $(PREFIX)/bin/$(EXE)

.PHONY: uninstall
uninstall:
	rm -f $(PREFIX)/bin/$(EXE)

.PHONY: test
test: $(EXE)
	@./tests/run.sh

$(EXE): $(SOURCES)
	cabal build
	ln -s -f $(BUILD)/$(EXE) ./$(EXE)

