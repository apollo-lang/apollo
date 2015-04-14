LEX_SPEC   := Lex.x
PARSE_SPEC := Parse.y
LEXER      := $(LEX_SPEC:.x=.hs)
PARSER     := $(PARSE_SPEC:.y=.hs)
GENERATED  := $(LEXER) $(PARSER)

EXE     := apollo
MAIN    := Main.hs
GFLAGS  := -Wall
SOURCES := $(wildcard *.hs) $(LEX_SPEC) $(PARSE_SPEC)

.PHONY: all test clean clean-gen generated

all: $(EXE) clean-gen

test: $(EXE)
	@tests/run.sh

clean-gen:
	rm -f *.hi *.o core $(GENERATED)

clean: clean-gen
	rm -f $(EXE)

generated: $(LEXER) $(PARSER)

$(LEXER): $(LEX_SPEC)
	alex $(LEX_SPEC)

$(PARSER): $(PARSE_SPEC)
	happy $(PARSE_SPEC)

$(EXE): $(SOURCES)
	alex $(LEX_SPEC)
	happy $(PARSE_SPEC)
	ghc $(GFLAGS) $(MAIN) -o $@

