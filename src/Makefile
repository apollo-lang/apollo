LEX_SPEC   := Lex.x
PARSE_SPEC := Parse.y
LEXER      := $(LEX_SPEC:.x=.hs)
PARSER     := $(PARSE_SPEC:.y=.hs)
GENERATED  := $(LEXER) $(PARSER)

EXE     := apollo
MAIN    := Main.hs
GFLAGS  := -Wall
SOURCES := $(wildcard *.hs) $(LEX_SPEC) $(PARSE_SPEC)

.PHONY: all test clean generated

all: $(EXE)

test: $(EXE)
	@tests/run.sh

clean:
	rm -rf tmp/
	rm -f $(GENERATED)
	rm -f $(EXE)

generated: $(LEXER) $(PARSER)

$(LEXER): $(LEX_SPEC)
	alex $< --outfile=$@

$(PARSER): $(PARSE_SPEC)
	happy $< --outfile=$@

$(EXE): $(SOURCES)
	ghc $(GFLAGS) --make $(MAIN) -o $@ -outputdir tmp/

