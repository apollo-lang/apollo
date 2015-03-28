LEX_SPEC   := Lexer.x
PARSE_SPEC := Parser.y
LEXER      := $(LEX_SPEC:.x=.hs)
PARSER     := $(PARSE_SPEC:.y=.hs)
GENERATED  := $(LEXER) $(PARSER)

EXE     := apollo
MAIN    := Main.hs
GFLAGS  := -Wall
SOURCES := $(wildcard *.hs) $(GENERATED)

.PHONY: all clean clean-gen generated

all: $(EXE) clean-gen

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
	ghc $(GFLAGS) $(MAIN) -o $@

