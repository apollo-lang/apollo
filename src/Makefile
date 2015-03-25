apollo: apollo.hs Parser.hs Tokens.hs
	ghc apollo.hs

Parser.hs: Parser.y
	happy Parser.y

Tokens.hs: Tokens.x
	alex Tokens.x

.PHONY:
clean:
	rm -f *.hi *.o core apollo Parser.hs Tokens.hs

# `make nice` removes all intermediate compiled files, save for `apollo` executable.
# Run it after `make` if you want your working directory to be nice and clean.

.PHONY:
nice:
	rm -f *.hi *.o core Parser.hs Tokens.hs

.PHONY:
all: clean apollo
