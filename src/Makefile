apollo: apollo.hs Parser.hs Tokens.hs
	ghc apollo.hs

Parser.hs: Parser.y
	happy Parser.y

Tokens.hs: Tokens.x
	alex Tokens.x

.PHONY:
clean:
	rm -f *.hi *.o core apollo Parser.hs Tokens.hs

.PHONY:
all: clean apollo
