HC := ghc

midi: 
	$(HC) -i../src testM.hs
	./testM
	rm -f *.hi *.o testM

clean:
	rm -f *.hi *.o *.mid testM

.PHONY: all
all: clean midi
