
## Flags for Pandoc:

FLAGS = -fmarkdown-implicit_figures --toc

## All markdown files in working directory:

SRC = $(wildcard *.md)

## All PDF targets:

PDFS=$(SRC:.md=.pdf)

all:	$(PDFS)

%.pdf: %.md
	pandoc -r markdown --latex-engine=pdflatex $(FLAGS) -o $@ $<

.PHONY: all

