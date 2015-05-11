## Flags for Pandoc:

FLAGS = --standalone -fmarkdown-implicit_figures --table-of-contents

## All markdown files in working directory:

SRC = $(wildcard *.md)

## All PDF targets:

PDFS=$(SRC:.md=.pdf)

all:	$(PDFS)

%.pdf: %.md
	pandoc --from=markdown --toc-depth=2 --latex-engine=pdflatex $(FLAGS) --output=$@ $<

.PHONY: all

