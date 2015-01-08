SOURCES := $(shell find . -name '*.Rpres')
SOURCES := $(patsubst %.Rpres,%,$(SOURCES))

knit:
	for src in $(SOURCES) ; do \
		echo knitting $$src.Rpres ; \
		Rscript -e "library(knitr); knit('$$src.Rpres', '$$src.md', quiet=T)" ; \
	done
tangle:
	for src in $(SOURCES) ; do \
		echo tangling $$src.Rpres ; \
		Rscript -e "library(knitr); purl('$$src.Rpres', quiet=T, documentation=2)" ; \
	done

html: knit
	for src in $(SOURCES) ; do \
		echo compiling $$src.Rpres to html ; \
		pandoc -s -S -i -t dzslides --mathjax $$src.md -o $$src.html ; \
	done
pdf: knit
	for src in $(SOURCES) ; do \
		echo compiling $$src.Rpres to pdf ; \
	done
all: knit tangle html pdf
clean:
	rm -vf *.md
	rm -vf *.R
	rm -vf *.html
	rm -vf *-figure/*
	rm -vf figure/*
