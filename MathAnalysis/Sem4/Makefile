
all: Integrals.pdf

clean:
	rm -rf *.pdf

.PHONY: all clean

OUTDIR=.

$(OUTDIR)/%.pdf: %.html
	wkhtmltopdf --javascript-delay 5000 $< $@
