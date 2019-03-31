
all: Integrals.pdf

clean:
	rm -rf *.pdf

.PHONY: all clean

OUTDIR=.

$(OUTDIR)/%.pdf: %.html
	wkhtmltopdf -O Landscape -s A5 --javascript-delay 5000 $< $@
