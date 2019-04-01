OUTDIR=.

all: pdf
.PHONY: pdf install all clean

pdf: $(OBJECTS)

clean:
	rm -rf *.pdf

install: pdf
	mkdir -p $(PREFIX)
	cp $(OUTDIR)/*.pdf $(PREFIX)


$(OUTDIR)/%.pdf: %.html
	wkhtmltopdf -O Landscape -s A5 --javascript-delay 5000 $< $@
