OUTDIR=.
BUILDDIR=build

all: html

.PHONY: html all clean

html: Defs.html

clean:
	rm -rf *.html

$(BUILDDIR)/%._html: %.md
	mkdir -p $(BUILDDIR)
	markdown $< > $@

$(OUTDIR)/%.html: $(BUILDDIR)/%._html
	cat utility/__header.html $< utility/__footer.html > $@

install: html
	mkdir -p $(PREFIX)
	cp $(OUTDIR)/*.html $(PREFIX)

