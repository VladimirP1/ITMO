
all: Defs.html

clean:
	rm -rf *.html

.PHONY: all clean

OUTDIR=.
BUILDDIR=build

$(BUILDDIR)/%._html: %.md
	markdown $< > $@

$(OUTDIR)/%.html: $(BUILDDIR)/%._html
	cat utility/__header.html $< utility/__footer.html > $@
