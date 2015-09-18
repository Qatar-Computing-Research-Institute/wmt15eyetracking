
LATEX = pdflatex
BIB = bibtex
MAIN = main
IND = makeindex
DATE= $$(date  +%a%d%h%H:%M)
all:
	@$(LATEX)	$(MAIN).tex
	@$(BIB) $(MAIN).aux
	@$(LATEX) $(MAIN).tex
pub:
	mkdir -p $(MAIN)-Draft
	cp $(MAIN).pdf $(MAIN)-Draft/$(MAIN)$(DATE).pdf
final:
	mkdir -p submitted
	cp $(MAIN).pdf submitted/$(MAIN)-submitted.pdf
