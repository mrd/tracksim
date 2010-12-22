HC = ghc
PDFLATEX = pdflatex

.PHONY: all
all: Main TravelTime.pdf

Main: Main.hs
	$(HC) --make $<

TravelTime.pdf: TravelTime.lhs
	$(PDFLATEX) $<

.PHONY: clean
clean:
	rm -f TravelTime.pdf TravelTime.log TravelTime.aux Main *.o *.hi
