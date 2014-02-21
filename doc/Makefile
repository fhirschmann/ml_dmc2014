TEXFLAGS = -e '$$pdflatex=q/pdflatex %O -shell-escape %S/' -pdf

doc.pdf: doc.tex
	latexmk $(TEXFLAGS) doc.tex
	latexmk -c doc.tex

doc.tex: doc.Rnw
	Rscript -e "library(knitr); knit('doc.Rnw')"

pres1.pdf: pres1.tex
	latexmk $(TEXFLAGS) pres1.tex
	latexmk -c pres1.tex

pres1.tex: pres1.Rnw
	Rscript -e "library(knitr); knit('pres1.Rnw')"
