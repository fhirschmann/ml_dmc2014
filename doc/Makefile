TEXFLAGS = -e '$$pdflatex=q/pdflatex %O -shell-escape %S/' -pdf

all: doc.tex doc.pdf pres1.tex pres1.pdf analyze_dmc2010.html

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

analyze_dmc2010.html: analyze_dmc2010.Rmd
	Rscript -e "library(knitr); knit2html('analyze_dmc2010.Rmd')"

notes.html: notes.Rmd
	Rscript -e "library(knitr); knit2html('notes.Rmd')"

clean:
	rm *.tex
	rm *.pdf
