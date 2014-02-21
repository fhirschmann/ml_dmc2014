TEXFLAGS = -e '$$pdflatex=q/pdflatex %O -shell-escape %S/' -pdf

doc.pdf: doc.tex
	latexmk $(TEXFLAGS) doc.tex
	latexmk -c doc.tex

doc.tex: doc.Rnw
	Rscript -e "library(knitr); knit('doc.Rnw')"
