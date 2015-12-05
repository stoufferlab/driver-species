all: manuscript.pdf

manuscript.tex : manuscript.Rnw
	R CMD Sweave manuscript.Rnw; R CMD Stangle manuscript.Rnw
	
manuscript.pdf : manuscript.tex
	R CMD pdflatex manuscript.tex; biber manuscript; R CMD pdflatex manuscript.tex; make clean
	
clean:
	rm *.aux; rm *.log; rm *.bcf; rm *.out; rm *.xml; rm *.blg; rm *.bbl
	
