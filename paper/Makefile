all: paper clean

paper: paper.pdf

paper.pdf: manuscript.pdf supp_info.pdf
	pdftk $^ cat output $@

manuscript.pdf: manuscript.tex
	pdflatex -interaction nonstopmode $<; \
	bibtex manuscript; \
	pdflatex -interaction nonstopmode $<; \
	bibtex manuscript; \
	pdflatex -interaction nonstopmode $<

manuscript.tex: manuscript.Rnw
	Rscript -e "knitr::knit('manuscript.Rnw')"

supp_info.pdf: supp_info.tex
	pdflatex -interaction nonstopmode $<; \
	bibtex supp_info; \
	pdflatex -interaction nonstopmode $<; \
	bibtex supp_info; \
	pdflatex -interaction nonstopmode $<

supp_info.tex: supp_info.Rnw
	Rscript -e "knitr::knit('supp_info.Rnw')"

clean:
	rm -fv *.aux *.fls *.log *blx.bib *.xml *.blg *.bbl *.fdb_latexmk *.bcf *.out
