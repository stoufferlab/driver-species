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

doc: manuscript.docx

manuscript.md : manuscript.Rmd
	Rscript -e "knitr::knit('manuscript.Rmd')"
	
manuscript.docx : manuscript.md template.docx
	pandoc +RTS -K512m -RTS manuscript.md --to docx --from markdown+autolink_bare_uris+ascii_identifiers+tex_math_single_backslash-implicit_figures --output manuscript.docx --filter /usr/local/bin/pandoc-citeproc --highlight-style tango --bibliography references.bib --reference-docx template.docx
	