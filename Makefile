.PHONY: run_main

run_main:
	Rscript main.R

LOGFILE=draft_`date +'%F'`

draft:  
	zip -j $(LOGFILE) paper/*.pdf