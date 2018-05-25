.PHONY: run_main

run_main:
	Rscript main.R

LOGFILE=sumbission_`date +'%F'`

submision:  
	zip -j $(LOGFILE) paper/*.pdf