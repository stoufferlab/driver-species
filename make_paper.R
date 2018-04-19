# Because the project started way before I adopted a workflow management
# strategy in R, and converting the scripts to adopt the workflow would require
# a bit too much work, this workflow is limited to building the publication
# files and the analyses added in the round of revisions from the Journal of
# Ecology submission

# LIBRARIES AND FUNCTIONS -------------------------------------------------

# make sure packrat is working
if(!packrat:::isPackratModeOn()) packrat::on()

library(drake)
library(rmarkdown)
library(tinytex)

# required by paper or analysis
library(readr)
library(dplyr)
library(igraph)
library(ggplot2)
library(latex2exp)


# load functions
f <- list.files("functions", full.names = T) %>% lapply(source)

# DEFINE PLAN ----------------------------------------------------------------

read_data_plan <- drake_plan(
	metadata = read_metadata('data/ntw_info.csv'),
	strings_in_dots = 'filenames'
)

# ilustrations_plan <- drake_plan(example_net = generate_example_network())

reporting_plan <- drake_plan(
	'publication/supp-info.tex' = render('publication/supp-info.Rmd', quiet = TRUE),
	'publication/supp-info.pdf' = latexmk('publication/supp-info.tex', clean = FALSE),
	'publication/manuscript.tex' = render('publication/manuscript.Rmd', quiet = TRUE),
	'publication/manuscript.pdf' = latexmk('publication/manuscript.tex', clean = FALSE),
	file_targets = TRUE
)


# MAKE --------------------------------------------------------------------

# set up plan
project_plan <- rbind(read_data_plan, reporting_plan)
project_config <- drake_config(project_plan)
vis_drake_graph(project_config, split_columns = T, targets_only = T)

# execute plan
# make(project_plan, parallelism = "parLapply", jobs = 7)
make(project_plan, graph = project_config$graph, config = project_config, session_info = T)

