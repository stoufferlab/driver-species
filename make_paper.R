# Because the project started way before I adopted a workflow management
# strategy in R, and converting the scripts to adopt the workflow would require
# a bit too much work, this workflow is limited to building the publication
# files and the analyses added in the round of revisions from the Journal of
# Ecology submission

# LIBRARIES AND FUNCTIONS -------------------------------------------------

library(drake)
library(rmarkdown)
library(tinytex)

# required by paper or analysis
library(latex2exp)
library(igraph)
library(ggplot2)

# load functions
f <- list.files("./functions/", full.names = T) %>% lapply(source)

# DEFINE PLAN ----------------------------------------------------------------

ilustrations <- drake_plan(example_net = generate_example_network())

reporting <- drake_plan(
	# 'publication/supp_info.tex' = render('publication/supporting_information.Rmd', quiet = TRUE),
	# 'publication/supp_info.pdf' = latexmk('publication/supp_info.tex', clean = FALSE),
	'publication/manuscript.tex' = render('publication/manuscript.Rmd', quiet = TRUE),
	'publication/manuscript.pdf' = latexmk('publication/manuscript.tex', clean = FALSE),
	file_targets = TRUE
)


# MAKE --------------------------------------------------------------------

# set up plan
project_plan <- rbind(ilustrations, reporting)
project_config <- drake_config(project_plan)
vis_drake_graph(project_config, split_columns = T, targets_only = T)

# execute plan
# make(project_plan, parallelism = "parLapply", jobs = 7)
make(project_plan, graph = project_config$graph, config = project_config, session_info = T)

