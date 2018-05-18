
# make sure packrat is working
if(!packrat:::isPackratModeOn()) packrat::on()

require(magrittr)

# load functions
f <- lapply(list.files("code", full.names = T), source)

read_data_plan <- drake::drake_plan(
  metadata = read_metadata(drake::file_in("data/ntw_info.csv")),
  bartomeus_networks = preprocess_bartomeus(drake::file_in("data/raw/Bartomeus_Ntw_nceas.txt")),
  lopezaraiza_networks = preprocess_lopezaraiza(
    drake::file_in("data/raw/networks_lopezaraiza/AC ctl a.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/AC ctl b.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/AC exp a.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/AC exp b.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/CD ctl a.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/CD ctl b.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/CD exp a.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/CD exp b.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/RF ctl a.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/RF ctl b.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/RF exp a.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/RF exp b.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/SA ctl a.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/SA ctl b.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/SA exp a.txt"),
    drake::file_in("data/raw/networks_lopezaraiza/SA exp b.txt")),
  ballantyne_networks = preprocess_ballantyne(
    drake::file_in("data/raw/Ballantyne-2015-raw_data.csv")),
  networks = c(bartomeus_networks, lopezaraiza_networks, ballantyne_networks),
  strings_in_dots = "literals"
)

basic_analysis_plan <- drake::drake_plan(
  network_properties = calc_ntws_properties(networks, properties = c(
    "connectance", 
    "web asymmetry", 
    "ISA", 
    "weighted NODF")), 
  strings_in_dots = "literals"
)

example_plots_plan <- drake::drake_plan(
  en_structural = example_ntw_structural(),
  en_direction = example_ntw_directions(), 
  en_chain = example_ntw_matchings_chain(),
  en_star = example_ntw_matchings_star(), 
  fig_control_config = make_fig_control_configurations(en_chain, en_star, en_direction), 
  fig_struct_control = make_fig_structural_control(en_direction, en_structural)
)

reporting_plan <- drake::drake_plan(
  render_pdf(drake::knitr_in('paper/supp-info.Rmd'), 
             drake::file_out('paper/supp-info.pdf'), clean_md = FALSE),
  render_pdf(drake::knitr_in('paper/manuscript.Rmd'), 
             drake::file_out('paper/manuscript.pdf'), clean_md = FALSE)
)

project_plan <- rbind(read_data_plan, basic_analysis_plan, example_plots_plan, reporting_plan)
project_config <- drake::drake_config(project_plan)
drake::make(project_plan, config = project_config)

