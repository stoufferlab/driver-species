
# make sure packrat is working
if(!packrat:::isPackratModeOn()) packrat::on()

require(magrittr)
require(foreach)
require(doMC)
registerDoMC(cores = 4)

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
  directed_networks = purrr::map(networks, as_directed_network, direction = "asymmetry", ties = "both", higher_level = "pol"),
  strings_in_dots = "literals"
)

example_plots_plan <- drake::drake_plan(
  en_structural = example_ntw_structural(),
  en_direction = example_ntw_directions(), 
  en_chain = example_ntw_matchings_chain(),
  en_star = example_ntw_matchings_star(), 
  fig_control_config = make_fig_control_configurations(en_chain, en_star, en_direction), 
  fig_struct_control = make_fig_structural_control(en_direction, en_structural),
  fig_supp_matching = make_fig_maximum_matching(en_direction), 
  fig_all_matching = make_fig_all_matchings(en_direction), 
  fig_small_network = make_small_network(networks),
  fig_input_graph = make_fig_input_graph(en_direction, en_star, en_chain), 
  fig_bidirectional = make_fig_bidirectional(en_chain), 
  fig_emp_controllability = make_fig_emp_contollability(controllability, randomisations_df, metadata), 
  fig_correlation = make_fig_correlation(sl_char_corr), 
  fig_control_capacity = make_fig_control_capacity(sigma_phi_df, species_empirical_coov, metadata)
)

control_capacity_testing_plan <- drake::drake_plan(
  control_input_pub_method = test_control_metrics(directed_networks, "input_graph", method = "published"),
  control_input_min_method = test_control_metrics(directed_networks, "input_graph", method = "mine"),
  control_lineg_method = test_control_metrics(directed_networks, "complementer", timeout = 60*60, exclude = "CD_ctl"),
  control_capacity_correlations = cc_correlations(control_input_pub_method, control_input_min_method, control_lineg_method),
  this_method = compare_cc_options(control_capacity_correlations, metadata),
  strings_in_dots = "literals"
)

controllability_plan <- drake::drake_plan(
  matched_networks = purrr::map(directed_networks, control_capacity_empirical_nets, l = aggregation_option_list, .method = this_method),
  controllability = controllability_emp(matched_networks),
  controllability_model_data = assemble_controllabillity_df(controllability, network_properties, metadata),
  controllability_models = fit_controllability_models(controllability_model_data),
  controllability_variable_importance = get_var_importance(controllability_models),
  random_directions = random_matching_sizes_emp(directed_networks, n = 99),
  random_interactions = random_matching_sizes_emp(networks, n = 99),
  randomisations_df = organise_randomisations(controllability, random_interactions, random_directions),
  strings_in_dots = "literals"
)

species_level_plan <- drake::drake_plan(
  species_empirical_coov = purrr::map_df(networks, plants_or_pols, .id = "net_name"),
  species_coovariates_df = purrr::map_df(directed_networks, get_species_coov, indices = c("degree", "species strength", "betweenness", "closeness"), .id = "net_name"), 
  sigma_phi_df = purrr::map_dfr(matched_networks, get_controllability_superiorness, .id = "net_name"),
  sl_characteristics = join_sl_characteristics(sigma_phi_df, species_coovariates_df),
  sl_char_corr = species_level_characteristics_correlation(sl_characteristics, metadata, method = "pearson"),
  secondary_ext = all_secondary_extinctions(networks, sl_characteristics),
  secondary_ext_std = standardize_secondary_extinctions(secondary_ext, controllability, metadata),
  strings_in_dots = "literals"
)

reporting_plan <- drake::drake_plan(
  render_pdf(drake::knitr_in('paper/supp-info.Rmd'), 
             drake::file_out('paper/supp-info.pdf'), clean_md = FALSE),
  render_pdf(drake::knitr_in('paper/manuscript.Rmd'), 
             drake::file_out('paper/manuscript.pdf'), clean_md = FALSE),
  render_pdf(drake::file_in('paper/reviewers-response.Rmd'), 
             drake::file_out('paper/reviewers-response.pdf'), clean_md = FALSE, clean_tex = FALSE), 
  compile_pdf(drake::file_in('paper/cover-letter.tex'), 
              drake::file_out('paper/cover-letter.pdf'))
)

project_plan <- drake::bind_plans(
  read_data_plan, basic_analysis_plan, example_plots_plan, 
  control_capacity_testing_plan, controllability_plan, species_level_plan, 
  reporting_plan)
project_config <- drake::drake_config(project_plan)
drake::make(project_plan, config = project_config)
