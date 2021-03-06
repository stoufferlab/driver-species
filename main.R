
# make sure packrat is working
# if(!packrat:::isPackratModeOn()) packrat::on()

require(magrittr)
require(foreach)
require(doMC)
registerDoMC(cores = 1)

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
  proportion_bilinks = get_prop_bilinks(networks, directed_networks),
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
  fig_emp_controllability = make_fig_emp_contollability(controllability, randomisations_df, metadata, controllability_models, controllability_model_data, network_properties), 
  fig_correlation = make_fig_correlation(sl_char_corr), 
  fig_control_capacity = make_fig_control_capacity(sl_characteristics),
  fig_superior = make_fig_superior(species_model_superior),
  fig_structural_stability = make_fig_structural_stability(critical_sp_df), 
  fig_models_degree = make_fig_models_degree(species_model_cc, species_model_superior),
  fig_rho_sensitivity = make_fig_rho_sensitivity(structural_rho_correlation, rho_feasibility),
  fig_assumption_sampling = make_fig_assumption_subsampling(metrics_subsampled, metadata), 
  fig_species_partial = make_fig_species_partial(species_model_cc), 
  fig_asymmetry_distribution = make_fig_asymmetry_dist(directed_networks)
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
  randomisations_test = test_randomisations(randomisations_df),
  strings_in_dots = "literals"
)

species_level_plan <- drake::drake_plan(
  species_empirical_coov = purrr::map_df(networks, plants_or_pols, .id = "net_name"),
  species_coovariates_df = purrr::map2_df(directed_networks, networks, get_species_coov, metrics = c("degree", "species strength", "betweenness", "closeness", "eigen", "page_rank", "nested_contribution", "interaction push pull"), .id = "net_name"), 
  chosen_rho = sensitivity_range$rho[round(length(sensitivity_range$rho))],
  chosen_delta = sensitivity_range$delta[1],
  structural_stability = get_all_struct(directed_networks, rho = chosen_rho, delta = chosen_delta), 
  sigma_phi_df = purrr::map_dfr(matched_networks, get_controllability_superiorness, .id = "net_name"),
  sl_characteristics = join_sl_characteristics(sigma_phi_df, species_coovariates_df, species_empirical_coov, structural_stability),
  rho_feasibility = get_rho_feasibility(sigma_phi_df, species_coovariates_df, species_empirical_coov,structural_rho_sensitivity, sensitivity_range),
  sl_char_corr = species_level_characteristics_correlation(sl_characteristics, metadata, vars = c("control_capacity", "superior", "page_rank.nondirected", "eigen.nondirected", "degree", "betweenness", "closeness"), method = "spearman"),
  secondary_ext = all_secondary_extinctions(networks, sl_characteristics),
  secondary_ext_std = standardize_secondary_extinctions(secondary_ext, controllability, metadata),
  species_model_superior = fit_species_models(sl_characteristics, metadata, response = "superior"), 
  species_model_cc = fit_species_models(sl_characteristics, metadata, response = "control_capacity"),
  critical_sp_df = extract_critical_sp_df(sl_characteristics, metadata),
  strings_in_dots = "literals"
)

test_assumption_plan <- drake::drake_plan(
  sensitivity_range = list(rho = seq(0.001, 0.01, length.out = 10), delta = seq(0, 0.5, length.out = 3)),
  structural_rho_sensitivity = get_structural_rho_sensitivity(sensitivity_range, directed_networks),
  structural_rho_correlation = get_structural_sensitivity_correlation(sensitivity_range, structural_rho_sensitivity, chosen_rho, metadata),
  visitation_importance_agreement = get_visitation_importance_agreement(sl_characteristics, metadata),
  metrics_subsampled = subsample_nets(networks, from = 0, to = 0.2, by = 0.01, this_method, bias = "none"), 
  strings_in_dots = "literals"
)

export_figure_data_plan <- drake::drake_plan(
  fig_emp_controllability_data = get_fig_emp_controllability_data(controllability, randomisations_df, metadata, controllability_models, controllability_model_data, network_properties), 
  saveRDS(object = fig_emp_controllability_data, 
          file = drake::file_out("data/processed/plot_data/fig_emp_controllaibility.rds"), 
          ascii = TRUE, compress = FALSE), 
  table_model_selection_data = get_table_model_selection_data(species_model_cc), 
  saveRDS(object = table_model_selection_data, 
          file = drake::file_out("data/processed/plot_data/tab_model_selection.rds"), 
          ascii = TRUE, compress = FALSE), 
  saveRDS(object = sl_characteristics, 
          file = drake::file_out("data/processed/plot_data/fig_control_capacity.rds"), 
          ascii = TRUE, compress = FALSE),
  saveRDS(object = metadata, 
          file = drake::file_out("data/processed/plot_data/metadata.rds"), 
          ascii = TRUE, compress = FALSE), 
  fig_species_partial_data = get_predictions_df_species_level(species_model_cc), 
  saveRDS(object = fig_species_partial_data, 
          file = drake::file_out("data/processed/plot_data/fig_partial_effects_control_capacity.rds"), 
          ascii = TRUE, compress = FALSE), 
  saveRDS(object = critical_sp_df, 
          file = drake::file_out("data/processed/plot_data/fig_structural_stability.rds"), 
          ascii = TRUE, compress = FALSE)
)

reporting_plan <- drake::drake_plan(
  render_pdf(drake::knitr_in('paper/supp-info.Rmd'), 
             drake::file_out('paper/supp-info.pdf'), clean_md = FALSE),
  render_pdf(drake::knitr_in('paper/manuscript.Rmd'), 
             drake::file_out('paper/manuscript.pdf'), clean_md = FALSE),
  render_pdf(drake::knitr_in('paper/reviewers-response.Rmd'),
             drake::file_out('paper/reviewers-response.pdf'), clean_md = FALSE, clean_tex = FALSE), 
  compile_pdf(drake::file_in('paper/cover-letter.tex'), 
              drake::file_out('paper/cover-letter.pdf')), 
  fig_graphical_abstract(en_direction, drake::file_out("paper/graphical-abstract.png")),
  fig_graphical_abstract(en_direction, drake::file_out("paper/graphical-abstract-small.png"), res = 150)
)

project_plan <- drake::bind_plans(
  read_data_plan, basic_analysis_plan, example_plots_plan, 
  control_capacity_testing_plan, controllability_plan, species_level_plan, 
  test_assumption_plan,
  export_figure_data_plan,
  reporting_plan)
project_config <- drake::drake_config(project_plan)
drake::make(project_plan, config = project_config)
