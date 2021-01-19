# setwd("C:/use/GitHub/ssanchezAZTI/WKDLSSLS_paperFRONTIERS")

# Generate parameters for alternative life histories
source(file.path("R","01_lifeHistoryTraits_STK1_bc.R"), echo=TRUE)
source(file.path("R","01_lifeHistoryTraits_STK2_bc.R"), echo=TRUE)
source(file.path("R","01_lifeHistoryTraits_STK1_lowprod.R"), echo=TRUE)
source(file.path("R","01_lifeHistoryTraits_STK1_highprod.R"), echo=TRUE)
source(file.path("R","01_lifeHistoryTraits_STK2_lowprod.R"), echo=TRUE)
source(file.path("R","01_lifeHistoryTraits_STK2_highprod.R"), echo=TRUE)


# Generate conditioning for alternative life histories
n <- FLCore::n
source(file.path("R","02_FLBEIA_conditioning.R"), echo=TRUE)

# Generate the list of OM and MP scenarios for the simulations
source(file.path("input","create_lists_scenarios_FRONTIERS.R"), echo=TRUE)
source(file.path("input","create_lists_scenarios_f0.R"), echo=TRUE)
source(file.path("input","create_lists_scenarios_sigR0.R"), echo=TRUE)

# Generate different conditioning for iterations in alternative life histories
# (to run in a cluster)
# source(file.path("R","03_FLBEIA_conditioning_iters.R"), echo=TRUE)
# # as previous, but without fishing
# source(file.path("R","03_FLBEIA_conditioning_iters_f0.R"), echo=TRUE)
# # as previous, but without uncertainty in recruitment
# source(file.path("R","03_FLBEIA_conditioning_iters_sigR0.R"), echo=TRUE)

# Comparison of the generated OMs in the historical period
source(file.path("R","04_OM_analysis.R"), echo=TRUE)

# Running the simulations and joining results
# (to run in a cluster)
# source(file.path("R","05_FLBEIA_scenarios_Baranov.R"), echo=TRUE)
# 06_results_sumStats.R

# Analysing specific iterations for a selection of scenarios
source(file.path("R","06b_traces_outputs.R"), echo=TRUE)

# Different plots for analysing results
source(file.path("R","07a_plots_trajectories.R"), echo=TRUE)
source(file.path("R","07b_plots_perfind_BC.R"), echo=TRUE)
source(file.path("R","07c_plots_perfind_sensOM.R"), echo=TRUE)
source(file.path("R","07d_plots_perfind_sensCVID.R"), echo=TRUE)
source(file.path("R","07e_plots_perfind_withBSAFE.R"), echo=TRUE)
source(file.path("R","07f_plots_calendars.R"), echo=TRUE)

# Final plots for the publication
source(file.path("R","08_plots_Frontiers.R"), echo=TRUE)

