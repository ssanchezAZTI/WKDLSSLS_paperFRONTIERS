# WKDLSSLS_paperFRONTIERS

Code for the paper in preparation based on ssanchezAZTI/WKDLSSLS_2019 (branch: paper) and including some improvements from libaibarriaga/WKDLSSLS2_2020

## Description of R scripts:

01_lifeHistoryTraits_STK1_bc.R
------------------------------
	For STK1 and bc creates in the input folder STK1bc_dataLH.RData that contains: stk, sr_model, sr_params, ref.pts (same for lowprod and highprod instead bc)

01_lifeHistoryTraits_STK2_bc.R
------------------------------
	For STK2 and bc creates in the input folder STK2bc_dataLH.RData that contains: stk, sr_model, sr_params, ref.pts (same for lowprod and highprod instead bc)

01b_brps.R
----------
	Code used in 01_lifeHistoryTraits_*_*.R for estimating the reference points.

02_FLBEIA_conditioning.R
------------------------
	For all defined stkn (STK1 and STK2) and lhsc (bc, lowprod and highprod) creates in the input folder the {stkn}{lhsc}_data.RData file that contains biols, SRs, fleets, indices, advice, main.ctrl, biols.ctrl, fleets.ctrl, obs.ctrl, assess.ctrl, advice.ctrl

03_FLBEIA_conditioning_iters.R
------------------------------
	Code to be run in the cluster (using cluster/iters.qsub).
	The OM is taken from the file input/list_oms.csv according to the task number.
    For each OM it creates in the input folder data_omXXX.RData that contains biols, SRs, fleets, indices, advice, main.ctrl, biols.ctrl, fleets.ctrl, obs.ctrl, assess.ctrl, advice.ctrl, IAV

03b_FLBEIA_conditioning_iters_f0.R
----------------------------------
	As 03_FLBEIA_conditioning_iters.R, but restricted to scenarios with historical F=0.

03c_FLBEIA_conditioning_iters_sigR0.R
-------------------------------------
	As 03_FLBEIA_conditioning_iters.R, but assumming that there is not error in the historical recruitment (i.e. values are those predicted from the SRR).
	
04_OM_analysis.R
----------------
	It compares the different OMs generated to analyse their properties (e.g. initial depletion levels, SSB/Bmsy, average catch in the last 10 years of the projection period, catch/MSY or F-levels).

05_FLBEIA_scenarios_Baranov.R
-----------------------------
	Code to be run in the cluster (using cluster/sims.qsub).
	The scenario is taken from the file input/list_scenarios.csv according to the task number.

06_results_sumStats.R
---------------------
	Code to be run in the cluster (using cluster/stats.qsub).
	The total amount of scenarios are taken from the folder output/output_scenarios and are stored in the file output/scenario_list.RData.

06b_traces_outputs.R
--------------------
	For analysing the traces of specific iteration for a selection of scenarios (for checking bugs).

07a_plots_trajectories.R
------------------------
	Code to plot catch and SSB trajectories for each OM (with lines for specific iterations).

07b_plots_perfind_BC.R
----------------------
	Code for generating several plots for scenarios in BC without a biomass safeguard in the rule (i.e. BC + BSAFE = "none").

07c_plots_perfind_sensOM.R
--------------------------
	Code for generating several plots for testing sensitivity on the OM assumptions (i.e. scenarios in sensOM).

07d_plots_perfind_sensCVID.R
----------------------------
	Code for generating several plots for testing sensitivity to the CV in the observed index (i.e. scenarios in sensCVID).

07e_plots_perfind_withBSAFE.R
-----------------------------
	Code for generating several plots for testing the impact of including a biomass safeguard in the rule (i.e. all scenarios in BC).

07f_plots_calendars.R
---------------------
	Code for analysing the anomalies observed in calendars analysis.

08_plots_Frontiers.R
--------------------
	Code for generating the plots to be included in the FRONTIERS' article. 



