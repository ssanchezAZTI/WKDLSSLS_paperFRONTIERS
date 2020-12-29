# WKDLSSLS_paperFRONTIERS

Code for the paper in preparation based on ssanchezAZTI/WKDLSSLS_2019 (branch: paper) and including some improvements from libaibarriaga/WKDLSSLS2_2020

## Description of R scripts:

01_lifeHistoryTraits_STK1_bc.R:
	For STK1 and bc creates in the input folder STK1bc_dataLH.RData that contains: stk, sr_model, sr_params, ref.pts (same for lowprod and highprod instead bc)

01_lifeHistoryTraits_STK2_bc.R:
	For STK2 and bc creates in the input folder STK2bc_dataLH.RData that contains: stk, sr_model, sr_params, ref.pts (same for lowprod and highprod instead bc)

01b_brps.R
	Code used in 01_lifeHistoryTraits_*_*.R for estimating the reference points.

02_FLBEIA_conditioning.R
	For all defined stkn (STK1 and STK2) and lhsc (bc, lowprod and highprod) creates in the input folder the {stkn}{lhsc}_data.RData file that contains biols, SRs, fleets, indices, advice, main.ctrl, biols.ctrl, fleets.ctrl, obs.ctrl, assess.ctrl, advice.ctrl

03_FLBEIA_conditioning_iters.R
	Code to be run in the cluster (using cluster/iters.qsub).
	The OM is taken from the file input/list_oms.csv according to the task number.
    For each OM it creates in the input folder data_omXXX.RData that contains biols, SRs, fleets, indices, advice, main.ctrl, biols.ctrl, fleets.ctrl, obs.ctrl, assess.ctrl, advice.ctrl, IAV

03_FLBEIA_conditioning_iters_f0.R
	As 03_FLBEIA_conditioning_iters.R, but restricted to scenarios with historical F=0.

03_FLBEIA_conditioning_iters_sigR0.R
	As 03_FLBEIA_conditioning_iters.R, but assumming that there is not error in the historical recruitment (i.e. values are those predicted from the SRR).
	
04_OM_analysis.R
	It compares the different OMs generated to analyse their properties (e.g. initial depletion levels, SSB/Bmsy, average catch in the last 10 years of the projection period, catch/MSY or F-levels).

05_FLBEIA_scenarios_Baranov.R
	Code to be run in the cluster (using cluster/sims.qsub).
	The scenario is taken from the file input/list_scenarios.csv according to the task number.


06_results_sumStats.R
	XXX

06b_traces_outputs.R
	XXX

07_plots_perfind_BC.R
	XXX

07_plots_trajectories.R
	XXX

07b_plots_perfind_sensCVID.R
	XXX

07b_plots_perfind_sensOM.R
	XXX

08_plots_perfind_bsafe.R
	XXX

08_plots_perfind_new.R
	XXX

09_plots_traces.R
	XXX

11_plots_Frontiers.R
	XXX



