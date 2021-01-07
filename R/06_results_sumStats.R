################################################################################
#  WKDLSSLS - summary statistics for each scenario and join results            # 
#------------------------------------------------------------------------------#
#   Sonia Sanchez (AZTI-Tecnalia)                                              #
#   created:  03/10/2018                                                       #
#   modified: 2019-05-30 15:03:08 - ssanchez: adapted for new case study       # 
################################################################################

# 05_results_sumStats.r - summary statistics for all scenarios
# ~/WKDLSSLS_2019/05_results_sumStats.r

# Copyright: AZTI, 2019
# Author: Sonia Sanchez (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3

#==============================================================================
#                                                                          ----
#==============================================================================

rm(list=ls())

#==============================================================================
# WORKING DIRECTORY                                                        ----
#==============================================================================

# wd <- "C:/use/GitHub/ssanchezAZTI/WKDLSSLS_2019" # main directory
# setwd(wd)

# directory with results

res.dir <- "./output"


#==============================================================================
# LOAD LIBRARIES AND FUNCTIONS                                             ----
#==============================================================================

library(FLBEIA, lib.loc="./Rlibs/")
library(R.utils)
library(data.table)
library(dplyr)
library(tidyr)

source("./R/fun/perfInd_wkdlssls.R")

rename <- dplyr::rename


#==============================================================================
# SCENARIOS                                                                ----
#==============================================================================

# complete list of scenarios

load(file.path(res.dir, "scenario_list.RData"))

# update the list of scenarios according to the output files 

files <- list.files(file.path(res.dir,"output_scenarios"))
scenario_list <- unique(sapply(gsub('.{6}$', '', files),function(x){strsplit(x,"_")[[1]][2]}))

save(list=c("scenario_list"),file=file.path(res.dir, "scenario_list.RData"))

# names of the scenarios
scenario_list


# Select specific groups of scenarios:
#--------------------------------------

sc.all <- read.csv(file.path("input","list_scenarios.csv")) %>% 
  filter(SCENARIO %in% scenario_list)

# - BC run:
# scenario_list_bc <- scenario_list[scenario_list %in% paste0("sc", sprintf("%06d",1:726))]
scenario_list_bc <- sc.all %>% 
  filter(LHSC == "bc" & SIGR == 0.75 & IDXT == "b1p" & CVID == "low") %>% .$SCENARIO %>% as.character()

# Sensitivity only for selected rules:
sc.sensOM <- sc.all %>% 
  filter( ADVT == "iny" & HCRI == "nin" & PBUF == 0 & 
            ((HCRT == "1o2" &  UCPL == 0.8 & UCPU == 0.8 ) | (HCRT == "1o2" &  UCPL == 0 & UCPU == 0 ) | 
               (HCRT == "1o2" &  UCPL == 0.8 & UCPU == 4 ) | (HCRT == "2o3" &  UCPL == 0.8 & UCPU == 0.8)) & 
            BSAFE == "none")

sc.sensCVID <- sc.all %>% 
  filter( ADVT == "iny" & HCRI == "nin" & PBUF == 0 &
            ((HCRT == "1o2" &  UCPL == 0.8 & UCPU == 0.8 ) | (HCRT == "2o3" &  UCPL == 0.8 & UCPU == 0.8) |
               (UCPL == 0 & UCPU == 0 )) & 
            BSAFE == "none")

# - Sens. to the OM:
# scenario_list_bc <- scenario_list[scenario_list %in% paste0("sc", sprintf("%06d",727:858))]
scenario_list_sensOM <- sc.sensOM %>% 
  filter(IDXT == "b1p" & CVID == "low") %>% .$SCENARIO %>% as.character()

# - Sens. to sampling quality
# scenario_list_bc <- scenario_list[scenario_list %in% paste0("sc", sprintf("%06d",859:990))]
scenario_list_sensCVID <- sc.sensCVID %>% 
  filter(IDXT == "b1p") %>% .$SCENARIO %>% as.character() # LHSC == "bc" & SIGR == 0.75 & 


# Save them:
#------------
save( list=c("scenario_list","scenario_list_bc","scenario_list_sensOM","scenario_list_sensCVID"),
      file=file.path(res.dir, "scenario_lists_byCase.RData"))


#==============================================================================
#  For each scenario compute the performance statistics and bind them:
#==============================================================================

# all.stats <- NULL

# # only for selected scenarios
# ff <- read.csv(file.path("input", "list_scenarios.csv"), header=T)
# idx <- subset(ff, ADVT=="iny" & PBUF==0 & HCRI=="nin" & HCRT %in% c("2o3", "1o2"))
# idx <- subset(idx, SIGR < 1)
# scenario_list <- idx$SCENARIO

for (cs in c("BC","sensOM","sensCVID")) {
  
  print(paste("~~~~~~ Case", cs))
  
  flnm <- paste0("perfstats_",cs,".csv")
  
  if (cs=="BC") {
    sl <- scenario_list_bc
  } else if (cs=="sensOM") {
    sl <- scenario_list_sensOM
  }  else if (cs=="sensCVID") {
    sl <- scenario_list_sensCVID
  } 
  
  for (scenario in sl){
    
    print(paste("~~~ Working on scenario", scenario))
    print(Sys.time())
    rm(list=ls()[! ls() %in% c("all.stats","scenario","scenario_list","sl",
                               "scenario_list_bc","scenario_list_sensOM","scenario_list_sensCVID",
                               "flnm","res.dir", "perfInd")])
    
    file.RData <- file.path(res.dir, "output_scenarios", paste0("out_",scenario,".RData"))
    
    out.stats <- perfInd(file.RData, scenario)
    
    if (file.exists(file.path(res.dir,"output_stats",flnm))) {
      write.table(out.stats, file=file.path(res.dir,"output_stats",flnm), sep=";", row.names=F, append=T, col.names=F)
    } else {
      write.table(out.stats, file=file.path(res.dir,"output_stats",flnm), sep=";", row.names=F, append=F)
    }
    
  }

}

#==============================================================================
#  Bind the quantile summaries if needed for FLBEIA:                       ----
#==============================================================================

all.bioQ <- NULL
all.advQ <- NULL

for (scenario in scenario_list){
  
  print(paste("~~~ Working on scenario", scenario))

  rm(list=ls()[! ls() %in% c("all.bioQ","all.advQ","scenario","scenario_list",
                             "scenario_list_bc","scenario_list_sensOM","scenario_list_sensCVID","res.dir")])

  file.RData <- file.path(res.dir, "output_scenarios", paste0("out_",scenario,".RData"))
  
  load(file.RData)
  
# summary quantiles in wide format

  out.bioQ <- get(paste(scenario,"bioQ",sep="_"))
  out.advQ <- get(paste(scenario,"advQ",sep="_"))
  
# join all scenarios
  
  all.bioQ <- rbind(all.bioQ, out.bioQ)
  all.advQ <- rbind(all.advQ, out.advQ)

}

all.bioQ <- as.data.frame(all.bioQ)
all.advQ <- as.data.frame(all.advQ)


#==============================================================================
#  Bind the annual risks and relative yields:                              ----
#==============================================================================

all.bio <- NULL

for (scenario in scenario_list_bc){

  file.RData <- file.path(res.dir, "output_scenarios", paste0("out_",scenario,".RData"))
  
  d.bio    <- loadToEnv(file.RData)[[paste(scenario,"bio",sep="_")]] 
  d.refpts <- loadToEnv(file.RData)[[paste(scenario,"refpts",sep="_")]]
  # d.pars   <- loadToEnv(file.RData)[[paste(scenario,"pars",sep="_")]] %>% rename(scenario = SCENARIO)
  
  d.bio <- d.bio %>% subset(year >= 30) %>% 
    # left_join(d.pars, by = "scenario") %>%
    ungroup() %>% 
    select(scenario, iter, year, ssb, catch) %>% # select(scenario, iter, STKN, FHIST, HCRT, UCPL, UCPU, year, ssb, catch) %>% 
    mutate(Blim=d.refpts["Blim", iter], 
           MSY=d.refpts["MSY", iter]) %>% 
    group_by(scenario, year) %>%
    summarize(Risk.Blim = mean(ssb<Blim),
              catch.MSY = median(catch/MSY))
  
  all.bio <- rbind(all.bio, d.bio)
  
}


#==============================================================================
# Save all the results for all scenarios
#==============================================================================

save(all.bioQ, all.advQ, file=file.path(res.dir, "all.RData"))

save(all.bio, file=file.path(res.dir, "BC_bio_yr.RData"))

#==============================================================================
