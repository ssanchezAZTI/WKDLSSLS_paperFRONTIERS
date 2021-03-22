################################################################################
#  WKDLSSLS results - summary of all results for Shiny App                     # 
#------------------------------------------------------------------------------#
#   AZTI-Tecnalia                                                              #
#   created:  17/03/2021                                                       #
#   modified:                                                                  #
################################################################################

# Copyright: AZTI, 2021
# Author: Sonia Sanchez, AZTI (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3

#==============================================================================
# WORKING DIRECTORY                                                        ----
#==============================================================================

# directory with input files
inp.dir <- "./input"

# directory with results
res.dir   <- "./output"
ressc.dir <- file.path(res.dir,"output_scenarios")

# directory with plots
plot.dir <- "./plots/FRONTIERS"

# shiny data
shiny.dir <- "./ShinyApp/data"


#==============================================================================
# LOAD LIBRARIES AND FUNCTIONS                                             ----
#==============================================================================

# # load libraries
# library(FLBEIA)
# library(ggplot2)
# library(R.utils)
# library(plyr)
library(tidyr)
library(dplyr)
# library(R.utils)
# 
# theme_set(theme_bw())


#==============================================================================
# HISTORICAL DATA - iav
#==============================================================================

dhist <- read.csv(file.path("input","list_oms_IAV_Depletion_TAC.csv"))

dhist <- dhist %>% 
  mutate(IAVhist = IAV, 
         FHIST = factor(FHIST, levels=c("f0","flow","fopt","fhigh")),
         LHSC = factor(LHSC, levels=c("lowprod","bc","highprod")),
         SIGR = factor(SIGR)) %>% 
  select(STKN, LHSC, SIGR, FHIST, IAVhist)


#==============================================================================
# SIMULATION DATA - quantiles
#==============================================================================

proj.yr <- 31

# load yearly information
load(file.path(res.dir,"all.RData"))
# all.bioQ; all.advQ

dat_bioQ <- all.bioQ

# add the other variables for scenario description

sc.dat <- read.table(file.path("input","list_scenarios.csv"), header=T, sep=",") %>% 
  mutate(SCnam = case_when( BSAFE == "none" ~ 
                              paste0( STKN, "_LHSC", LHSC, "_SIGR", SIGR, "_FHIST", FHIST, "_CVID", CVID, 
                                      "_ADVT", ADVT, "_", HCRT,
                                      "_PBUF", PBUF, "_UC", UCPL, "-", UCPU, "_HCRI", HCRI, sep=""),
                            TRUE ~ 
                              paste0( STKN, "_LHSC", LHSC, "_SIGR", SIGR, "_FHIST", FHIST, "_CVID", CVID, 
                                      "_ADVT", ADVT, "_", HCRT, BSAFE,
                                      "_PBUF", PBUF, "_UC", UCPL, "-", UCPU, "_HCRI", HCRI, sep="")),
         LHnam = paste0(STKN,LHSC)) %>% #select(SCENARIO, SCnam, LHnam) %>% 
  dplyr::rename(scenario=SCENARIO) 

dat_bioQ <- dat_bioQ %>% left_join(sc.dat, by="scenario")

# reshape to long format for ggplot

dat_bioQ <- dat_bioQ %>%
  gather("var_q", "value", -c("scenario","stock","year",OM:BSAFE,"SCnam","LHnam")) %>%
  separate("var_q", into = c("indicator", "quantile"), sep = "_") %>%
  tidyr::spread("quantile", "value")

# reoder some variables
dat_bioQ <- dat_bioQ %>% mutate(FHIST = factor(FHIST, levels=c("flow","fopt","fhigh")))


#==============================================================================
# SIMULATION DATA - Risk.Blim & catch.MSY
#==============================================================================

# load yearly information
load(file.path(res.dir,"BC_bio_yr.RData"))
# all.bioQ; all.advQ

dat_bio <- all.bio

# add the other variables for scenario description

dat_bio <- dat_bio %>% 
  left_join(sc.dat, by="scenario")

OMnam.levels <- apply(arrange(expand.grid(STKN=c("STK1","STK2"), 
                                          FHIST=c("flow","fopt","fhigh")),STKN), 1, paste, collapse="_")

ucpl.val <- as.character(sort(unique(dat_bio$UCPL)))
if (ucpl.val[1]=="0") ucpl.val <- c(ucpl.val[-1],ucpl.val[1])

ucpu.val <- as.character(sort(unique(dat_bio$UCPU)))
if (ucpu.val[1]=="0") ucpu.val <- c(ucpu.val[-1],ucpu.val[1])

dat_bio <- dat_bio %>%
  mutate(FHIST = factor(FHIST, levels=c("flow","fopt","fhigh")), 
         OMnam = paste(STKN,FHIST,sep="_"), 
         OMnam = factor(OMnam, levels=OMnam.levels), 
         UCPL = factor(UCPL, ordered = TRUE, levels=ucpl.val), 
         UCPU = factor(UCPU, ordered = TRUE, levels=ucpu.val),
         UC = paste0("(",UCPL,",",UCPU,")"),
         UC = case_when(UC=="(0,0)" ~ "(NA,NA)",
                        TRUE ~ UC))


#==============================================================================
# Read performance statistics
#==============================================================================

# BC
#~~~~~~~~~~~

  df_bc <- read.table(file.path(res.dir,"output_stats","perfstats_BC.csv"), header=T, sep=";")
  
  # reshape to the long format for ggplot
  
  df_bc <- df_bc %>% gather(indicator, value, names(df_bc)[!names(df_bc) %in% c("stock", "scenario", "term", "depl.ini", "IAVhist", "RiskBlim.ini")])
  
  # period as an ordered factor for the figures
  
  df_bc$term <- factor(df_bc$term, levels=c("short","mid","long"))
  
  row.names(df_bc) <- NULL
  df_bc$stock <- NULL
  names(df_bc)[1] <- "SCENARIO"
  
  # add the other variables for scenario description
  
  sc.dat <- read.table(file.path("input","list_scenarios.csv"), header=T, sep=",")
  df_bc <- df_bc %>% left_join(sc.dat, by="SCENARIO") # we use the function join from dplyr instead of merge to keep the order of the rows
  
  # compute the ratio between the CV of the obs index and the IAV
  
  df_bc <- df_bc %>% mutate(CVIndex = case_when(CVID=="low"  ~ 0.25, 
                                          CVID=="high" ~ 0.5, 
                                          CVID=="iav"  ~ IAVhist, 
                                          CVID=="iav2" ~ 2*IAVhist, 
                                          TRUE         ~ 0*NA), 
                      Ratio = CVIndex/IAVhist)
  
  # factorize some values
  
  ucpl.val <- as.character(sort(unique(df_bc$UCPL)))
  if (ucpl.val[1]=="0") ucpl.val <- c(ucpl.val[-1],ucpl.val[1])
  
  ucpu.val <- as.character(sort(unique(df_bc$UCPU)))
  if (ucpu.val[1]=="0") ucpu.val <- c(ucpu.val[-1],ucpu.val[1])
  
  df_bc <- df_bc %>% mutate(UCPL = factor(UCPL, ordered = TRUE, levels=ucpl.val), 
                      UCPU = factor(UCPU, ordered = TRUE, levels=ucpu.val),
                      UC = paste0("(",UCPL,",",UCPU,")"),
                      UC = case_when(UC=="(0,0)" ~ "(NA,NA)",
                                     TRUE ~ UC),
                      FHIST = factor(FHIST, levels=c("flow","fopt","fhigh")),
                      ADVT = ordered(ADVT, levels=c("fix","int","iny","fpa")), 
                      HCRI = ordered(HCRI, levels=c("fix","pob","pyc","nin")), 
                      BSAFE = ordered(BSAFE, levels=c("none","Imin","Iminpa","Inorm")),
                      HCR = case_when(BSAFE == "none" ~ HCRT,
                                       TRUE ~ paste(HCRT,BSAFE,sep="_")),
                      CVID = ordered(CVID, levels=c("low","high","iav","iav2")),
                      OMnam = paste(STKN,FHIST,sep="_"), 
                      OMnam = factor(OMnam, 
                                     levels=apply(arrange(expand.grid(STKN=c("STK1","STK2"), 
                                                                      FHIST=c("flow","fopt","fhigh")),STKN), 1, paste, collapse="_")))
# sensCIVD
#~~~~~~~~~~~
  
  df_cvid <- read.table(file.path(res.dir,"output_stats","perfstats_sensCVID.csv"), header=T, sep=";")
  
  # reshape to the long format for ggplot
  
  df_cvid <- df_cvid %>% gather(indicator, value, names(df_cvid)[!names(df_cvid) %in% c("stock", "scenario", "term", "depl.ini", "IAVhist", "RiskBlim.ini")])
  
  # period as an ordered factor for the figures
  
  df_cvid$term <- factor(df_cvid$term, levels=c("short","mid","long"))
  
  row.names(df_cvid) <- NULL
  df_cvid$stock <- NULL
  names(df_cvid)[1] <- "SCENARIO"
  
  # add the other variables for scenario description
  
  sc.dat <- read.table(file.path("input","list_scenarios.csv"), header=T, sep=",")
  df_cvid <- df_cvid %>% left_join(sc.dat, by="SCENARIO") # we use the function join from dplyr instead of merge to keep the order of the rows
  
  # compute the ratio between the CV of the obs index and the IAV
  
  df_cvid <- df_cvid %>% mutate(CVIndex = case_when(CVID=="low"  ~ 0.25, 
                                          CVID=="high" ~ 0.5, 
                                          CVID=="iav"  ~ IAVhist, 
                                          CVID=="iav2" ~ 2*IAVhist, 
                                          TRUE         ~ 0*NA), 
                      RatioHist = CVIndex/IAVhist)
  
  iavproj <- df_cvid %>% filter(indicator=="IAV") %>% select(SCENARIO, term, value) %>% dplyr::rename(IAVproj=value)
  
  df_cvid <- df_cvid  %>% left_join(iavproj, by=c("SCENARIO","term")) %>% 
    mutate(RatioProj = CVIndex/IAVproj)
  
  # factorize some values
  
  ucpl.val <- as.character(sort(unique(df_cvid$UCPL)))
  if (ucpl.val[1]=="0") ucpl.val <- c(ucpl.val[-1],ucpl.val[1])
  
  ucpu.val <- as.character(sort(unique(df_cvid$UCPU)))
  if (ucpu.val[1]=="0") ucpu.val <- c(ucpu.val[-1],ucpu.val[1])
  
  df_cvid <- df_cvid %>% mutate(UCPL = factor(UCPL, ordered = TRUE, levels=ucpl.val), 
                      UCPU = factor(UCPU, ordered = TRUE, levels=ucpu.val),
                      UC = paste0("(",UCPL,",",UCPU,")"),
                      UC = case_when(UC=="(0,0)" ~ "(NA,NA)",
                                     TRUE ~ UC),
                      FHIST = factor(FHIST, levels=c("flow","fopt","fhigh")),
                      LHSC = factor(LHSC, levels = c("lowprod","bc","highprod")),
                      ADVT = ordered(ADVT, levels=c("fix","int","iny","fpa")), 
                      HCRI = ordered(HCRI, levels=c("fix","pob","pyc","nin")), 
                      CVID = ordered(CVID, levels=c("low","iav","high","iav2")),
                      OMnam = paste(STKN,FHIST,sep="_"), 
                      OMnam = factor(OMnam, 
                                     levels=apply(arrange(expand.grid(STKN=c("STK1","STK2"), 
                                                                      FHIST=c("flow","fopt","fhigh")),STKN), 1, paste, collapse="_")))
  
# sensOM
#~~~~~~~~~~~
  
  df_om <- read.table(file.path(res.dir,"output_stats","perfstats_sensOM.csv"), header=T, sep=";")
  
  # reshape to the long format for ggplot
  
  df_om <- df_om %>% gather(indicator, value, names(df_om)[!names(df_om) %in% c("stock", "scenario", "term", "depl.ini", "IAVhist", "RiskBlim.ini")])
  
  # period as an ordered factor for the figures
  
  df_om$term <- factor(df_om$term, levels=c("short","mid","long"))
  
  row.names(df_om) <- NULL
  df_om$stock <- NULL
  names(df_om)[1] <- "SCENARIO"
  
  # add the other variables for scenario description
  
  sc.dat <- read.table(file.path("input","list_scenarios.csv"), header=T, sep=",")
  df_om <- df_om %>% left_join(sc.dat, by="SCENARIO") # we use the function join from dplyr instead of merge to keep the order of the rows
  
  # compute the ratio between the CV of the obs index and the IAV
  
  df_om <- df_om %>% mutate(CVIndex = case_when(CVID=="low"  ~ 0.25, 
                                          CVID=="high" ~ 0.5, 
                                          CVID=="iav"  ~ IAVhist, 
                                          CVID=="iav2" ~ 2*IAVhist, 
                                          TRUE         ~ 0*NA), 
                      Ratio = CVIndex/IAVhist)
  
  # factorize some values
  
  ucpl.val <- as.character(sort(unique(df_om$UCPL)))
  if (ucpl.val[1]=="0") ucpl.val <- c(ucpl.val[-1],ucpl.val[1])
  
  ucpu.val <- as.character(sort(unique(df_om$UCPU)))
  if (ucpu.val[1]=="0") ucpu.val <- c(ucpu.val[-1],ucpu.val[1])
  
  df_om <- df_om %>% mutate(UCPL = factor(UCPL, ordered = TRUE, levels=ucpl.val), 
                      UCPU = factor(UCPU, ordered = TRUE, levels=ucpu.val),
                      UC = paste0("(",UCPL,",",UCPU,")"),
                      UC = case_when(UC=="(0,0)" ~ "(NA,NA)",
                                     TRUE ~ UC),
                      FHIST = factor(FHIST, levels=c("flow","fopt","fhigh")),
                      LHSC = factor(LHSC, levels=c("lowprod","bc","highprod")),
                      SIGR = factor(SIGR),
                      ADVT = ordered(ADVT, levels=c("fix","int","iny","fpa")), 
                      HCRI = ordered(HCRI, levels=c("fix","pob","pyc","nin")), 
                      CVID = ordered(CVID, levels=c("low","high","iav","iav2")),
                      OMnam = paste(STKN,FHIST,sep="_"), 
                      OMnam = factor(OMnam, 
                                     levels=apply(arrange(expand.grid(STKN=c("STK1","STK2"), 
                                                                      FHIST=c("flow","fopt","fhigh")),STKN), 1, paste, collapse="_")))
  
  
#==============================================================================
# COLOUR PALETTES
#==============================================================================

# for the UC

ucp.col <- c("cadetblue1", "blue",                        # (0.2,x)
             "olivedrab1", "springgreen", "springgreen4", # (0.5,x)
             "yellow", "darkorange", "red", "darkred",    # (0.8,x)
             "darkmagenta")                               # (NA,NA)

  
  
# for subset of UC
ucp.col2 <- c("orange",       # (0.5,0.5)
              "springgreen4", # (0.8,0.8)
              "yellow",       # (0.8,2.75)
              "blue",         # (0.8,4)
              "red",          # (0.8,5.25)
              "darkmagenta")  # (NA,NA)


# for subset of UC

ucp.col3 <- c("red",          # (0.2,0.2)
              "springgreen4", # (0.8,0.8)
              "blue",         # (0.8,4)
              "darkmagenta")  # (NA,NA)



#==============================================================================
# comparison of several performance statistics 
#==============================================================================

# names and labels of performance statistics

perfnms <- c("Risk3.Blim","catch.MSY")

perflabels <- c("Risk3.Blim","catch/MSY")



#==============================================================================
# SAVE objects                                                             ----
#==============================================================================

save( dhist, dat_bio, dat_bioQ, df_bc, df_cvid, df_om, 
      ucp.col, ucp.col2, ucp.col3, perfnms, perflabels,
      file = file.path(shiny.dir,"plotinputs.RData"))


