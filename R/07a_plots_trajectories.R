################################################################################
#  WKDLSSLS results - plots of trajectories                                    # 
#------------------------------------------------------------------------------#
#   AZTI-Tecnalia                                                              #
#   created:  10/07/2020                                                       #
#   modified:                                                                  #
################################################################################

# Copyright: AZTI, 2020
# Author: AZTI (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3

#==============================================================================
#                                                                          ----
#==============================================================================

rm(list=ls())

set.seed(215)


#==============================================================================
# WORKING DIRECTORY                                                        ----
#==============================================================================

# directory with input files
inp.dir <- "./input"

# directory with results
res.dir   <- "./output"
ressc.dir <- file.path(res.dir,"output_scenarios")

# directory with plots
plot.dir <- "./plots"

#==============================================================================
# LOAD LIBRARIES AND FUNCTIONS                                             ----
#==============================================================================

# load libraries
library(R.utils)
library(ggplot2)
# library(plyr)
library(tidyr)
library(dplyr)

theme_set(theme_bw())


#==============================================================================
# DATA
#==============================================================================

proj.yr <- 31


# load yearly information (quantiles)
#--------------------------------------

load(file.path(res.dir,"all.RData"))
# all.bioQ; all.advQ

dat <- all.bioQ

# add the other variables for scenario description

sc.dat0 <- read.table(file.path("input","list_scenarios.csv"), header=T, sep=",") %>% 
  mutate(SCnam = case_when( BSAFE == "none" ~ 
                              paste0( STKN, "_LHSC", LHSC, "_SIGR", SIGR, "_FHIST", FHIST, "_CVID", CVID, 
                                      "_ADVT", ADVT, "_", HCRT,
                                      "_PBUF", PBUF, "_UC", UCPL, "-", UCPU, "_HCRI", HCRI, sep=""),
                            TRUE ~ 
                              paste0( STKN, "_LHSC", LHSC, "_SIGR", SIGR, "_FHIST", FHIST, "_CVID", CVID, 
                                      "_ADVT", ADVT, "_", HCRT, BSAFE,
                                      "_PBUF", PBUF, "_UC", UCPL, "-", UCPU, "_HCRI", HCRI, sep="")),
         LHnam = paste0(STKN,LHSC)) %>% 
 select(SCENARIO, SCnam, LHnam) %>% rename(scenario=SCENARIO) 

dat <- dat %>% left_join(sc.dat0, by="scenario")

# reshape to long format for ggplot

dat <- dat %>%
  gather("var_q", "value", -c("scenario","stock","year","SCnam","LHnam")) %>%
  separate("var_q", into = c("indicator", "quantile"), sep = "_") %>%
  spread("quantile", "value")


# load statistics by year
#--------------------------

load(file.path(res.dir,"BC_bio_yr.RData"))
# all.bio

dat_bio <- all.bio

# add the other variables for scenario description

OMnam.levels <- apply(arrange(expand.grid(STKN=c("STK1","STK2"), 
                                          FHIST=c("flow","fopt","fhigh")),STKN), 1, paste, collapse="_")
sc.dat <- read.table(file.path("input","list_scenarios.csv"), header=T, sep=",") %>% 
  mutate(SCnam = case_when( BSAFE == "none" ~ 
                              paste0( STKN, "_LHSC", LHSC, "_SIGR", SIGR, "_FHIST", FHIST, "_CVID", CVID, 
                                      "_ADVT", ADVT, "_", HCRT,
                                      "_PBUF", PBUF, "_UC", UCPL, "-", UCPU, "_HCRI", HCRI, sep=""),
                            TRUE ~ 
                              paste0( STKN, "_LHSC", LHSC, "_SIGR", SIGR, "_FHIST", FHIST, "_CVID", CVID, 
                                      "_ADVT", ADVT, "_", HCRT, BSAFE,
                                      "_PBUF", PBUF, "_UC", UCPL, "-", UCPU, "_HCRI", HCRI, sep="")),
         LHnam = paste0(STKN,LHSC), 
         FHIST = factor(FHIST, levels=c("flow","fopt","fhigh")), 
         OMnam = paste(STKN,FHIST,sep="_"), 
         OMnam = factor(OMnam, levels=OMnam.levels), 
         ADVT = ordered(ADVT, levels=c("fix","int","iny","fpa")),
         UC = paste0("(",UCPL,",",UCPU,")")) %>% 
  rename(scenario=SCENARIO) 

dat_bio <- dat_bio %>% left_join(sc.dat, by="scenario")

advt.col <- c("red",  # int 
              "blue", #iny 
              "springgreen") # fpa


# select specific iterations
#-----------------------------

sc0 <- unique(dat$scenario)[1]
nit.sample <- 2

it.sel <- sample( loadToEnv(file.path(ressc.dir,paste0("out_",sc0,".RData")))[[paste(sc0,"bio",sep="_")]] %>% 
                    .$iter %>% unique(), nit.sample)
# 464 319



#==============================================================================
# TRAJECTORIES (SSB & catch)
#==============================================================================

# indicators to plot

ids <- c("ssb","catch")


# select indicators

dd <- dat %>% filter(indicator %in% ids)


# plot

t1 <- Sys.time()

pdf(file.path(plot.dir,"trajectories_by_scenario.pdf"), width=14, onefile=T)

  for (sc in unique(dat$scenario)) {
    
    print(paste("~~~ Working on scenario", sc))
    
    aux <- dd %>% filter(scenario == sc)
    
    scnam  <- unique(aux$SCnam)
    refpts <- loadToEnv(file.path(inp.dir,paste0(unique(aux$LHnam),"_dataLH.RData")))[["ref.pts"]]
    
    # all iterations (wide format)
    
    dd.its <- loadToEnv(file.path(ressc.dir,paste0("out_",sc,".RData")))[[paste(sc,"bio",sep="_")]]
    
    # select specific indicators
    
    aux2 <- dd.its %>% select(scenario,stock,year,iter,one_of(ids)) %>% 
      mutate(iter = as.factor(iter))
    
    # reshape to the long format for ggplot
    
    aux2 <- reshape( aux2, direction="long", varying=names(aux2)[-c(1:4)], v.names=c("value"),
                     idvar=names(aux2)[c(1:4)], timevar="indicator", times=names(aux2)[-c(1:4)])
    
    p <- ggplot(data = aux, aes(x = year, y = q50)) + 
      geom_line() + 
      # geom_line(data = obj, aes(x=year, y=value, color = iter)) +
      geom_ribbon(aes(x = year, ymin = q05, ymax = q95), alpha = 0.5) + 
      facet_wrap(~indicator, scales = "free") + 
      expand_limits(y=0) +
      geom_vline(xintercept = proj.yr - 0.5, linetype = "longdash") +
      geom_hline(lty = 2, data = data.frame(indicator = c(rep("ssb",2),"catch"),
                                            Bref = c(Blim=refpts[["Blim"]], Bcollapse=0.1*refpts[["B0"]],
                                                     MSY=refpts[["MSY"]])),
                 aes(yintercept = Bref), color = c("orange", "red", "green")) +
      theme_bw() + 
      theme(text = element_text(size = 20), 
            title = element_text(size = 16, face = "bold"), 
            strip.text = element_text(size = 20)) + 
      ylab("") + 
      geom_line(data=subset(aux2, iter %in% it.sel),
                aes(year, y=value, group=iter, col=iter))+
      ggtitle(scnam) + 
      theme(plot.title = element_text(hjust = 0.5))
    print(p)
  }

dev.off()

t2 <- Sys.time()

t2 - t1



#==============================================================================
# TRAJECTORIES (risks & ryields)
#==============================================================================

aux <- dat_bio %>% filter(LHSC == "bc" & SIGR == 0.75 & CVID == "low" & 
                            HCRT != "ft0" & HCRI == "nin" & BSAFE == "none") %>% 
  select(-LHSC, -SIGR, -CVID, -HCRI, -BSAFE) %>% 
  mutate(OMnam = factor(OMnam, levels = OMnam.levels))


# - risks

pdf(file.path(plot.dir,"trajectories_risks_byUCs.pdf"), width=14, onefile=T)

for (ucopt in unique(aux$UC)) {
  
  aux1 <- aux %>% 
    filter( UC == ucopt)
  
  p <- ggplot(data = aux1, aes(x = year, y = Risk.Blim, col=ADVT)) +
    geom_line() +
    facet_grid(OMnam ~ HCRT, scales = "free") +
    scale_color_manual(values = advt.col)+
    geom_hline(yintercept = 0.05, linetype = "longdash") +
    theme_bw() +
    theme(text = element_text(size = 20),
          title = element_text(size = 16, face = "bold"),
          strip.text = element_text(size = 12)) +
    ggtitle(paste("UC:",ucopt))
  
  print(p)
  
}

dev.off()


# - catch.MSY

pdf(file.path(plot.dir,"trajectories_ryields_byUCs.pdf"), width=14, onefile=T)

for (ucopt in unique(aux$UC)) {
  
  aux1 <- aux %>% 
    filter( UC == ucopt)
  
  p <- ggplot(data = aux1, aes(x = year, y = catch.MSY, col=ADVT)) +
    geom_line() +
    facet_grid(OMnam ~ HCRT, scales = "free") +
    scale_color_manual(values = advt.col)+
    geom_hline(yintercept = 1, linetype = "longdash") +
    theme_bw() +
    theme(text = element_text(size = 20),
          title = element_text(size = 16, face = "bold"),
          strip.text = element_text(size = 12)) +
    ggtitle(paste("UC:",ucopt))
  
  print(p)
  
}

dev.off()


