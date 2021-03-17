################################################################################
#  WKDLSSLS results - plots for the FRONTIERS paper                            # 
#------------------------------------------------------------------------------#
#   AZTI-Tecnalia                                                              #
#   created:  15/07/2020                                                       #
#   modified:                                                                  #
################################################################################

# Copyright: AZTI, 2020
# Author: Sonia Sanchez, AZTI (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3

#==============================================================================
#                                                                          ----
#==============================================================================

rm(list=ls())

set.seed(2159)

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

#==============================================================================
# LOAD LIBRARIES AND FUNCTIONS                                             ----
#==============================================================================

# load libraries
library(FLBEIA)
library(ggplot2)
library(R.utils)
library(plyr)
library(tidyr)
library(dplyr)
library(R.utils)

theme_set(theme_bw())


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
# IAV: among different OMs
#==============================================================================

# lapply(df_om, unique)
# 
# aux <- df_om %>% filter(term == "short" & HCRT == "1o2" & UC == "(NA,NA)" %>% 
#   select(IAVhist, STKN, LHSC, SIGR, FHIST)
  
dhist <- read.csv(file.path("input","list_oms_IAV_Depletion_TAC.csv"))

aux <- dhist %>% 
    mutate(IAVhist = IAV, 
           FHIST = factor(FHIST, levels=c("f0","flow","fopt","fhigh")),
           LHSC = factor(LHSC, levels=c("lowprod","bc","highprod")),
           SIGR = factor(SIGR)) %>% 
    select(STKN, LHSC, SIGR, FHIST, IAVhist)

jpeg(file.path(plot.dir,"fig02_IAVhist.jpeg"), quality=100, width=900, height=700)

  p <- ggplot(aux, aes(x=SIGR, y=IAVhist, fill=FHIST))+
    geom_bar(stat="identity", position="dodge")+
    facet_grid(LHSC ~ STKN)+
    theme(text = element_text(size = 20), 
          title = element_text(size = 16, face = "bold"), 
          strip.text = element_text(size = 20))
  
  print(p)

dev.off()

jpeg(file.path(plot.dir,"fig02b_IAVhist_SIGR0.jpeg"), quality=100, width=900, height=700)

  p <- ggplot(aux %>% filter(SIGR==0 & FHIST != "f0"), aes(x=LHSC, y=IAVhist, fill=LHSC))+
    geom_bar(stat="identity", position="dodge")+
    facet_grid(FHIST ~ STKN)+
    theme(text = element_text(size = 20), 
          title = element_text(size = 16, face = "bold"), 
          strip.text = element_text(size = 20))
  
  print(p)

dev.off()

# - historical IAV

iavhist_sr0 <- aux %>% filter(SIGR == 0 & FHIST != "f0") %>% 
  mutate(IAVhist = round(IAVhist, 4)) %>%
  unique() %>% 
  spread(LHSC, IAVhist) %>% mutate
iavhist_sr0
#   STKN SIGR FHIST lowprod     bc highprod
# 1 STK1    0  flow  0.0301 0.0339   0.0347
# 2 STK1    0  fopt  0.0523 0.0514   0.0490
# 3 STK1    0 fhigh  0.0889 0.0725   0.0628
# 4 STK2    0  flow  0.0223 0.0259   0.0276
# 5 STK2    0  fopt  0.0417 0.0450   0.0464
# 6 STK2    0 fhigh  0.0769 0.0728   0.0711


# IAV ranges for each stock (for BC)
aux %>% filter(SIGR == 0.75) %>% 
  group_by(STKN) %>% 
  summarise(IAV.min = min(IAVhist), IAV.max = max(IAVhist))
#  STKN  IAV.min IAV.max
#  <chr>   <dbl>   <dbl>
# 1 STK1    0.374   0.844
# 2 STK2    0.180   0.420

dhist %>% filter(LHSC == "bc" & SIGR == 0.75 & FHIST != "f0" & CVID == "low") %>%
  mutate(IAV = round(IAV,2), 
         FHIST = factor(FHIST, levels=c("flow","fopt","fhigh"))) %>%
  select(STKN, LHSC, FHIST, IAV) %>% 
  spread(FHIST, IAV)
#   STKN LHSC flow fopt fhigh
# 1 STK1   bc 0.54 0.65  0.77
# 2 STK2   bc 0.24 0.30  0.38

iavproj <- df_bc %>% filter(indicator == "IAV" & ADVT == "iny" & BSAFE == "none") %>% 
  select(OMnam, term, HCR, UC, value) 

iavproj %>% spread(term, value)

iavproj %>% 
  group_by(OMnam, term) %>% 
  summarise(IAV.min = min(value), IAV.mean = mean(value), IAV.med = median(value), IAV.max = max(value))

iavproj %>% filter(term == "short") %>% 
  group_by(OMnam) %>% 
  summarise(IAV.min = min(value), IAV.mean = mean(value), IAV.med = median(value), IAV.max = max(value))
#   OMnam      IAV.min IAV.mean IAV.med IAV.max
#   <fct>        <dbl>    <dbl>   <dbl>   <dbl>
# 1 STK1_flow    0.505    0.554   0.552   0.627
# 2 STK1_fopt    0.590    0.651   0.663   0.722
# 3 STK1_fhigh   0.625    0.719   0.728   0.784
# 4 STK2_flow    0.237    0.256   0.249   0.305
# 5 STK2_fopt    0.295    0.347   0.330   0.455
# 6 STK2_fhigh   0.398    0.514   0.495   0.690
    

#==============================================================================
# TRAJECTORIES: 2-over-3 rule witb 20% ucaps
#==============================================================================

# - biomass and catch

dd <- dat_bioQ %>% filter(indicator %in% c("ssb","catch","Risk3.Blim","catch.MSY") & 
                            HCRT=="2o3" & UCPL==0.2 & UCPU==0.2 & BSAFE=="none")

aux <- dd %>% filter(LHSC == "bc" & SIGR == 0.75 & CVID == "low" & ADVT == "iny")

# lapply(aux, unique)

refpts <- list()
for (lh in unique(aux$LHnam))
  refpts[[lh]] <- loadToEnv(file.path(inp.dir,paste0(lh,"_dataLH.RData")))[["ref.pts"]]


# all iterations (wide format)

dd.its <- NULL

sc0 <- unique(dat_bio$scenario)[1]
nit.sample <- 2
it.sel <- sample( loadToEnv(file.path(ressc.dir,paste0("out_",sc0,".RData")))[[paste(sc0,"bio",sep="_")]] %>% 
                    .$iter %>% unique(), nit.sample)
#   6 466

for (sc in unique(aux$scenario)) {

  # specific scenario

  sc.its <- loadToEnv(file.path(ressc.dir,paste0("out_",sc,".RData")))[[paste(sc,"bio",sep="_")]]

  # specific iteration

  sc.its <- sc.its %>% filter(iter %in% it.sel) %>%
    select(scenario,stock,year,iter,one_of(c("ssb","catch"))) %>%
    mutate(iter = as.factor(iter))

  dd.its <- rbind(dd.its, sc.its)
  
  rm(sc.its)

}

# add information on scenario

dd.its <- dd.its %>% rename(SCENARIO = scenario) %>%
  left_join(sc.dat, by="SCENARIO") %>% select( names(sc.dat), names(dd.its[-1])) %>%
  mutate(FHIST = factor(FHIST, levels=c("flow","fopt","fhigh")))

# reshape to the long format for ggplot

aux2 <- reshape( dd.its, direction="long", varying=names(dd.its)[-c(1:20)], v.names=c("value"),
                 idvar=names(dd.its)[c(1:20)], timevar="indicator", times=names(dd.its)[-c(1:20)]) %>% 
  mutate(fhist = factor(FHIST, levels=c("flow","fopt","fhigh")))

jpeg(file.path(plot.dir,"fig04_trajectories_2o3_ucp20.jpeg"), quality=100, width=900, height=700)

  p <- ggplot(data = aux %>% filter(indicator %in% c("ssb","catch")), aes(x = year, y = q50)) + 
    geom_line() + 
    geom_ribbon(aes(x = year, ymin = q05, ymax = q95), alpha = 0.35) + 
    geom_line(data=aux2, aes(year, y=value, group=iter, col=iter))+
    facet_grid(FHIST ~ STKN + indicator) + 
    expand_limits(y=0) +
    geom_vline(xintercept = proj.yr - 0.5, linetype = "longdash") +
    geom_hline(lty = 2, data = data.frame(STKN = rep(unique(aux$STKN),each=3), 
                                          indicator = rep(c(rep("ssb",2),"catch"),length(unique(aux$STKN))),
                                          Bref = unlist(lapply(refpts, function(x) c(Blim=x[["Blim"]], Bcollapse=0.1*x[["B0"]],
                                                                                     MSY=x[["MSY"]])))),
               aes(yintercept = Bref), color = rep(c("orange", "red", "green"),6)) +
    theme_bw() + 
    theme(text = element_text(size = 20), 
          title = element_text(size = 16, face = "bold"), 
          strip.text = element_text(size = 20),
          legend.position = "none") + 
    ylab("tonnes") + 
    theme(plot.title = element_text(hjust = 0.5))
  print(p)

dev.off()


# risks 2o3 rule with 20% UC

risks0 <- df_bc %>% filter(LHSC == "bc" & SIGR == 0.75 & CVID == "low" & 
                             HCRT=="2o3" & UCPL==0.2 & UCPU==0.2 & BSAFE=="none" & 
                             ADVT == "iny" & indicator == "Risk3.Blim") %>% 
  rename(Risk3.Blim = value) %>% 
  select(OMnam, STKN, FHIST, ADVT, term, Risk3.Blim) 

risks0 %>% filter(term != "mid") %>% 
  select(OMnam, term, Risk3.Blim) %>% 
  spread(key = term, value = Risk3.Blim)
#        OMnam short  long
# 1  STK1_flow 0.164 0.094
# 2  STK1_fopt 0.344 0.197
# 3 STK1_fhigh 0.461 0.268
# 4  STK2_flow 0.007 0.017
# 5  STK2_fopt 0.121 0.190
# 6 STK2_fhigh 0.438 0.520

risks0 %>% group_by(STKN, ADVT, term) %>% 
  summarise(risk.min = min(Risk3.Blim), risk.max = max(Risk3.Blim))
#   STKN  ADVT  term  risk.min risk.max
#  <chr> <ord> <fct>    <dbl>    <dbl>
# 1 STK1  iny   short    0.164    0.461
# 2 STK1  iny   mid      0.169    0.472
# 3 STK1  iny   long     0.094    0.268
# 4 STK2  iny   short    0.007    0.438
# 5 STK2  iny   mid      0.012    0.469
# 6 STK2  iny   long     0.017    0.52 

# risks by OM

risks <- df_bc %>% filter(LHSC == "bc" & SIGR == 0.75 & CVID == "low" & BSAFE == "none" & 
                          indicator == "Risk3.Blim" & term == "short") %>% 
  rename(Risk3.Blim = value) %>% 
  select(OMnam, STKN, FHIST, ADVT, HCRT, UC, Risk3.Blim)

risks.f0 <- risks %>% filter(HCRT == "ft0") %>% 
  rename(Risk3.Blim_f0 = Risk3.Blim) %>% 
  select(OMnam, Risk3.Blim_f0)

risks <- risks %>% filter(ADVT == "iny") %>% right_join(risks.f0, by="OMnam")

risks %>% group_by(OMnam) %>% summarise(risk_min = min(Risk3.Blim), 
                                        risk_max = max(Risk3.Blim))
#   OMnam      risk_min risk_max
#   <fct>         <dbl>    <dbl>
# 1 STK1_flow     0.06     0.242
# 2 STK1_fopt     0.237    0.437
# 3 STK1_fhigh    0.423    0.625
# 4 STK2_flow     0.001    0.055
# 5 STK2_fopt     0.035    0.263
# 6 STK2_fhigh    0.319    0.585


risks %>% group_by(STKN) %>% summarise(risk_min = min(Risk3.Blim), 
                                       risk_max = max(Risk3.Blim))
#   STKN  risk_min risk_max
#   <chr>    <dbl>    <dbl>
# 1 STK1     0.06     0.625
# 2 STK2     0.001    0.585

# addedd risks for the 2o3 rule (to expected risks without catches)
risks.2o3 <- risks %>% filter(HCRT == "2o3") %>% 
  mutate(addRisk = Risk3.Blim - Risk3.Blim_f0) 

risks.2o3 %>% group_by(OMnam) %>% 
  summarise(f0Risk.min  = min(Risk3.Blim_f0), 
            f0Risk.max  = max(Risk3.Blim_f0),
            addRisk.min = min(addRisk), 
            addRisk.max = max(addRisk))

#   OMnam      f0Risk.min f0Risk.max addRisk.min addRisk.max
#   <fct>           <dbl>      <dbl>       <dbl>       <dbl>
# 1 STK1_flow       0.007      0.007       0.137       0.235
# 2 STK1_fopt       0.051      0.051       0.267       0.386
# 3 STK1_fhigh      0.137      0.137       0.315       0.488
# 4 STK2_flow       0          0           0.003       0.055
# 5 STK2_fopt       0.003      0.003       0.104       0.26 
# 6 STK2_fhigh      0.138      0.138       0.269       0.447

risks.2o3_ucp0.2 <- risks.2o3 %>% filter(UC == "(0.2,0.2)") %>% 
  select(OMnam, Risk3.Blim, Risk3.Blim_f0, addRisk)
risks.2o3_ucp0.2
#        OMnam Risk3.Blim Risk3.Blim_f0 addRisk
# 1  STK1_fopt      0.344         0.051   0.293
# 2  STK1_flow      0.164         0.007   0.157
# 3 STK1_fhigh      0.461         0.137   0.324
# 4  STK2_fopt      0.121         0.003   0.118
# 5  STK2_flow      0.007         0.000   0.007
# 6 STK2_fhigh      0.438         0.138   0.300


#==============================================================================
# comparison of several performance statistics 
#==============================================================================

# names and labels of performance statistics

perfnms <- c("Risk3.Blim","catch.MSY")

perflabels <- c("Risk3.Blim","catch/MSY")


#==============================================================================
# INTIAL RISKS: last historic year & short-term without catches 
#==============================================================================

# for in-year

aux <- df_bc %>% filter(indicator == "Risk3.Blim" & ADVT %in% c("fix","iny")) %>% 
  rename(Risk3.Blim = value) %>% select(-c(indicator, OM:MP, SIGR, CVFH:ADVT, PBUF:Ratio))

aux_st <- aux %>% filter(term == "short") %>% select(-term)
aux_lt <- aux %>% filter(term == "long") %>% select(-term)

rf0 <- aux %>% filter(HCRT == "ft0" & term != "mid") %>%
  select(-c(SCENARIO, depl.ini:RiskBlim.ini, HCRT:OMnam)) %>% 
  rename(Risk3.Blim_f0 = Risk3.Blim)

rf0_st <- rf0 %>% filter(term == "short") %>% select(-term) %>% rename(Risk3.Blim_f0_st = Risk3.Blim_f0)
rf0_lt <- rf0 %>% filter(term == "long") %>% select(-term) %>% rename(Risk3.Blim_f0_lt = Risk3.Blim_f0)
rf0 <- full_join(rf0_st, rf0_lt, by = c("STKN", "LHSC", "FHIST"))
  
aux <- aux_lt %>% filter(HCRT != "ft0") %>% left_join(rf0, by = c("STKN", "LHSC", "FHIST"))

init.risks <- aux %>% select(STKN, FHIST, RiskBlim.ini, Risk3.Blim_f0_st, Risk3.Blim_f0_lt) %>% 
  unique() %>% arrange(STKN, FHIST)
init.risks
#   STKN FHIST RiskBlim.ini Risk3.Blim_f0_st Risk3.Blim_f0_lt
# 1 STK1  flow        0.018            0.007                0
# 2 STK1  fopt        0.124            0.051                0
# 3 STK1 fhigh        0.400            0.137                0
# 4 STK2  flow        0.000            0.000                0
# 5 STK2  fopt        0.012            0.003                0
# 6 STK2 fhigh        0.302            0.138                0

write.csv(init.risks, file=file.path(plot.dir,"tab2_initRisks.csv"), row.names=F)


# initial catches
dat_bio %>% select(-SCnam) %>% 
  filter( year==30 & STKN == "STK1" & HCRI != "fix" & ADVT != "iny") %>% print(n=250)
dat_bio %>% ungroup() %>% 
  select(-scenario, -SCnam, -OM, -MP, -LHnam) %>% 
  filter( year==30 & STKN == "STK1" & HCRI != "fix") %>%
  select(-Risk.Blim, -(LHSC:CVID), -year, - STKN, -(PBUF:UCPU)) %>% 
  group_by(OMnam, ADVT) %>% 
  summarise(catch.MSY_min = min(catch.MSY), catch.MSY_max = max(catch.MSY))
#   OMnam      ADVT  catch.MSY_min catch.MSY_max
#   <fct>      <chr>         <dbl>         <dbl>
# 1 STK1_flow  fpa           0.811         0.811
# 2 STK1_flow  int           0.811         0.811
# 3 STK1_flow  iny           0.797         0.890
# 4 STK1_fopt  fpa           1.02          1.02 
# 5 STK1_fopt  int           1.02          1.02 
# 6 STK1_fopt  iny           1.01          1.06 
# 7 STK1_fhigh fpa           0.969         0.969
# 8 STK1_fhigh int           0.969         0.969
# 9 STK1_fhigh iny           0.910         0.962

# Taking input data for: om001 - STK1	bc	0.75	fopt	0.1	b1p	low
stk1_fl <- loadToEnv(file.path(inp.dir,"iters","data_om001.RData"))[["fleets"]]
chist   <- iterMedians(seasonSums(quantSums(catchWStock(stk1_fl, "STK1"))))

refpts <- loadToEnv(file.path(inp.dir,"iters","data_om001.RData"))[["advice.ctrl"]]
msy    <- mean(refpts$STK1$ref.pts["MSY",])

# mean of last 5 years
mean(chist[,26:30,])
mean(chist[,26:30,]) / msy # 1.033393

# mean of last year
mean(chist[,30,])
mean(chist[,30,]) / msy # 1.020372

# jpeg(file.path(plot.dir,"fig_initRisk_allOMs.jpeg",sep=""), quality=100, width=1400, height=700)
# # jpeg(file.path(plot.dir,"fig_initRisk_allOMs_freeScale.jpeg",sep=""), quality=100, width=1400, height=700)
# 
#   p <- ggplot(aux_lt, aes(x=UC, y=Risk3.Blim, fill=UC))+
#     geom_bar(stat="identity")+
#     facet_grid(OMnam ~ HCRT)+ #, scales="free"
#     ylab("Risk3.Blim")+
#     scale_fill_manual(values = ucp.col)+
#     geom_hline(yintercept = 0.05, linetype = "longdash")+
#     geom_hline(aes(yintercept=Risk3.Blim_f0), data = aux, linetype = "dotdash")+
#     geom_hline(aes(yintercept=RiskBlim.ini), data = aux)+
#     ylim(c(0,1))+ # comment for free scales
#     theme(axis.text.x=element_blank(), 
#           text = element_text(size = 20), 
#           title = element_text(size = 16, face = "bold"), 
#           strip.text = element_text(size = 20))
#   
#   print(p)
# 
# dev.off()


#==============================================================================
# PLOTS BC
#==============================================================================

  #----------------------------------------------
  # comparison of different rules
  #----------------------------------------------
  
  # effect of the calendar
  
  # library(RColorBrewer)
  
  jpeg(file.path(plot.dir,"fig03_ryield_vs_risk_allOMs_st&lt.jpeg",sep=""), quality=100, width=1400, height=700)
  
    aux <- df_bc %>% filter(term %in% c("short","long") & ADVT!= "fix" & BSAFE == "none" & indicator %in% perfnms)
    
    # lapply(aux, unique)
    
    p <- ggplot(aux, aes(x=ADVT, y=value, group=interaction(HCRT,BSAFE,PBUF,UC,HCRI), col=UC, lty=HCRT))+
      geom_line()+
      geom_hline(aes(yintercept = 0.05), data = subset(aux, indicator == "Risk3.Blim"), linetype="dashed") +
      facet_grid(indicator + term ~ OMnam)+ #, scales="free"
      scale_colour_manual(values = ucp.col)+
      ylab("")+
      theme(text = element_text(size = 20), 
            title = element_text(size = 16, face = "bold"), 
            strip.text = element_text(size = 20))
    print(p)
  
  dev.off()
  
  jpeg(file.path(plot.dir,"fig03b_ryield_vs_risk_allOMs.jpeg",sep=""), quality=100, width=1400, height=700)
  
    aux <- df_bc %>% filter(term == "long" & ADVT!= "fix" & BSAFE == "none" & indicator %in% perfnms)
    
    # lapply(aux, unique)
    
    p <- ggplot(aux, aes(x=ADVT, y=value, group=interaction(HCRT,BSAFE,PBUF,UC,HCRI), col=UC, lty=HCRT))+
      geom_line()+
      geom_hline(aes(yintercept = 0.05), data = subset(aux, indicator == "Risk3.Blim"), linetype="dashed") +
      facet_grid(indicator ~ OMnam, scales="free")+
      scale_colour_manual(values = ucp.col)+
      ylab("")+
      theme(text = element_text(size = 20), 
            title = element_text(size = 16, face = "bold"), 
            strip.text = element_text(size = 20))
    print(p)
  
  dev.off()
  
  
  #----------------------------------------------
  # Comparing numeric values
  #----------------------------------------------
  
  # RISK
  
  # calendar checks
  caldf <- df_bc %>% filter(indicator=="Risk3.Blim" & term != "mid" & HCRT != "ft0" & BSAFE == "none") %>% 
    select(term, OMnam, STKN, FHIST, UC, ADVT, HCRT, value) %>% 
    tidyr::spread("ADVT", "value") %>% 
    mutate(iny_int = iny - int, 
           fpa_int = fpa - int, 
           fpa_iny = fpa - iny)
  
  # - differences contrary to expected (hisgher risks)
  caldf %>% filter(iny_int >=0.01 & term == "short")
  caldf %>% filter(fpa_int >=0.01 & term == "short")
  caldf %>% filter(fpa_iny >=0.01 & term == "short")
  caldf %>% filter(iny_int >=0.01 & term == "long")
  caldf %>% filter(fpa_int >=0.01 & term == "long")
  caldf %>% filter(fpa_iny >=0.01 & term == "long")
  
  caldf %>% group_by(term, OMnam, FHIST) %>% 
    summarise(iny_int = max(iny_int), fpa_int = max(fpa_int), fpa_iny = max(fpa_iny))
  
  caldf %>% filter(iny_int >=0.01 & term == "short" & (int <= 0.05 | iny <= 0.05 | fpa <= 0.05))
  caldf %>% filter(fpa_int >=0.01 & term == "short" & (int <= 0.05 | iny <= 0.05 | fpa <= 0.05))
  caldf %>% filter(fpa_iny >=0.01 & term == "short" & (int <= 0.05 | iny <= 0.05 | fpa <= 0.05))
  #    term     OMnam STKN FHIST         UC HCRT   int   iny   fpa iny_int fpa_int fpa_iny
  # 1 short STK2_fopt STK2  fopt (0.8,2.75)  1o2 0.211 0.045 0.059  -0.166  -0.152   0.014
  # 2 short STK2_fopt STK2  fopt    (0.8,4)  1o2 0.215 0.045 0.060  -0.170  -0.155   0.015
  # 3 short STK2_fopt STK2  fopt (0.8,5.25)  1o2 0.215 0.045 0.060  -0.170  -0.155   0.015
  # 4 short STK2_fopt STK2  fopt    (NA,NA)  1o2 0.213 0.045 0.060  -0.168  -0.153   0.015
  caldf %>% filter(iny_int >=0.01 & term == "long" & (int <= 0.05 | iny <= 0.05 | fpa <= 0.05))
  caldf %>% filter(fpa_int >=0.01 & term == "long" & (int <= 0.05 | iny <= 0.05 | fpa <= 0.05))
  caldf %>% filter(fpa_iny >=0.01 & term == "long" & (int <= 0.05 | iny <= 0.05 | fpa <= 0.05))
  #   term      OMnam STKN FHIST      UC HCRT   int   iny   fpa iny_int fpa_int fpa_iny
  # 1 long STK1_fhigh STK1 fhigh (NA,NA)  1o2 0.076 0.008 0.037  -0.068  -0.039   0.029
  # 2 long STK1_fhigh STK1 fhigh (NA,NA)  1o3 0.035 0.005 0.023  -0.030  -0.012   0.018
  # 3 long STK2_fhigh STK2 fhigh (NA,NA)  1o2 0.112 0.020 0.044  -0.092  -0.068   0.024
  
  # - in-year calendar
  caldf %>% filter(iny_int >=0.01 & term == "short") %>% .$UC %>% unique()
  caldf %>% filter(iny_int >=0.01 & term == "short") %>% .$HCRT %>% unique()
  caldf %>% filter(iny_int >=0.01 & term == "short" & HCRT != "2o3")             # STK1_fhigh
  caldf %>% filter(iny_int >=0.01 & term == "short" & !HCRT %in% c("2o3","1o5")) # STK1_fhigh & never (0.8,0.8)
  caldf %>% filter(iny_int >=0.01 & term == "long") %>% .$UC %>% unique()        # (0.5,1.5) & (0.5,1)
  caldf %>% filter(iny_int >=0.01 & term == "long") %>% .$HCRT %>% unique()      # 2o3
  
  # - differences by FHIST
  caldf %>% filter(term == "short") %>% 
    group_by(FHIST) %>% summarise(iny_int.min = min(iny_int), iny_int.max = max(iny_int), 
                                  fpa_int.min = min(fpa_int), fpa_int.max = max(fpa_int), 
                                  fpa_iny.min = min(fpa_iny), fpa_iny.max = max(fpa_iny))
  #   FHIST iny_int.min iny_int.max fpa_int.min fpa_int.max fpa_iny.min fpa_iny.max
  #   <fct>       <dbl>       <dbl>       <dbl>       <dbl>       <dbl>       <dbl>
  # 1 flow       -0.13       -0.011      -0.153      -0.011     -0.0600       0.001
  # 2 fopt       -0.181      -0.003      -0.201      -0.03      -0.096        0.023
  # 3 fhigh      -0.134       0.074      -0.158       0.002     -0.105        0.017
  
  caldf %>% filter(term == "short") %>% 
    group_by(STKN, FHIST) %>% summarise(iny_int.min = min(iny_int), iny_int.max = max(iny_int))
  #   STKN  FHIST iny_int.min iny_int.max
  #   <chr> <fct>       <dbl>       <dbl>
  # 1 STK1  flow       -0.13      -0.028 
  # 2 STK1  fopt       -0.074     -0.003 
  # 3 STK1  fhigh      -0.018      0.074 
  # 4 STK2  flow       -0.1       -0.011 
  # 5 STK2  fopt       -0.181     -0.043 
  # 6 STK2  fhigh      -0.134      0.0250 
  
  caldf %>% filter(term == "long") %>% 
    group_by(FHIST) %>% summarise(iny_int.min = min(iny_int), iny_int.max = max(iny_int), 
                                  fpa_int.min = min(fpa_int), fpa_int.max = max(fpa_int), 
                                  fpa_iny.min = min(fpa_iny), fpa_iny.max = max(fpa_iny))
  #   FHIST iny_int.min iny_int.max fpa_int.min fpa_int.max fpa_iny.min fpa_iny.max
  #   <fct>       <dbl>       <dbl>       <dbl>       <dbl>       <dbl>       <dbl>
  # 1 flow       -0.273      0.0380      -0.307      -0.001     -0.0960      0.0210
  # 2 fopt       -0.434      0.0470      -0.469       0.021     -0.171       0.0390
  # 3 fhigh      -0.429      0.035       -0.476       0.02      -0.208       0.039 
  
  
  # RELATIVE YIELDS
  
  # calendar checks
  caldf <- df_bc %>% filter(indicator=="catch.MSY" & term != "mid" & HCRT != "ft0" & BSAFE == "none") %>%
    select(term, OMnam, STKN, FHIST, UC, ADVT, HCRT, value) %>%
    tidyr::spread("ADVT", "value") %>%
    mutate(iny_int = iny - int,
           fpa_int = fpa - int,
           fpa_iny = fpa - iny)
  
  # - differences contrary to expected (less relative yields)
  caldf %>% filter(iny_int <=0.01 & term == "short")
  caldf %>% filter(fpa_int <=0.01 & term == "short")
  caldf %>% filter(fpa_iny <=0.01 & term == "short")
  caldf %>% filter(iny_int <=0.01 & term == "long")
  caldf %>% filter(fpa_int <=0.01 & term == "long")
  caldf %>% filter(fpa_iny <=0.01 & term == "long")
  
  # - in-year calendar
  caldf %>% filter(iny_int <=0.01 & term == "short") %>% .$UC %>% unique()
  caldf %>% filter(iny_int <=0.01 & term == "short") %>% .$HCRT %>% unique()
  caldf %>% filter(iny_int <=0.01 & term == "short" & HCRT != "2o3")
  caldf %>% filter(iny_int <=0.01 & term == "short" & !HCRT %in% c("2o3","1o5"))
  caldf %>% filter(iny_int <=0.01 & term == "long") %>% .$UC %>% unique()
  caldf %>% filter(iny_int <=0.01 & term == "long") %>% .$HCRT %>% unique()
  caldf %>% filter(iny_int <=0.01 & term == "long" & HCRT != "2o3")
  caldf %>% filter(iny_int <=0.01 & term == "long" & !HCRT %in% c("2o3","1o5"))
  
  # - differences by FHIST
  caldf %>% filter(term == "long") %>%
    group_by(FHIST) %>% summarise(iny_int.min = min(iny_int), iny_int.max = max(iny_int),
                                  fpa_int.min = min(fpa_int), fpa_int.max = max(fpa_int),
                                  fpa_iny.min = min(fpa_iny), fpa_iny.max = max(fpa_iny))
  
  
  # RISKs comparison by term
  
  risk_short <- df_bc %>% filter(indicator=="Risk3.Blim" & term == "short" & HCRT != "ft0" & BSAFE == "none" & 
                                   ADVT == "iny") %>% 
    select(STKN, FHIST, UC, ADVT, HCRT, value) %>% 
    tidyr::spread(STKN, value) %>% 
    mutate(diff = STK1-STK2)
  risk_short
  risk_mid <- df_bc %>% filter(indicator=="Risk3.Blim" & term == "mid" & HCRT != "ft0" & BSAFE == "none" 
                               & ADVT == "iny") %>% 
    select(STKN, FHIST, UC, ADVT, HCRT, value) %>% 
    tidyr::spread(STKN, value) %>% 
    mutate(diff = STK1-STK2)
  risk_mid 
  risk_long <- df_bc %>% filter(indicator=="Risk3.Blim" & term == "long" & HCRT != "ft0" & BSAFE == "none" 
                                & ADVT == "iny") %>% 
    select(STKN, FHIST, UC, ADVT, HCRT, value) %>% 
    tidyr::spread(STKN, value) %>% 
    mutate(diff = STK1-STK2)
  risk_long 
  
  
  
  
  # effect of rule and uncertainty caps (in-year advice)
  
  for (ind in perfnms){
    
    if (ind == "Risk3.Blim") {
      jpeg(file.path(plot.dir,paste("fig05_compareOMs_risk.jpeg",sep="")), quality=100, width=1400, height=700)
    } else if (ind == "catch.MSY")
      jpeg(file.path(plot.dir,paste("fig07_compareOMs_ryield.jpeg",sep="")), quality=100, width=1400, height=700)
    
    aux <- subset(df_bc, indicator %in% ind & term %in% c("short", "long"))
    
    # aux.fix <- aux %>% subset(ADVT=="fix") %>% select(-HCRT)
    # rules <- unique(aux$HCRT)[unique(aux$HCRT)!="ft0"]
    # aux.fix <- aux.fix[rep(1:nrow(aux.fix),times = length(rules)),]
    # aux.fix$HCRT <- rep(rules, each=nrow(aux.fix)/length(rules))
    # 
    # aux.fix_lt <- aux.fix %>% subset(term == "long")
    
    aux_st <- aux %>% subset(HCRT != "ft0" & BSAFE == "none" & ADVT == "iny" & term == "short")
    aux_lt <- aux %>% subset(HCRT != "ft0" & BSAFE == "none" & ADVT == "iny" & term == "long")
    
    lapply(aux_lt, unique)
    
    p <- ggplot(aux_lt, aes(x=UC, y=value, fill=UC))+
      geom_bar(stat="identity")+
      facet_grid(OMnam ~ HCRT)+
      ylab(perflabels[perfnms==ind])+
      scale_fill_manual(values = ucp.col)+
      geom_point(aes(x=UC, y=value), data = aux_st, colour = "black")+
      theme(axis.text.x=element_blank(), 
            text = element_text(size = 20), 
            title = element_text(size = 16, face = "bold"), 
            strip.text = element_text(size = 20))
    
    if(length(grep("Risk", ind))>0) {
      p <- p + geom_hline(yintercept = 0.05, linetype = "longdash")
      # p <- p + geom_hline(aes(yintercept = value), data = subset(aux.fix_lt, indicator == "Risk3.Blim"), col="red")
      p <- p + ylim(c(0,1))
    }
    
    if (ind == "catch.MSY")
      p <- p + geom_hline(yintercept = 1, linetype = "longdash")
    
    print(p)
    
    dev.off()
  }
  
  # tabulated results
  sumres <- df_bc %>% filter(indicator %in% c("Risk3.Blim","catch.MSY","IAV") & HCRT != "ft0" & 
                                 BSAFE == "none") %>% 
    mutate(indicator = ordered(indicator, levels=c("Risk3.Blim","catch.MSY","IAV"))) %>% 
    select(STKN, FHIST, term, ADVT, HCRT, UC, indicator, value) %>% 
    # mutate(value = round(value,2)) %>% 
    tidyr::spread(indicator, value)
  
  write.csv(sumres, file=file.path(plot.dir,"tabXX_summary_risk&ryield_iav.csv"), row.names=F)


#==============================================================================
# TRAJECTORIES: n-over-m rules
#==============================================================================

# - risks

aux <- dat_bio %>% filter(LHSC == "bc" & SIGR == 0.75 & CVID == "low" & ADVT == "iny" & 
                            HCRT != "ft0" & BSAFE == "none" & HCRI == "nin") %>% 
    select(-LHSC, -SIGR, -CVID, -ADVT, -HCRI) %>% 
    mutate(OMnam = factor(OMnam, levels = OMnam.levels))

jpeg(file.path(plot.dir,"fig06_trajectories_risks.jpeg"), quality=100, width=1400, height=700)
  
  p <- ggplot(data = aux, aes(x = year, y = Risk.Blim, col=UC)) +
    geom_line() +
    facet_grid(OMnam ~ HCRT, scales = "free") +
    scale_color_manual(values = ucp.col)+
    # geom_vline(xintercept = c(35.5, 50.5), linetype = "longdash") +
    geom_hline(yintercept = 0.05, linetype = "longdash") +
    theme_bw() +
    theme(text = element_text(size = 20),
          title = element_text(size = 16, face = "bold"),
          strip.text = element_text(size = 20))
  
  # p <- ggplot(aux_lt, aes(x=UC, y=value, fill=UC))
  
  print(p)
  
dev.off()

jpeg(file.path(plot.dir,"fig06_sameScale_trajectories_risks.jpeg"), quality=100, width=1400, height=700)

  p <- ggplot(data = aux, aes(x = year, y = Risk.Blim, col=UC)) +
    geom_line() +
    facet_grid(OMnam ~ HCRT) +
    scale_color_manual(values = ucp.col)+
    # geom_vline(xintercept = c(35.5, 50.5), linetype = "longdash") +
    geom_hline(yintercept = 0.05, linetype = "longdash") +
    theme_bw() +
    theme(text = element_text(size = 20),
          title = element_text(size = 16, face = "bold"),
          strip.text = element_text(size = 20))
  
  # p <- ggplot(aux_lt, aes(x=UC, y=value, fill=UC))
  
  print(p)

dev.off()


df_bc %>% 
  filter(indicator=="Risk3.Blim" & term == "short" & HCRT == "1o3" & ADVT == "iny" & UC == "(0.2,0.2)") %>% 
  select(STKN, FHIST, value) %>% 
  group_by(STKN) %>% 
  summarise(risk.min = round(min(value),2), risk.max = round(max(value),2))

#  STKN  risk.min risk.max
#   <chr>    <dbl>    <dbl>
# 1 STK1      0.14     0.45
# 2 STK2      0.01     0.39

  
# - catch.MSY

jpeg(file.path(plot.dir,paste("fig06b_trajectories_ryield.jpeg",sep="")), quality=100, width=1400, height=700)
  
  p <- ggplot(data = aux, aes(x = year, y = catch.MSY, col=UC)) +
    geom_line() +
    facet_grid(OMnam ~ HCRT, scales = "free") +
    scale_color_manual(values = ucp.col)+
    # geom_vline(xintercept = c(35.5, 50.5), linetype = "longdash") +
    geom_hline(yintercept = 1, linetype = "longdash") +
    theme_bw() +
    theme(text = element_text(size = 20),
          title = element_text(size = 16, face = "bold"),
          strip.text = element_text(size = 20))
  
  print(p)
  
dev.off()





#==============================================================================
# PLOTS sens CIVID
#==============================================================================
  
jpeg(file.path(plot.dir,paste("fig10b_sensCIVD_compareOMs.jpeg",sep="")), quality=100, width=1400, height=700)
  
  aux <- subset(df_cvid, indicator %in% perfnms & term == "long" & LHSC == "bc" & SIGR == 0.75) %>% 
    mutate(rule = paste(HCRT,UC,sep="_"))
  
  # lapply(aux, unique)
  
  p <- ggplot(aux, aes(x=CVIndex, y=value, col=HCRT))+
    geom_line(aes(linetype=UC), size = 1.75)+
    geom_point(aes(shape=HCRT), size = 2, col="black", stroke = 2)+geom_point(aes(shape=HCRT), size = 2, col="black", stroke = 2)+
    geom_hline(lty = 2, data = data.frame(indicator = "Risk3.Blim", ref = 0.05), aes(yintercept = ref))+
    facet_grid(indicator ~ OMnam, scales="free")+
    scale_fill_brewer(palette="Paired", type = "qual")+
    ylab("")+
    theme(text = element_text(size = 20), 
          title = element_text(size = 16, face = "bold"), 
          strip.text = element_text(size = 20), 
          legend.key.size = grid::unit(2.5,"lines"))
  print(p)
  
dev.off()
  
jpeg(file.path(plot.dir,paste("fig10_sensCIVD_compareOMs_st&lt.jpeg",sep="")), quality=100, width=1400, height=700)
  
  aux <- subset(df_cvid, indicator %in% perfnms & term %in% c("short", "long") & LHSC == "bc" & SIGR == 0.75) %>% 
    mutate(rule = paste(HCRT,UC,sep="_"))
  
  # lapply(aux, unique)
  
  p <- ggplot(aux, aes(x=CVIndex, y=value, col=HCRT))+
    geom_line(aes(linetype=UC), size = 1.75)+
    geom_point(aes(shape=HCRT), size = 2, col="black", stroke = 2)+geom_point(aes(shape=HCRT), size = 2, col="black", stroke = 2)+
    geom_hline(lty = 2, data = data.frame(indicator = "Risk3.Blim", ref = 0.05), aes(yintercept = ref))+
    facet_grid(indicator + term ~ OMnam, scales="free")+
    scale_fill_brewer(palette="Paired", type = "qual")+
    ylab("")+
    theme(text = element_text(size = 20), 
          title = element_text(size = 16, face = "bold"), 
          strip.text = element_text(size = 20), 
          legend.key.size = grid::unit(2.5,"lines"))
  print(p)
  
dev.off()


# - IAV

jpeg(file.path(plot.dir,paste("fig11_IAVproj.jpeg",sep="")), quality=100, width=1400, height=700)

  aux <- subset(df_cvid, indicator == "Risk3.Blim" & term != "mid" & UCPL == 0 & UCPU == 0 & 
                  SIGR == 0.75 & LHSC == "bc") # for easier interpretation
  
  p <- ggplot(aux, aes(x=IAVproj, y=value, col=OMnam, shape=HCRT))+
    geom_point(size=3)+
    facet_grid(term ~ CVID)+
    # ylim(c(0,1))+
    xlab("IAVproj")+
    ylab("Risk3.Blim")+
    theme(text = element_text(size = 20), 
          title = element_text(size = 16, face = "bold"), 
          strip.text = element_text(size = 20))
  print(p)

dev.off()


jpeg(file.path(plot.dir,paste("fig11b_IAVproj.jpeg",sep="")), quality=100, width=1400, height=700)

  aux <- subset(df_cvid, indicator == "Risk3.Blim" & term != "mid" & UCPL == 0 & UCPU == 0 & 
                  SIGR == 0.75 & LHSC == "bc") # for easier interpretation
  
  p <- ggplot(aux, aes(x=IAVproj, y=value, col=OMnam, shape=HCRT))+
    geom_point(size=3)+
    facet_grid(term ~ .)+
    # ylim(c(0,1))+
    xlab("IAVproj")+
    ylab("Risk3.Blim")+
    theme(text = element_text(size = 20), 
          title = element_text(size = 16, face = "bold"), 
          strip.text = element_text(size = 20))
  print(p)

dev.off()



#==============================================================================
# PLOTS sens OM
#==============================================================================

aux <- subset(df_om, indicator %in% perfnms & term == "long" & CVID == "low") %>% 
  mutate(rule = paste(HCRT,UC,sep="_"), 
         LHSC_SIGR = paste(LHSC,SIGR,sep="_"), 
         LHSC_SIGR = factor(as.character(LHSC_SIGR), 
                            levels = c(outer(levels(aux$LHSC), levels(aux$SIGR), paste, sep="_"))))

jpeg(file.path(plot.dir,paste("fig12_risk_sensOM_byLHSC&SIGR.jpeg",sep="")), quality=100, width=1400, height=700)

  p <- ggplot(aux %>% filter(indicator == "Risk3.Blim"), aes(x=SIGR, y=value, fill=LHSC))+
    geom_bar(stat="identity", position="dodge")+
    geom_hline(yintercept = 0.05, linetype = "longdash")+
    facet_grid(rule ~ OMnam)+
    ylab("Risk3.Blim")+
    scale_fill_brewer(palette="Paired", type = "qual")+
    theme(text = element_text(size = 20), 
          title = element_text(size = 16, face = "bold"), 
          strip.text = element_text(size = 20))
  
  print(p)

dev.off()

jpeg(file.path(plot.dir,paste("fig13_ryield_sensOM_byLHSC&SIGR.jpeg",sep="")), quality=100, width=1400, height=700)

  p <- ggplot(aux %>% filter(indicator == "catch.MSY"), aes(x=SIGR, y=value, fill=LHSC))+
    geom_bar(stat="identity", position="dodge")+
    geom_hline(yintercept = 1, linetype = "longdash")+
    facet_grid(rule ~ OMnam)+
    ylab("catch.MSY")+
    scale_fill_brewer(palette="Paired", type = "qual")+
    theme(text = element_text(size = 20), 
          title = element_text(size = 16, face = "bold"), 
          strip.text = element_text(size = 20))
  
  print(p)

dev.off()


#==============================================================================
# Best rule
#==============================================================================

aux <- df_bc %>% subset(indicator %in% perfnms & 
                LHSC == "bc" & SIGR == 0.75 & CVID == "low" & ADVT == "iny" & HCRT != "ft0") %>% 
  select(term, STKN, FHIST, OMnam, HCRT, BSAFE, UC, indicator, value) %>% 
  tidyr::spread(indicator, value)


aux %>% filter(term == "long") %>% 
  group_by(FHIST, HCRT, UC) %>% 
  summarise(risk.min = min(Risk3.Blim), risk.max = max(Risk3.Blim)) %>% 
  as.data.frame()

# lprec.all <- aux %>% filter(term == "long" & FHIST == "fhigh" & STKN == "STK2" & Risk3.Blim <= 0.05)
#   
#   select(-LHSC, -SIGR, -CVID, -(CVFH:ADVT), -PBUF, -HCRI, -CVIndex)
#   mutate(rule = paste(HCRT,UC,sep="_"), 
#          LHSC_SIGR = paste(LHSC,SIGR,sep="_"), 
#          LHSC_SIGR = factor(as.character(LHSC_SIGR), 
#                             levels = c(outer(levels(aux$LHSC), levels(aux$SIGR), paste, sep="_"))))

# # select only 2o3_(0.2,0.2)
# dd <- aux %>% filter(term == "long" & ( HCRT != "2o3" | (HCRT == "2o3" & UC == "0.2_0.2")))

# discard caps with very high long risks: UC(0.2,0.2), UC(0.2,0.25), UC(0.5,1.0), UC(0.5,1.5)

dd <- aux %>% filter( BSAFE == "none" & #term == "long" & 
                        !UC %in% c("(0.2,0.2)", "(0.2,0.25)", "(0.5,1)", "(0.5,1.5)") )

jpeg(file.path(plot.dir,paste("fig08_risk_vs_ryield.jpeg",sep="")), quality=100, width=1400, height=700)

  p <- ggplot(dd, aes(x=catch.MSY, y=Risk3.Blim, shape=HCRT, colour=UC))+
    geom_point(size=3)+
    geom_hline(yintercept = 0.05, linetype = "longdash")+
    facet_grid(term ~ OMnam, scales = "free_y")+
    scale_colour_manual(values = ucp.col2)+
    theme(text = element_text(size = 20), 
          title = element_text(size = 16, face = "bold"), 
          strip.text = element_text(size = 20))
  
  print(p)

dev.off()


dd <- aux %>% filter( BSAFE == "none" & term != "mid" & #term == "long" & 
                        !UC %in% c("(0.2,0.2)", "(0.2,0.25)", "(0.5,1)", "(0.5,1.5)") )

jpeg(file.path(plot.dir,paste("fig08b_risk_vs_ryield.jpeg",sep="")), quality=100, width=1400, height=700)

  p <- ggplot(dd, aes(x=catch.MSY, y=Risk3.Blim, shape=HCRT, colour=UC))+
    geom_point(size=3)+
    geom_hline(yintercept = 0.05, linetype = "longdash")+
    facet_grid(term ~ OMnam, scales = "free_y")+
    scale_colour_manual(values = ucp.col2)+
    theme(text = element_text(size = 20), 
          title = element_text(size = 16, face = "bold"), 
          strip.text = element_text(size = 20))
  
  print(p)

dev.off()


# without 2o3 

dd <- aux %>% filter( HCRT != "2o3" & BSAFE == "none" & #term == "long" & 
                        !UC %in% c("(0.2,0.2)", "(0.2,0.25)", "(0.5,1)", "(0.5,1.5)") )

jpeg(file.path(plot.dir,paste("fig08c_risk_vs_ryield.jpeg",sep="")), quality=100, width=1400, height=700)

  p <- ggplot(dd, aes(x=catch.MSY, y=Risk3.Blim, shape=HCRT, colour=UC))+
    geom_point(size=3)+
    geom_hline(yintercept = 0.05, linetype = "longdash")+
    facet_grid(term ~ OMnam, scales = "free_y")+
    scale_colour_manual(values = ucp.col2)+
    theme(text = element_text(size = 20), 
          title = element_text(size = 16, face = "bold"), 
          strip.text = element_text(size = 20))
  
  print(p)

dev.off()


# - effect of biomass safeguard: 

aux <- df_bc %>% subset(indicator %in% perfnms & LHSC == "bc" & SIGR == 0.75 & CVID == "low" & ADVT == "iny" & 
                          HCRT != "ft0") %>% 
  select(term, STKN, FHIST, OMnam, HCRT, BSAFE, HCR, UC, indicator, value) %>% 
  tidyr::spread(indicator, value)

  # restrict to BSAFE cases
  
  rls  <- grep( "_I", unique(aux$HCR), value = TRUE) %>% substr(1,3) %>% unique()
  ucps <- aux %>% filter(grepl( "_I", HCR)) %>% .$UC %>% unique()
  
  aux <- aux %>% filter(HCRT %in% rls & UC %in% ucps)
  
  # # 1o2 rule with UC(NA,NA), UC(0.5,0.5), (0.8,0.8). (0.8,4)
  # dd <- aux %>% filter( HCRT == "1o2" & UC %in% c("(NA,NA)", "(0.5,0.5)", "(0.8,0.8)", "(0.8,4)") )
  
  jpeg(file.path(plot.dir,paste("fig09_risk_vs_ryield_1o2bsafe.jpeg",sep="")), quality=100, width=1400, height=700)
  
    dd <- aux %>% filter(HCRT == "1o2" & term != "mid")
    
    p <- ggplot(dd, aes(x=catch.MSY, y=Risk3.Blim, shape=HCR, colour=UC))+
      geom_point(size=3)+
      geom_hline(yintercept = 0.05, linetype = "longdash")+
      facet_grid(term ~ OMnam, scales = "free_y")+
      scale_colour_manual(values = ucp.col3)+
      theme(text = element_text(size = 20), 
            title = element_text(size = 16, face = "bold"), 
            strip.text = element_text(size = 20))
    
    print(p)
  
  dev.off()
  
  jpeg(file.path(plot.dir,paste("fig09b_risk_vs_ryield_1o2bsafe.jpeg",sep="")), quality=100, width=1400, height=700)
  
    dd <- aux %>% filter(HCRT == "1o2") #!
    
    p <- ggplot(dd, aes(x=catch.MSY, y=Risk3.Blim, shape=HCR, colour=UC))+
      geom_point(size=3)+
      geom_hline(yintercept = 0.05, linetype = "longdash")+
      facet_grid(term ~ OMnam, scales = "free_y")+
      scale_colour_manual(values = ucp.col3)+
      theme(text = element_text(size = 20), 
            title = element_text(size = 16, face = "bold"), 
            strip.text = element_text(size = 20))
    
    print(p)
  
  dev.off()
  
  # 2o3 rule
  
  jpeg(file.path(plot.dir,paste("fig09c_risk_vs_ryield_2o3bsafe.jpeg",sep="")), quality=100, width=1400, height=700)
  
    dd <- aux %>% filter(HCRT == "2o3" & term != "mid")
    
    p <- ggplot(dd, aes(x=catch.MSY, y=Risk3.Blim, shape=HCR, colour=UC))+
      geom_point(size=3)+
      geom_hline(yintercept = 0.05, linetype = "longdash")+
      facet_grid(term ~ OMnam, scales = "free_y")+
      scale_colour_manual(values = ucp.col3)+
      theme(text = element_text(size = 20), 
            title = element_text(size = 16, face = "bold"), 
            strip.text = element_text(size = 20))
    
    print(p)
  
  dev.off()
  
  jpeg(file.path(plot.dir,paste("fig09d_risk_vs_ryield_2o3bsafe.jpeg",sep="")), quality=100, width=1400, height=700)
  
    dd <- aux %>% filter(HCRT == "2o3")
    
    p <- ggplot(dd, aes(x=catch.MSY, y=Risk3.Blim, shape=HCR, colour=UC))+
      geom_point(size=3)+
      geom_hline(yintercept = 0.05, linetype = "longdash")+
      facet_grid(term ~ OMnam, scales = "free_y")+
      scale_colour_manual(values = ucp.col3)+
      theme(text = element_text(size = 20), 
            title = element_text(size = 16, face = "bold"), 
            strip.text = element_text(size = 20))
    
    print(p)
  
  dev.off()


# - Risks per relative yield

dd <- aux %>% 
  mutate(ratio = Risk3.Blim/catch.MSY)

jpeg(file.path(plot.dir,paste("figXX_ratRiskRyield.jpeg",sep="")), quality=100, width=1400, height=700)

  p <- ggplot(dd, aes(x=UC, y=ratio, shape=HCRT, colour=UC))+
    geom_point(size=3)+
    facet_grid(term ~ OMnam, scales = "free")+
    ylab("Risk per tonne")+
    scale_colour_manual(values = ucp.col)+
    theme(axis.text.x=element_blank(), 
          text = element_text(size = 20), 
          title = element_text(size = 16, face = "bold"), 
          strip.text = element_text(size = 20))
  
  print(p)

dev.off()


# - Risks per tonne

aux <- df_bc %>% subset(indicator %in% c("Risk3.Blim","catch") & term != "mid" & 
                          LHSC == "bc" & SIGR == 0.75 & CVID == "low" & ADVT == "iny" & 
                          HCRT != "ft0" & BSAFE == "none") %>% 
  select(term, STKN, FHIST, OMnam, HCRT, UC, indicator, value) %>% 
  tidyr::spread(indicator, value)

dd <- aux %>% 
  mutate(ratio = Risk3.Blim/catch)

jpeg(file.path(plot.dir,paste("figXX_riskPERtonne.jpeg",sep="")), quality=100, width=1400, height=700)

  p <- ggplot(dd, aes(x=UC, y=ratio, shape=HCRT, colour=UC))+
    geom_point(size=3)+
    facet_grid(term ~ OMnam, scales = "free")+
    ylab("Risk per tonne")+
    scale_colour_manual(values = ucp.col)+
    theme(axis.text.x=element_blank(), 
          text = element_text(size = 20), 
          title = element_text(size = 16, face = "bold"), 
          strip.text = element_text(size = 20))
  
  print(p)

dev.off()

aux <- df_bc %>% subset(indicator %in% c("Risk3.Blim","catch") & term != "mid" & 
                          LHSC == "bc" & SIGR == 0.75 & CVID == "low" & ADVT == "iny" &
                          HCR %in% c("1o2","1o2_Inorm") & 
                          UC %in% (df_bc %>% subset(HCR %in% c("1o2_Inorm")) %>% .$UC %>% unique())) %>% 
                          #!UCPL %in% c(0.2,0.5)
  select(term, STKN, FHIST, OMnam, HCR, UC, indicator, value) %>% 
  tidyr::spread(indicator, value) %>% 
  mutate(ratio = Risk3.Blim/catch)

jpeg(file.path(plot.dir,paste("figXX_riskPERtonne_selection.jpeg",sep="")), quality=100, width=1400, height=700)

  p <- ggplot(aux, aes(x=UC, y=ratio, shape=HCR, colour=UC))+
    geom_point(size=3)+
    facet_grid(term ~ OMnam, scales = "free")+
    ylab("Risk per tonne")+
    scale_colour_manual(values = ucp.col)+
    theme(axis.text.x=element_blank(), 
          text = element_text(size = 20), 
          title = element_text(size = 16, face = "bold"), 
          strip.text = element_text(size = 20))
  
  print(p)

dev.off()


#==============================================================================
# SURVIVAL RATES                                                           ----
#==============================================================================

# natural mortality-at-age

mage <- data.frame()

for (stk in unique(df_om$STKN)) for (p in unique(df_om$LHSC)) {
  
  dat <- loadToEnv(file.path(inp.dir,paste0(stk,p,"_dataLH.RData")))[["stk"]]
  
  mage <- rbind( mage, 
                 data.frame( STKN=stk, LHSC=p, age=dimnames(dat)$age, 
                             value=iterMeans(seasonSums(dat@m))[,1,drop=TRUE]))
  
}

mage %>% spread(age, value)

mage %>% group_by(STKN, LHSC) %>% 
  summarise(meanmort=mean(value)) %>% 
  mutate(survp = exp(-meanmort))

#   STKN  LHSC     meanmort survp
#   <chr> <chr>       <dbl> <dbl>
# 1 STK1  bc          1.14  0.321
# 2 STK1  highprod    1.14  0.321
# 3 STK1  lowprod     1.14  0.321
# 4 STK2  bc          0.502 0.605
# 5 STK2  highprod    0.502 0.605
# 6 STK2  lowprod     0.502 0.605


mage <- mage %>% spread(age, value)

mage





# survival

survr <- data.frame()

for (stk in unique(df_om$STKN)) for (p in unique(df_om$LHSC)) {
  
  dat <- loadToEnv(file.path(inp.dir,paste0(stk,p,"_dataLH.RData")))[["stk"]]
  
  survr <- rbind( survr, 
                  data.frame( STKN=stk, LHSC=p, 
                              surv1plus=exp(-quantMeans(iterMeans(seasonSums(dat@m))[-1,1,])[drop=TRUE]), 
                              surv1_2=exp(-quantMeans(iterMeans(seasonSums(dat@m))[2:3,1,])[drop=TRUE]),
                              surv1_3=exp(-quantMeans(iterMeans(seasonSums(dat@m))[2:4,1,])[drop=TRUE]),
                              surv4plus=exp(-quantMeans(iterMeans(seasonSums(dat@m))[-c(1:4),1,])[drop=TRUE])
                              ))
  
}

survr

#   STKN     LHSC surv1plus   surv1_2   surv1_3 surv4plus
# 1 STK1       bc 0.3381835 0.2670018 0.2963147 0.3859683
# 2 STK1  lowprod 0.3381835 0.2670018 0.2963147 0.3859683
# 3 STK1 highprod 0.3381835 0.2670018 0.2963147 0.3859683
# 4 STK2       bc 0.6045445 0.5393449 0.5647117 0.6471870
# 5 STK2  lowprod 0.6045445 0.5393449 0.5647117 0.6471870
# 6 STK2 highprod 0.6045445 0.5393449 0.5647117 0.6471870


