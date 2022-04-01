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
plot.dir <- "./plots/ICES"

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
# comparison of several performance statistics 
#==============================================================================

# names and labels of performance statistics

perfnms <- c("Risk3.Blim","catch.MSY")

perflabels <- c("Risk3.Blim","catch/MSY")



#==============================================================================
# SELECTED RULES
#==============================================================================

df_sel <- df_bc %>% 
  filter((HCRT == "1o2" & UC %in% c("(0.2,0.2)","(0.8,0.8)")) | (HCRT == "2o3" & UC == "(0.2,0.2)" & BSAFE == "none") | 
           (HCRT == "1o2" & UC == "(0.8,0.8)" & BSAFE == "Inorm")) %>% 
  mutate(rule = paste(HCRT, UC, sep = "_"))


# df_sel %>% filter(!(HCR %in% c("1o2_Imin","2o3_Imin")))


#==============================================================================
# CALENDARS
#==============================================================================


  #----------------------------------------------
  # plots
  #----------------------------------------------
  
  # effect of the calendar
  
  # library(RColorBrewer)
  
  jpeg(file.path(plot.dir,"figICES_mainrules_ryield_vs_risk_allOMs_st&lt.jpeg",sep=""), quality=100, width=1400, height=700)
  
    aux <- df_sel %>% filter(term %in% c("short","long") & ADVT!= "fix" & BSAFE == "none" & indicator %in% perfnms)
    
    # lapply(aux, unique)
    
    p <- ggplot(aux, aes(x=ADVT, y=value, group=interaction(HCRT,BSAFE,PBUF,UC,HCRI), col=rule, lty=rule))+
      geom_line()+
      geom_hline(aes(yintercept = 0.05), data = subset(aux, indicator == "Risk3.Blim"), linetype="dashed") +
      facet_grid(indicator + term ~ OMnam)+ #, scales="free"
      scale_colour_manual(values = ucp.col3)+
      ylab("")+
      theme(text = element_text(size = 20), 
            title = element_text(size = 16, face = "bold"), 
            strip.text = element_text(size = 20))
    print(p)
  
  dev.off()
  
  for (rl in c("1o2_(0.2,0.2)", "2o3_(0.2,0.2)", "1o2_(0.8,0.8)")) {
    
    aux <- df_sel %>% filter(ADVT!= "fix" & BSAFE == "none" & indicator %in% perfnms & rule == rl)
    
    # - by term
    
    jpeg(file.path(plot.dir,paste0("figICES_",rl,"_ryield_vs_risk_allOMs_byTerm.jpeg")), quality=100, width=1400, height=700)
    
      p <- ggplot(aux, aes(x=ADVT, y=value, group=term, col=term, lty=term))+
        geom_line()+
        geom_hline(aes(yintercept = 0.05), data = subset(aux, indicator == "Risk3.Blim"), linetype="dashed") +
        facet_grid(indicator ~ OMnam, scales="free")+
        scale_colour_manual(values = ucp.col3)+
        ylab("")+
        theme(text = element_text(size = 20), 
              title = element_text(size = 16, face = "bold"), 
              strip.text = element_text(size = 20))
      print(p)
    
    dev.off()
    
    # # - by calendar
    # 
    # jpeg(file.path(plot.dir,paste0("figICES_",rl,"_ryield_vs_risk_allOMs_byADVT.jpeg")), quality=100, width=1400, height=700)
    # 
    #   p <- ggplot(aux, aes(x=term, y=value, group=ADVT, col=ADVT, lty=ADVT))+
    #     geom_line()+
    #     geom_hline(aes(yintercept = 0.05), data = subset(aux, indicator == "Risk3.Blim"), linetype="dashed") +
    #     facet_grid(indicator ~ OMnam, scales="free")+
    #     scale_colour_manual(values = ucp.col3)+
    #     ylab("")+
    #     theme(text = element_text(size = 20), 
    #           title = element_text(size = 16, face = "bold"), 
    #           strip.text = element_text(size = 20))
    #   print(p)
    # 
    # dev.off()
    
  }
  
  # - 1o2_Inorm_(0.8,0.8)
  
  rlnm <- "1o2_Inorm_(0.8,0.8)"
  rl   <- "1o2_(0.8,0.8)"
  bs   <- "Inorm"
  
  aux <- df_sel %>% filter(ADVT!= "fix" & BSAFE == bs & indicator %in% perfnms & rule == rl)
    
  jpeg(file.path(plot.dir,paste0("figICES_",rlnm,"_ryield_vs_risk_allOMs_byTerm.jpeg")), quality=100, width=1400, height=700)
    
    p <- ggplot(aux, aes(x=ADVT, y=value, group=term, col=term, lty=term))+
      geom_line()+
      geom_hline(aes(yintercept = 0.05), data = subset(aux, indicator == "Risk3.Blim"), linetype="dashed") +
      facet_grid(indicator ~ OMnam, scales="free")+
      scale_colour_manual(values = ucp.col3)+
      ylab("")+
      theme(text = element_text(size = 20), 
            title = element_text(size = 16, face = "bold"), 
            strip.text = element_text(size = 20))
    print(p)
    
  dev.off()
  
  
  
  #----------------------------------------------
  # numeric values
  #----------------------------------------------
  
  # RISK
  
  # calendar checks
  caldf <- df_sel %>% filter(indicator=="Risk3.Blim" & term != "mid" & HCRT != "ft0" & BSAFE == "none") %>% 
    select(term, OMnam, ADVT, rule, value) %>% 
    tidyr::spread("ADVT", "value") %>% 
    mutate(iny_int = iny - int, 
           fpa_int = fpa - int, 
           fpa_iny = fpa - iny)
  
  for (rl in unique(caldf$rule))
    for (tt in unique(caldf$term)) {
      print("---------------------------------------------------------")
      print(paste0(rl," - ", tt, " term"))
      dd <- caldf %>% filter(term == tt, rule == rl) %>% select(-term, -rule)
      print(dd)
      print("---------------------------------------------------------")
    }
  
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


