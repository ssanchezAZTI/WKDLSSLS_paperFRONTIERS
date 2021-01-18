################################################################################
#  WKDLSSLS results - plots for analysing calendar effect                      # 
#------------------------------------------------------------------------------#
#   AZTI-Tecnalia                                                              #
#   created:  05/01/2021                                                       #
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

#==============================================================================
# WORKING DIRECTORY                                                        ----
#==============================================================================

# directory with input files
inp.dir <- "./input"

# directory with results
res.dir <- "./output"

# directory with plots
plot.dir <- "./plots/BC_calendar"

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
  
#==============================================================================
# COLOUR PALETTES
#==============================================================================

# for the UC

ucp.col <- c("cadetblue1", "blue",                        # (0.2,x)
             "olivedrab1", "springgreen", "springgreen4", # (0.5,x)
             "yellow", "darkorange", "red", "darkred",    # (0.8,x)
             "darkmagenta")                               # (NA,NA)

# for rules

rule.col <- c("red",          # 1o2
              "springgreen4", # 1o3
              "blue",         # 1o5
              "darkmagenta")  # 2o3


  
#==============================================================================
# comparison of several performance statistics 
#==============================================================================

# names and labels of performance statistics

perfnms <- c("Risk3.Blim","catch.MSY")

perflabels <- c("Risk3.Blim","catch/MSY")



#==============================================================================
# Calendar effect (all)
#==============================================================================


# all OMs - all terms

jpeg(file.path(plot.dir,"ryield_vs_risk_allOMs_smlt.jpeg",sep=""), quality=100, width=1400, height=700)

  aux <- df_bc %>% filter(ADVT!= "fix" & BSAFE == "none" & indicator %in% perfnms)
  
  p <- ggplot(aux, aes(x=ADVT, y=value, group=interaction(HCRT,BSAFE,PBUF,UC,HCRI), col=UC, lty=HCRT))+
    geom_line()+
    geom_hline(aes(yintercept = 0.05), data = subset(aux, indicator == "Risk3.Blim"), linetype="dashed") +
    facet_grid(indicator + term ~ OMnam, scales="free")+
    scale_colour_manual(values = ucp.col)+
    ylab("")+
    theme(text = element_text(size = 20), 
          title = element_text(size = 16, face = "bold"), 
          strip.text = element_text(size = 20))
  print(p)

dev.off()


# all OMs - short & long terms

jpeg(file.path(plot.dir,"ryield_vs_risk_allOMs_st&lt.jpeg",sep=""), quality=100, width=1400, height=700)

  aux <- df_bc %>% filter(term %in% c("short","long") & ADVT!= "fix" & BSAFE == "none" & indicator %in% perfnms)
  
  p <- ggplot(aux, aes(x=ADVT, y=value, group=interaction(HCRT,BSAFE,PBUF,UC,HCRI), col=UC, lty=HCRT))+
    geom_line()+
    geom_hline(aes(yintercept = 0.05), data = subset(aux, indicator == "Risk3.Blim"), linetype="dashed") +
    facet_grid(indicator + term ~ OMnam, scales="free")+
    scale_colour_manual(values = ucp.col)+
    ylab("")+
    theme(text = element_text(size = 20), 
          title = element_text(size = 16, face = "bold"), 
          strip.text = element_text(size = 20))
  print(p)

dev.off()


# all OMs - long terms

jpeg(file.path(plot.dir,"ryield_vs_risk_allOMs.jpeg",sep=""), quality=100, width=1400, height=700)

  aux <- df_bc %>% filter(term == "long" & ADVT!= "fix" & BSAFE == "none" & indicator %in% perfnms)
  
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


# all OMs - short & medium terms

jpeg(file.path(plot.dir,"ryield_vs_risk_allOMs_st&mt.jpeg",sep=""), quality=100, width=1400, height=700)

  aux <- df_bc %>% filter(term %in% c("short","mid") & ADVT != "fix" & BSAFE == "none" & indicator %in% perfnms)
  
  p <- ggplot(aux, aes(x=ADVT, y=value, group=interaction(HCRT,BSAFE,PBUF,UC,HCRI), col=UC, lty=HCRT))+
    geom_line()+
    geom_hline(aes(yintercept = 0.05), data = subset(aux, indicator == "Risk3.Blim"), linetype="dashed") +
    facet_grid(indicator + term ~ OMnam, scales="free")+
    scale_colour_manual(values = ucp.col)+
    ylab("")+
    theme(text = element_text(size = 20), 
          title = element_text(size = 16, face = "bold"), 
          strip.text = element_text(size = 20))
  print(p)

dev.off()
  


#==============================================================================
# Calendar effect (1o2 selection of UCs by term)
#==============================================================================

for (tt in unique(df_bc$term)) {
  
  jpeg(file.path(plot.dir, paste0("ryield_vs_risk_1o2selection_",tt,".jpeg",sep="")), quality=100, width=1400, height=700)
  
    aux <- df_bc %>% filter(term == tt & ADVT!= "fix" & indicator %in% perfnms &
                              HCRT == "1o2" & BSAFE == "none" & (UC=="(NA,NA)" | UC=="(0.8,0.8)" | UC=="(0.8,4)"))
    
    p <- ggplot(aux, aes(x=ADVT, y=value, group=interaction(HCRT,BSAFE,PBUF,UC,HCRI), col=UC, lty=UC))+
      geom_line()+
      geom_hline(aes(yintercept = 0.05), data = subset(aux, indicator == "Risk3.Blim"), linetype="dashed") +
      facet_grid(indicator ~ OMnam, scales="free")+
      # scale_colour_manual(values = ucp.col)+
      ylab("")+
      theme(text = element_text(size = 20),
            title = element_text(size = 16, face = "bold"),
            strip.text = element_text(size = 20))+
      ggtitle(paste0("1-over-2 (",tt,"-term)"))
    
    print(p)
  
  dev.off()
  
  # # without UC
  # 
  # jpeg(file.path(plot.dir, paste0("ryield_vs_risk_1o2noUC_",tt,".jpeg",sep="")), quality=100, width=1400, height=700)
  # 
  #   aux2 <- aux %>% filter(UC=="(NA,NA)")
  # 
  #   p <- ggplot(aux2, aes(x=ADVT, y=value, group=interaction(HCRT,BSAFE,PBUF,UC,HCRI)))+
  #     geom_line()+
  #     geom_hline(aes(yintercept = 0.05), data = subset(aux, indicator == "Risk3.Blim"), linetype="dashed") +
  #     facet_grid(indicator ~ OMnam, scales="free")+
  #     ylab("")+
  #     theme(text = element_text(size = 20),
  #           title = element_text(size = 16, face = "bold"),
  #           strip.text = element_text(size = 20))+
  #     ggtitle(paste0("1-over-2 + UC(NA,NA) (",tt,"-term)"))
  # 
  #   print(p)
  # 
  # dev.off()
  # 
  # # 80% symetric UC
  # 
  # jpeg(file.path(plot.dir, paste0("ryield_vs_risk_1o2UC80sym_",tt,".jpeg",sep="")), quality=100, width=1400, height=700)
  # 
  #   aux2 <- aux %>% filter(UC=="(0.8,0.8)")
  # 
  #   p <- ggplot(aux2, aes(x=ADVT, y=value, group=interaction(HCRT,BSAFE,PBUF,UC,HCRI)))+
  #     geom_line()+
  #     geom_hline(aes(yintercept = 0.05), data = subset(aux, indicator == "Risk3.Blim"), linetype="dashed") +
  #     facet_grid(indicator ~ OMnam, scales="free")+
  #     ylab("")+
  #     theme(text = element_text(size = 20),
  #           title = element_text(size = 16, face = "bold"),
  #           strip.text = element_text(size = 20))+
  #     ggtitle(paste0("1-over-2 + UC(0.8,0.8) (",tt,"-term)"))
  # 
  #   print(p)
  # 
  # dev.off()
  # 
  # # 80% assymetric UC
  # 
  # jpeg(file.path(plot.dir, paste0("ryield_vs_risk_1o2UC80assym_",tt,".jpeg",sep="")), quality=100, width=1400, height=700)
  # 
  #   aux2 <- aux %>% filter(UC=="(0.8,4)")
  # 
  #   p <- ggplot(aux2, aes(x=ADVT, y=value, group=interaction(HCRT,BSAFE,PBUF,UC,HCRI)))+
  #     geom_line()+
  #     geom_hline(aes(yintercept = 0.05), data = subset(aux, indicator == "Risk3.Blim"), linetype="dashed") +
  #     facet_grid(indicator ~ OMnam, scales="free")+
  #     ylab("")+
  #     theme(text = element_text(size = 20),
  #           title = element_text(size = 16, face = "bold"),
  #           strip.text = element_text(size = 20))+
  #     ggtitle(paste0("1-over-2 + UC(0.8,4) (",tt,"-term)"))
  # 
  #   print(p)
  # 
  # dev.off()

}


# #==============================================================================
# # 1o2 UC(NA,NA) (all terms)
# #==============================================================================
# 
# jpeg(file.path(plot.dir, "ryield_vs_risk_1o2noUC.jpeg"), quality=100, width=1400, height=700)
# 
#   aux <- df_bc %>% filter(ADVT!= "fix" & indicator %in% perfnms &
#                              HCRT == "1o2" & UC=="(NA,NA)" & BSAFE == "none" )
#   
#   p <- ggplot(aux, aes(x=ADVT, y=value, group=interaction(HCRT,BSAFE,PBUF,UC,HCRI,term), col=term, lty=term))+
#     geom_line()+
#     geom_hline(aes(yintercept = 0.05), data = subset(aux, indicator == "Risk3.Blim"), linetype="dashed") +
#     facet_grid(indicator ~ OMnam, scales="free")+
#     ylab("")+
#     theme(text = element_text(size = 20),
#           title = element_text(size = 16, face = "bold"),
#           strip.text = element_text(size = 20))+
#     ggtitle(paste0("1-over-2 + UC(NA,NA) (",tt,"-term)"))
#   
#   print(p)
# 
# dev.off()
# 
# 
# #----------------------------------------------
# # 1o2 UC(0.8,0.8) (all terms)
# #----------------------------------------------
# 
# jpeg(file.path(plot.dir, "ryield_vs_risk_1o2UC80sym.jpeg"), quality=100, width=1400, height=700)
# 
#     aux <- df_bc %>% filter(ADVT!= "fix" & indicator %in% perfnms &
#                               HCRT == "1o2" & UC=="(0.8,0.8)" & BSAFE=="none")
#     
#     p <- ggplot(aux, aes(x=ADVT, y=value, group=term, col=term, lty=term))+
#       geom_line()+
#       geom_hline(aes(yintercept = 0.05), data = subset(aux, indicator == "Risk3.Blim"), linetype="dashed") +
#       facet_grid(indicator ~ OMnam, scales="free")+
#       ylab("")+
#       theme(text = element_text(size = 20),
#             title = element_text(size = 16, face = "bold"),
#             strip.text = element_text(size = 20))+
#       ggtitle("1-over-2 + UC(0.8,0.8)")
#   
#   print(p)
# 
# dev.off()
# 
# 
# #----------------------------------------------
# # 1o2 UC(0.8,4) (all terms)
# #----------------------------------------------
# 
# jpeg(file.path(plot.dir, "ryield_vs_risk_1o2UC80assym.jpeg"), quality=100, width=1400, height=700)
# 
#   aux <- df_bc %>% filter(ADVT!= "fix" & indicator %in% perfnms &
#                             HCRT == "1o2" & UC=="(0.8,4)" & BSAFE=="none")
#   
#   p <- ggplot(aux, aes(x=ADVT, y=value, group=term, col=term, lty=term))+
#     geom_line()+
#     geom_hline(aes(yintercept = 0.05), data = subset(aux, indicator == "Risk3.Blim"), linetype="dashed") +
#     facet_grid(indicator ~ OMnam, scales="free")+
#     ylab("")+
#     theme(text = element_text(size = 20),
#           title = element_text(size = 16, face = "bold"),
#           strip.text = element_text(size = 20))+
#     ggtitle("1-over-2 + UC(0.8,4)")
#   
#   print(p)
# 
# dev.off()
  
  
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
  
  caldf_short <- caldf %>% filter(term == "short") %>% select(-term) %>% rename(int_short = int, iny_short = iny)
  caldf_long  <- caldf %>% filter(term == "long") %>% select(-term) %>% rename(int_long = int, iny_long = iny)
  
  caldf_diff <- full_join(caldf_long, caldf_short, by = c("OMnam", "STKN", "FHIST", "UC", "HCRT") ) %>% 
    mutate(int = int_long - int_short, 
           iny = iny_long - iny_short) %>% select(-c(fpa.x:fpa_iny.y)) %>% 
    mutate(diff = iny - int)
  
  summary(caldf_diff$int)
  summary(caldf_diff$iny)
  
  caldf_diff %>% filter( int>0 | iny>0) # aumentan los riesgos en el largo plazo
  caldf_diff %>% filter( (int>0 | iny>0) & HCRT != "2o3")
  caldf_diff %>% filter( int>0 & HCRT != "2o3") %>% .$UC %>% unique()
  caldf_diff %>% filter( iny>0 & HCRT != "2o3") %>% .$UC %>% unique()
  caldf_diff %>% filter( int<0 | iny<0) # disminuyem los riesgos en el largo plazo
  caldf_diff %>% filter( (int<0 | iny<0) & HCRT != "2o3") %>% .$UC %>% unique()
  
  # - differences contrary to expected (hisgher risks)
  caldf %>% filter(iny_int >=0.01 & term == "short")
  caldf %>% filter(fpa_int >=0.01 & term == "short")
  caldf %>% filter(fpa_iny >=0.01 & term == "short")
  caldf %>% filter(iny_int >=0.01 & term == "long")
  caldf %>% filter(fpa_int >=0.01 & term == "long")
  caldf %>% filter(fpa_iny >=0.01 & term == "long")
  
  # - in-year calendar
  caldf %>% filter(iny_int >=0.01 & term == "short") %>% .$UC %>% unique()
  caldf %>% filter(iny_int >=0.01 & term == "short") %>% .$HCRT %>% unique()
  caldf %>% filter(iny_int >=0.01 & term == "short" & HCRT != "2o3")             # STK1_fhigh
  caldf %>% filter(iny_int >=0.01 & term == "short" & !HCRT %in% c("2o3","1o5")) # STK1_fhigh & never (0.8,0.8)
  caldf %>% filter(iny_int >=0.01 & term == "long") %>% .$UC %>% unique()
  caldf %>% filter(iny_int >=0.01 & term == "long") %>% .$HCRT %>% unique()
  caldf %>% filter(iny_int >=0.01 & term == "long" & HCRT != "2o3")
  caldf %>% filter(iny_int >=0.01 & term == "long" & !HCRT %in% c("2o3","1o5"))
  caldf %>% filter(iny_int >0 & term == "long" & HCRT != "2o3")
  
  # - differences by FHIST
  caldf %>% filter(term == "short") %>% 
    group_by(FHIST) %>% summarise(iny_int.min = min(iny_int), iny_int.max = max(iny_int), 
                                  fpa_int.min = min(fpa_int), fpa_int.max = max(fpa_int), 
                                  fpa_iny.min = min(fpa_iny), fpa_iny.max = max(fpa_iny))
  #   FHIST iny_int.min iny_int.max fpa_int.min fpa_int.max fpa_iny.min fpa_iny.max
  #   <fct>       <dbl>       <dbl>       <dbl>       <dbl>       <dbl>       <dbl>
  # 1 flow       -0.091      0.001       -0.089     0.001       -0.0330      0.03  
  # 2 fopt       -0.163     -0.0100      -0.188    -0.00500     -0.059       0.015 
  # 3 fhigh      -0.251      0.0450      -0.280     0.001       -0.089       0.0480
  
  caldf %>% filter(term == "short") %>% 
    group_by(STKN, FHIST) %>% summarise(iny_int.min = min(iny_int), iny_int.max = max(iny_int))
  #   STKN  FHIST iny_int.min iny_int.max
  #   <chr> <fct>       <dbl>       <dbl>
  # 1 STK1  flow       -0.091     -0.017 
  # 2 STK1  fopt       -0.075     -0.0190
  # 3 STK1  fhigh      -0.026      0.0450
  # 4 STK2  flow       -0.023      0.001 
  # 5 STK2  fopt       -0.163     -0.0100
  # 6 STK2  fhigh      -0.251     -0.108 
  
  caldf %>% filter(term == "long") %>% 
    group_by(FHIST) %>% summarise(iny_int.min = min(iny_int), iny_int.max = max(iny_int), 
                                          fpa_int.min = min(fpa_int), fpa_int.max = max(fpa_int), 
                                          fpa_iny.min = min(fpa_iny), fpa_iny.max = max(fpa_iny))
  #   FHIST iny_int.min iny_int.max fpa_int.min fpa_int.max fpa_iny.min fpa_iny.max
  #   <fct>       <dbl>       <dbl>       <dbl>       <dbl>       <dbl>       <dbl>
  # 1 flow       -0.038       0.007      -0.033       0.008     -0.021        0.02 
  # 2 fopt       -0.141       0.031      -0.156       0.03      -0.043        0.035
  # 3 fhigh      -0.856       0.083      -0.796       0.083     -0.0790       0.419


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
  
  
#==============================================================================
# CHECK PROBLEMS OBSERVED
#==============================================================================
  
# Note: see ./plots/BC_calendar/ryield_vs_risk_all_OMs_smlt.jpeg
#       strange values for SKT2_fhigh
  
names(df_bc)

# - rules with relative yields < 0.5

df_bc %>% filter(indicator == "catch.MSY" & ADVT!="fix" & term=="long" & STKN=="STK2" & FHIST=="fhigh" & value <0.5) %>% 
  select(SCENARIO, OMnam, ADVT, HCRT, UC, indicator, value)

# Selection:             int       iny       fpa
# - 2o3_(0.2,0.2)   sc000987
# - 2o3_(0.2,0.25)  sc000999  sc000997  sc000998
# - 1o2_(0.2,0.2)   sc000981
# - 1o2_(0.2,0.25)  sc000993            sc000992
# - 1o3_(0.2,0.25)  sc000996
# - 1o5_(0.2,0.25)  sc001002

df_bc %>% filter(indicator == "catch.MSY" & ADVT!="fix" & BSAFE=="none" & term=="long" & 
                   STKN=="STK2" & FHIST=="fhigh") %>% 
  select(OMnam, ADVT, HCRT, UC, value) %>% 
  filter( (HCRT=="2o3" & UC=="(0.2,0.2)") | (HCRT=="2o3" & UC=="(0.2,0.25)") | 
            (HCRT=="1o2" & UC=="(0.2,0.2)") | (HCRT=="1o2" & UC=="(0.2,0.25)") | 
            (HCRT=="1o3" & UC=="(0.2,0.25)") | (HCRT=="1o5" & UC=="(0.2,0.25)") ) %>% 
  spread(ADVT, value)
#        OMnam HCRT         UC        int       iny       fpa
# 1 STK2_fhigh  1o2  (0.2,0.2) 0.45174894 1.0282512 1.0639618
# 2 STK2_fhigh  1o2 (0.2,0.25) 0.03843695 0.9628941 0.2438019
# 3 STK2_fhigh  1o3 (0.2,0.25) 0.06919253 1.2284903 1.0788156
# 4 STK2_fhigh  1o5 (0.2,0.25) 0.47273023 1.2668448 1.2825625
# 5 STK2_fhigh  2o3  (0.2,0.2) 0.06077578 1.2334014 1.2500266
# 6 STK2_fhigh  2o3 (0.2,0.25) 0.01455146 0.2785906 0.2626940


df_bc %>% filter(indicator == "catch.MSY" & ADVT!="fix" & BSAFE=="none" & term=="long" & 
                   STKN=="STK2" & FHIST=="fhigh" & value <0.5) %>% 
  select(SCENARIO, OMnam, ADVT, HCRT, UC, indicator, value)


# - rules with risks > 0.85

df_bc %>% filter(indicator == "Risk3.Blim" & ADVT!="fix" & BSAFE=="none" & term=="long" & 
                   STKN=="STK2" & FHIST=="fhigh" & value > 0.85) %>% 
  select(SCENARIO, OMnam, ADVT, HCRT, UC, indicator, value)

# Selection:             int       iny       fpa
# - 2o3_(0.2,0.2)   sc000987
# - 2o3_(0.2,0.25)  sc000999  sc000997  sc000998
# - 1o2_(0.2,0.2)   sc000981
# - 1o2_(0.2,0.25)  sc000993            sc000992
# - 1o3_(0.2,0.25)  sc000996
# - 1o5_(0.2,0.25)  sc001002


df_bc %>% filter(indicator == "Risk3.Blim" & ADVT!="fix" & BSAFE=="none" & term=="long" & 
                   STKN=="STK2" & FHIST=="fhigh") %>% 
  select(OMnam, ADVT, HCRT, UC, value) %>% 
  filter( (HCRT=="2o3" & UC=="(0.2,0.2)") | (HCRT=="2o3" & UC=="(0.2,0.25)") | 
            (HCRT=="1o2" & UC=="(0.2,0.2)") | (HCRT=="1o2" & UC=="(0.2,0.25)") | 
            (HCRT=="1o3" & UC=="(0.2,0.25)") | (HCRT=="1o5" & UC=="(0.2,0.25)") ) %>% 
  spread(ADVT, value)
#        OMnam HCRT         UC   int   iny   fpa
# 1 STK2_fhigh  1o2  (0.2,0.2) 0.935 0.079 0.498
# 2 STK2_fhigh  1o2 (0.2,0.25) 0.999 0.686 0.991
# 3 STK2_fhigh  1o3 (0.2,0.25) 1.000 0.264 0.609
# 4 STK2_fhigh  1o5 (0.2,0.25) 0.922 0.144 0.126
# 5 STK2_fhigh  2o3  (0.2,0.2) 0.997 0.204 0.226
# 6 STK2_fhigh  2o3 (0.2,0.25) 1.000 0.991 0.994


aux <- df_bc %>% filter(indicator %in% c("catch.MSY","Risk3.Blim") & ADVT!="fix" & BSAFE=="none" & term=="long" & 
                   STKN=="STK2" & FHIST=="fhigh") %>% 
  select(OMnam, ADVT, HCRT, UC, indicator, value) %>% 
  filter( (HCRT=="2o3" & UC=="(0.2,0.2)") | (HCRT=="2o3" & UC=="(0.2,0.25)") | 
            (HCRT=="1o2" & UC=="(0.2,0.2)") | (HCRT=="1o2" & UC=="(0.2,0.25)") | 
            (HCRT=="1o3" & UC=="(0.2,0.25)") | (HCRT=="1o5" & UC=="(0.2,0.25)") ) %>% 
  spread(ADVT, value)

aux_ryield <- aux %>% filter(indicator == "catch.MSY") %>% select(-indicator)
names(aux_ryield)[-c(1:3)] <- paste( names(aux_ryield)[-c(1:3)], "ryield", sep = "_")

aux_risk   <- aux %>% filter(indicator == "Risk3.Blim") %>% select(-indicator)
names(aux_risk)[-c(1:3)] <- paste( names(aux_risk)[-c(1:3)], "risk", sep = "_")

bind_cols(aux_ryield, aux_risk[,-c(1:3)])
#        OMnam HCRT         UC int_ryield iny_ryield fpa_ryield int_risk iny_risk fpa_risk
# 1 STK2_fhigh  1o2  (0.2,0.2) 0.45174894  1.0282512  1.0639618    0.935    0.079    0.498
# 2 STK2_fhigh  1o2 (0.2,0.25) 0.03843695  0.9628941  0.2438019    0.999    0.686    0.991
# 3 STK2_fhigh  1o3 (0.2,0.25) 0.06919253  1.2284903  1.0788156    1.000    0.264    0.609
# 4 STK2_fhigh  1o5 (0.2,0.25) 0.47273023  1.2668448  1.2825625    0.922    0.144    0.126
# 5 STK2_fhigh  2o3  (0.2,0.2) 0.06077578  1.2334014  1.2500266    0.997    0.204    0.226
# 6 STK2_fhigh  2o3 (0.2,0.25) 0.01455146  0.2785906  0.2626940    1.000    0.991    0.994


# RULES TO BE ANALYSED for OM: STK2_fhigh
# Selection:             int       iny       fpa
# - 2o3_(0.2,0.2)   sc000987
# - 2o3_(0.2,0.25)  sc000999  sc000997  sc000998
# - 1o2_(0.2,0.2)   sc000981
# - 1o2_(0.2,0.25)  sc000993            sc000992
# - 1o3_(0.2,0.25)  sc000996
# - 1o5_(0.2,0.25)  sc001002


# RULES TO BE ANALYSED for OM: STK2_fhigh

prl <- df_bc %>% filter(indicator == "catch.MSY" & ADVT!="fix" & BSAFE=="none" & term=="long" & 
                   STKN=="STK2" & FHIST=="fhigh") %>% 
  select(HCRT, UC, ADVT, SCENARIO) %>% 
  filter( (HCRT=="2o3" & UC=="(0.2,0.2)") | (HCRT=="2o3" & UC=="(0.2,0.25)") | 
            (HCRT=="1o2" & UC=="(0.2,0.2)") | (HCRT=="1o2" & UC=="(0.2,0.25)") | 
            (HCRT=="1o3" & UC=="(0.2,0.25)") | (HCRT=="1o5" & UC=="(0.2,0.25)") ) 

prl %>% spread(ADVT, SCENARIO)

#   HCRT         UC      int      iny      fpa
# 1  1o2  (0.2,0.2) sc000981 sc000979 sc000980
# 2  1o2 (0.2,0.25) sc000993 sc000991 sc000992
# 3  1o3 (0.2,0.25) sc000996 sc000994 sc000995
# 4  1o5 (0.2,0.25) sc001002 sc001000 sc001001
# 5  2o3  (0.2,0.2) sc000987 sc000985 sc000986
# 6  2o3 (0.2,0.25) sc000999 sc000997 sc000998

rules <- prl$SCENARIO 

# problematic rules

jpeg(file.path(plot.dir,"ryield_vs_risk_STK2fhigh_problems.jpeg",sep=""), quality=100, width=1400, height=700)

  aux <- df_bc %>% filter(ADVT!= "fix" & BSAFE == "none" & indicator %in% perfnms & SCENARIO %in% rules)
  
  p <- ggplot(aux, aes(x=ADVT, y=value, group=interaction(HCRT,BSAFE,PBUF,UC,HCRI), col=HCRT, lty=UC))+
    geom_line()+
    geom_hline(aes(yintercept = 0.05), data = subset(aux, indicator == "Risk3.Blim"), linetype="dashed") +
    facet_grid(indicator ~ OMnam + term, scales="free")+
    scale_colour_manual(values = rule.col)+
    ylab("")+
    theme(text = element_text(size = 20), 
          title = element_text(size = 16, face = "bold"), 
          strip.text = element_text(size = 20))
  print(p)

dev.off()



#==============================================================================
# TRAJECTORIES: n-over-m rules
#==============================================================================


# restricted to STK2_fhigh (as anomalies in results)

aux <- dat_bio %>% filter(LHSC == "bc" & SIGR == 0.75 & CVID == "low" & ADVT == "iny" &
                            HCRT != "ft0" & BSAFE == "none" & HCRI == "nin") %>%
    select(-LHSC, -SIGR, -CVID, -ADVT, -HCRI) %>%
    mutate(OMnam = factor(OMnam, levels = OMnam.levels))

dd <- dat_bioQ %>% filter(indicator %in% c("ssb","catch")) %>% 
  filter(LHSC == "bc" & SIGR == 0.75 & CVID == "low" & 
           HCRT != "ft0" & BSAFE == "none" & HCRI == "nin") %>% 
  select(-SCnam, -LHSC, -SIGR, -CVID, -HCRI) %>% 
  filter(stock=="STK2" & FHIST=="fhigh") %>% 
  select(scenario, year, LHnam, ADVT, HCRT, UCPL, UCPU, indicator:q95) %>% 
  mutate(ADVT = factor(ADVT, levels=c("int","iny","fpa")),
         UC = paste0("(",UCPL,",",UCPU,")"),
         UC = case_when(UC=="(0,0)" ~ "(NA,NA)",
                        TRUE ~ UC))


# for (id in c("ssb","catch")) { # unique(dat$indicator)
#   
#   pdf(file.path(plot.dir,paste0("trajectories_",id,"_UC(02_025).pdf")), width=14, onefile=T)
#   
#   aux <- dd %>% filter(UCPL==0.2 & UCPU==0.25) %>% filter(indicator == id)
#   
#   # scnam  <- unique(aux$SCnam)
#   refpts <- loadToEnv(file.path(inp.dir,paste0(unique(aux$LHnam),"_dataLH.RData")))[["ref.pts"]]
#   
#   p <- ggplot(data = aux, aes(x = year, y = q50)) + #, color = ADVT
#     geom_line() + 
#     geom_ribbon(aes(x = year, ymin = q05, ymax = q95), alpha = 0.5) + #, fill = ADVT
#     facet_grid(HCRT~indicator+ADVT, scales = "free") + 
#     expand_limits(y=0) +
#     geom_vline(xintercept = proj.yr - 0.5, linetype = "longdash") +
#     theme_bw() + 
#     theme(text = element_text(size = 20), 
#           title = element_text(size = 16, face = "bold"), 
#           strip.text = element_text(size = 20)) + 
#     ylab("") + 
#     # ggtitle(scnam) + 
#     theme(plot.title = element_text(hjust = 0.5))
#   if (id=="ssb") {
#     p <- p + 
#       geom_hline(lty = 2, data = data.frame(indicator = "ssb", Blim = refpts[["Blim"]]),
#                  aes(yintercept = Blim), color ="orange") +
#       geom_hline(lty = 2, data = data.frame(indicator = "ssb", Bcollapse = 0.1*refpts[["B0"]]), 
#                  aes(yintercept = Bcollapse), color = "red")
#   } else if (id=="catch")
#     p <- p + geom_hline(lty = 2, data = data.frame(indicator ="catch", MSY=refpts[["MSY"]]), 
#                         aes(yintercept = MSY), color = "green")
#   
#   print(p)
#   
#   dev.off()
#   
# }
# 
# 
# 
# 
# 
# 
# jpeg(file.path(plot.dir,"trajectories_STK2fhigh.jpeg"), quality=100, width=1400, height=700)
# 
#   p <- ggplot(data = aux, aes(x = year, y = Risk.Blim, col=UC)) +
#     geom_line() +
#     facet_grid(OMnam ~ HCRT, scales = "free") +
#     scale_color_manual(values = ucp.col)+
#     # geom_vline(xintercept = c(35.5, 50.5), linetype = "longdash") +
#     geom_hline(yintercept = 0.05, linetype = "longdash") +
#     theme_bw() +
#     theme(text = element_text(size = 20),
#           title = element_text(size = 16, face = "bold"),
#           strip.text = element_text(size = 20))
# 
#   print(p)
# 
# dev.off()
# 
# df_bc %>%
#   filter(indicator=="Risk3.Blim" & term == "short" & HCRT == "1o3" & ADVT == "iny" & UC == "(0.2,0.2)") %>%
#   select(STKN, FHIST, value) %>%
#   group_by(STKN) %>%
#   summarise(risk.min = round(min(value),2), risk.max = round(max(value),2))
# 
# #  STKN  risk.min risk.max
# #   <chr>    <dbl>    <dbl>
# # 1 STK1      0.08     0.47
# # 2 STK2      0        0.42
# 
# 
# # - catch.MSY
# 
# jpeg(file.path(plot.dir,paste("trajectories_ryield.jpeg",sep="")), quality=100, width=1400, height=700)
# 
#   p <- ggplot(data = aux, aes(x = year, y = catch.MSY, col=UC)) +
#     geom_line() +
#     facet_grid(OMnam ~ HCRT, scales = "free") +
#     scale_color_manual(values = ucp.col)+
#     # geom_vline(xintercept = c(35.5, 50.5), linetype = "longdash") +
#     geom_hline(yintercept = 1, linetype = "longdash") +
#     theme_bw() +
#     theme(text = element_text(size = 20),
#           title = element_text(size = 16, face = "bold"),
#           strip.text = element_text(size = 20))
# 
#   print(p)
# 
# dev.off()
# 
# 
