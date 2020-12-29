################################################################################
#  WKDLSSLS (statistics for historical trajectories in the OMs)                #
#     Note: file should be called from 01_lifeHistoryTraits_*.R                #
#           (otherwise additional parameters should be defined)                #
#------------------------------------------------------------------------------#
#   Leire Ibaibarriaga (AZTI-Tecnalia)                                         #
#   created:  04/10/2019                                                       #
#   modified: 2020-05-21 16:40:01 add extra statistics                         #
################################################################################

# 04_OM_analysis.R - 
# ~/WKDLSSLS_2019/R/04_OM_analysis.R

# Copyright: AZTI, 2019
# Author: Leire Ibaibarriaga (AZTI) (<libaibarriaga@azti.es>)
#
# Distributed under the terms of the GNU GPLv3


#==============================================================================
# LOAD LIBRARIES AND FUNCTIONS                                             ----
#==============================================================================

library(FLBEIA)
library(dplyr)


#==============================================================================
# DATA                                                                     ----
#==============================================================================

omf0.csv    <- read.csv(file.path("input", "list_oms_f0.csv"), header=T)
om.csv      <- read.csv(file.path("input", "list_oms.csv"), header=T)
omsigR0.csv <- read.csv(file.path("input", "list_oms_sigR0.csv"), header=T)

# join both datasets
oms.csv <- omf0.csv %>% bind_rows(om.csv) %>% bind_rows(omsigR0.csv)


#==============================================================================
# CREATE CSV WITH EXTRA INFORMATION FOR EACH OM                            ----
#==============================================================================

out <- NULL

for (om in unique(oms.csv$OM)) {
  
  print(paste("Working on ", om))
  
  wd <- ifelse( om %in% om.csv$OM, "input/iters", 
                ifelse( om %in% omf0.csv$OM, "input/iters/f0", 
                        ifelse( om %in% omsigR0.csv$OM, "input/iters/sigR0", NA)))
  
  load(file.path(wd, paste("data_",om,".RData",sep="")))
  
  # Initial depletion
  Dinitial.med <- as.numeric(iterMedians(ssb(biols[[1]])[,30,,2,,]/advice.ctrl[[1]]$ref.pts["B0",]))
  Dinitial.mean <- as.numeric(iterMeans(ssb(biols[[1]])[,30,,2,,]/advice.ctrl[[1]]$ref.pts["B0",]))
  Dinitial.cv <- as.numeric(iterCVs(ssb(biols[[1]])[,30,,2,,]/advice.ctrl[[1]]$ref.pts["B0",]))
  
  # SSB/Bmsy
  SSB_Bmsy.med  <- median(ssb(biols[[1]])[,30,,2,,]/advice.ctrl[[1]]$ref.pts["Bmsy",])
  SSB_Bmsy.mean <- mean(ssb(biols[[1]])[,30,,2,,]/advice.ctrl[[1]]$ref.pts["Bmsy",])
  
  # average catch in the last 10 years of the projection period
  TAC.mean     <- mean(advice$TAC[,21:30,,,,]) # alternatively: yearMeans(iterMeans(advice$TAC[,21:30,,,,]))
  
  # TAC_MSY.mean
  TAC_MSY.mean <- mean(advice$TAC[,21:30,,,,]/advice.ctrl[[1]]$ref.pts["MSY",])
  
  # F-levels
  F.mean <- mean(fyr[,21:30,,,,])
  F_Fmsy.mean <- mean(fyr[,21:30,,,,]/advice.ctrl[[1]]$ref.pts["Fmsy",])
  
  # output
  out <- rbind(out, data.frame(OM=om, IAV=IAV, 
                               Dinitial.med=Dinitial.med, Dinitial.mean=Dinitial.mean, Dinitial.cv=Dinitial.cv, 
                               SSB_Bmsy.med=SSB_Bmsy.med, SSB_Bmsy.mean=SSB_Bmsy.mean, 
                               TAC.mean=TAC.mean, TAC_MSY.mean=TAC_MSY.mean, 
                               F.mean=F.mean, F_Fmsy.mean=F_Fmsy.mean))
}

oms.csv <- oms.csv %>% full_join(out, by = "OM")

write.csv(oms.csv, file=file.path("input","list_oms_IAV_Depletion_TAC.csv"), row.names=F)


#==============================================================================
# PLOTS                                                                    ----
#==============================================================================

library(ggplot2)
theme_set(theme_bw())

oms.csv$FHIST <- ordered(oms.csv$FHIST, levels=c("f0","flow","fopt","fhigh"))
oms.csv$LHSC  <- ordered(oms.csv$LHSC, levels=c("lowprod","bc","highprod" ))
oms.csv$SIGR <- as.factor(oms.csv$SIGR)

pdf(file.path("input","IAV_Depletion.pdf"), onefile=T)
ggplot(oms.csv, aes(SIGR, IAV, fill=STKN))+
  geom_bar(stat="identity", position="dodge")+
  facet_grid(FHIST ~ LHSC)
ggplot(oms.csv, aes(SIGR, IAV, fill=LHSC))+
  geom_bar(stat="identity", position="dodge")+
  facet_grid(FHIST ~ STKN)
ggplot(oms.csv, aes(SIGR, IAV, fill=FHIST))+
  geom_bar(stat="identity", position="dodge")+
  facet_grid(LHSC ~ STKN)
ggplot(oms.csv, aes(SIGR, Dinitial.med, fill=STKN))+
  geom_bar(stat="identity", position="dodge")+
  facet_grid(FHIST ~ LHSC)
ggplot(oms.csv, aes(SIGR, Dinitial.med, fill=LHSC))+
  geom_bar(stat="identity", position="dodge")+
  facet_grid(FHIST ~ STKN)
ggplot(oms.csv, aes(SIGR, Dinitial.med, fill=FHIST))+
  geom_bar(stat="identity", position="dodge")+
  facet_grid(LHSC ~ STKN)
dev.off()

