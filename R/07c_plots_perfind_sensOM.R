################################################################################
#  WKDLSSLS results - plots of performance statistics                          # 
#------------------------------------------------------------------------------#
#   AZTI-Tecnalia                                                              #
#   created:  01/08/2019                                                       #
#   modified:                                                                  #
################################################################################

# Copyright: AZTI, 2019
# Author: AZTI (<libaibarriaga@azti.es>)
#
# Distributed under the terms of the GNU GPLv3

#==============================================================================
#                                                                          ----
#==============================================================================

rm(list=ls())

#==============================================================================
# WORKING DIRECTORY                                                        ----
#==============================================================================

# directory with results
res.dir <- "./output"

# directory with plots
plot.dir <- "./plots/sensOM"

#==============================================================================
# LOAD LIBRARIES AND FUNCTIONS                                             ----
#==============================================================================

# load libraries
library(ggplot2)
library(plyr)
library(tidyr)
library(dplyr)

theme_set(theme_bw())

#==============================================================================
# SCENARIOS                                                               ----
#==============================================================================

# names of the scenarios

#load(file.path(res.dir, "scenario_list.RData"))
#length(scenario_list)

#==============================================================================
# Read performance statistics
#==============================================================================

# load performance statistics for all the scenarios

df <- read.table(file.path(res.dir,"output_stats","perfstats_sensOM.csv"), header=T, sep=";")
#df <- read.table(file.path(res.dir,"perfstats_across_OMs.csv"), header=T, sep=";")

# reshape to the long format for ggplot

df <- df %>% gather(indicator, value, names(df)[!names(df) %in% c("stock", "scenario", "term", "depl.ini", "IAVhist", "RiskBlim.ini")])

# period as an ordered factor for the figures

df$term <- factor(df$term, levels=c("short","mid","long"))

row.names(df) <- NULL
df$stock <- NULL
names(df)[1] <- "SCENARIO"

# add the other variables for scenario description

sc.dat <- read.table(file.path("input","list_scenarios.csv"), header=T, sep=",")
df <- df %>% left_join(sc.dat, by="SCENARIO") # we use the function join from dplyr instead of merge to keep the order of the rows

# only scenarios with BSAFE = "none"

unique(df$BSAFE) #"none"

# compute the ratio between the CV of the obs index and the IAV

df <- df %>% mutate(CVIndex = case_when(CVID=="low"  ~ 0.25, 
                                        CVID=="high" ~ 0.5, 
                                        CVID=="iav"  ~ IAVhist, 
                                        CVID=="iav2" ~ 2*IAVhist, 
                                        TRUE         ~ 0*NA), 
                    Ratio = CVIndex/IAVhist)

# factorize some values

ucpl.val <- as.character(sort(unique(df$UCPL)))
if (ucpl.val[1]=="0") ucpl.val <- c(ucpl.val[-1],ucpl.val[1])

ucpu.val <- as.character(sort(unique(df$UCPU)))
if (ucpu.val[1]=="0") ucpu.val <- c(ucpu.val[-1],ucpu.val[1])

df <- df %>% mutate(UCPL = factor(UCPL, ordered = TRUE, levels=ucpl.val), 
                    UCPU = factor(UCPU, ordered = TRUE, levels=ucpu.val),
                    UCP.l_u = paste(UCPL,UCPU,sep = "_"),
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
# comparison of several performance statistics 
#==============================================================================

# names and labels of performance statistics

# perfnms <- c("ssb.B0","f.Fmsy","Risk3.Blim","Risk3.Collapse","catch.MSY",
#              "hr","Risk.hrmax")
# 
# perflabels <- c("SSB/B0","F/Fmsy","Risk3.Blim","Risk3.Collapse","catch/MSY", 
#                 "HR (Catch/SSB)","P(HR > HRmax)")

perfnms <- c("Risk3.Blim","catch.MSY")
perflabels <- c("Risk3.Blim","catch/MSY")


# #==============================================================================
# # PLOTS for each OM
# #==============================================================================
# 
# jpeg(file.path(plot.dir,paste("fig_sensOM_LHSC_compareOMs.jpeg",sep="")), quality=100, width=1400, height=700)
# 
# aux <- subset(df_om, indicator %in% perfnms & term %in% c("short", "long") & CVID == "low") %>% 
#   mutate(rule = paste(HCRT,UCP.l_u,sep="_"))
# 
# p <- ggplot(aux, aes(x=LHSC, y=value, col=OMnam, shape=rule))+
#   geom_point(size=2)+
#   geom_line(linetype="dashed")+
#   geom_hline(lty = 2, data = data.frame(indicator = "Risk3.Blim", ref = 0.05), aes(yintercept = ref))+
#   facet_grid(indicator + term ~ SIGR, scales="free")+
#   scale_fill_brewer(palette="Paired", type = "qual")+
#   theme(text = element_text(size = 20), 
#         title = element_text(size = 16, face = "bold"), 
#         strip.text = element_text(size = 20))
# print(p)
# 
# dev.off()

#==============================================================================
# PLOTS for each OM
#==============================================================================

# effect of CVID by OM

pdf(file.path(plot.dir,paste("plot_compare_acrossOMs.pdf",sep="")), onefile=T)

# - by LHSC

for (om in unique(df$OMnam)) {
    
  aux <- subset(df, indicator %in% perfnms & OMnam == om & term %in% c("short", "long") & CVID == "low") %>%
    mutate(rule = paste(HCRT,UCP.l_u,sep="_"))
  
  aux.fix <- aux %>% subset(ADVT=="fix") %>% select(-HCRT)
  rules <- unique(aux$HCRT)[unique(aux$HCRT)!="ft0"]
  aux.fix <- aux.fix[rep(1:nrow(aux.fix),times = length(rules)),]
  aux.fix$HCRT <- rep(rules, each=nrow(aux.fix)/length(rules))
  aux     <- aux %>% subset(HCRT!="ft0")
  
  p <- ggplot(aux, aes(x=LHSC, y=value, fill=SIGR))+
    geom_bar(stat="identity",position="dodge")+
    geom_hline(lty = 2, data = data.frame(indicator = "Risk3.Blim", ref = 0.05), aes(yintercept = ref))+
    facet_grid(indicator + term ~ rule, scales="free")+
    scale_fill_brewer(palette="Paired", type = "qual")+
    theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 9))+
    ggtitle(om)
  
  print(p)
  
}
 
# - by SIGR

for (om in unique(df$OMnam)) {
  
  aux <- subset(df, indicator %in% perfnms & OMnam == om & term %in% c("short", "long") & CVID == "low") %>%
    mutate(rule = paste(HCRT,UCP.l_u,sep="_"))
  
  aux.fix <- aux %>% subset(ADVT=="fix") %>% select(-HCRT)
  rules <- unique(aux$HCRT)[unique(aux$HCRT)!="ft0"]
  aux.fix <- aux.fix[rep(1:nrow(aux.fix),times = length(rules)),]
  aux.fix$HCRT <- rep(rules, each=nrow(aux.fix)/length(rules))
  aux     <- aux %>% subset(HCRT!="ft0")
  
  p <- ggplot(aux, aes(x=SIGR, y=value, fill=LHSC))+
    geom_bar(stat="identity",position="dodge")+
    geom_hline(lty = 2, data = data.frame(indicator = "Risk3.Blim", ref = 0.05), aes(yintercept = ref))+
    facet_grid(indicator + term ~ rule, scales="free")+
    scale_fill_brewer(palette="Paired", type = "qual")+
    ggtitle(om)
  
  print(p)
    
}

dev.off()
rm(om)


# effect of CVID by OM

pdf(file.path(plot.dir,paste("plot_compare_acrossOMs_catch.pdf",sep="")), onefile=T)

# - by LHSC

for (tt in c("short", "long")) {
  
  aux <- subset(df, indicator == "catch" & term == tt & CVID == "low" & HCRT!="ft0") %>%
    mutate(rule = paste(HCRT,UCP.l_u,sep="_"))
  
  p <- ggplot(aux, aes(x=LHSC, y=value, fill=SIGR))+
    geom_bar(stat="identity",position="dodge")+
    facet_grid(OMnam ~ rule, scales="free")+
    scale_fill_brewer(palette="Paired", type = "qual")+
    theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 9))+
    ylab("catch")+
    ggtitle(paste0(tt,"-term"))
  
  print(p)
  
}

# - by SIGR

for (tt in c("short", "long")) {
  
  aux <- subset(df, indicator == "catch" & term == tt & CVID == "low" & HCRT!="ft0") %>%
    mutate(rule = paste(HCRT,UCP.l_u,sep="_"))
  
  p <- ggplot(aux, aes(x=SIGR, y=value, fill=LHSC))+
    geom_bar(stat="identity",position="dodge")+
    facet_grid(OMnam ~ rule, scales="free")+
    scale_fill_brewer(palette="Paired", type = "qual")+
    ylab("catch")+
    ggtitle(paste0(tt,"-term"))
  
  print(p)
  
}

dev.off()
rm(tt)

