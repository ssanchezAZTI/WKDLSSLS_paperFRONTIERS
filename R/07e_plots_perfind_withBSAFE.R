################################################################################
#  WKDLSSLS results - plots of performance statistics                          # 
#------------------------------------------------------------------------------#
#   AZTI-Tecnalia                                                              #
#   created:  08/01/2021                                                       #
#   modified:                                                                  #
################################################################################

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

# directory with results
res.dir  <- "./output"

# directory with plots
plot.dir <- "./plots/withBSAFE"

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

df <- read.table(file.path(res.dir,"output_stats","perfstats_BC.csv"), header=T, sep=";")

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
                    UC = paste0("(",UCPL,",",UCPU,")"),
                    FHIST = factor(FHIST, levels=c("flow","fopt","fhigh")),
                    ADVT = ordered(ADVT, levels=c("fix","int","iny","fpa")), 
                    HCRI = ordered(HCRI, levels=c("fix","pob","pyc","nin")),
                    BSAFE = ordered(BSAFE, levels=c("none","Imin","Iminpa","Inorm")),
                    rule = case_when(BSAFE == "none" ~ HCRT,
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

ucp.col <- c("cadetblue1", "blue", "springgreen", "springgreen4", # 1o2
             "yellow", "darkorange", "red", "darkred")            # 2o3

ucp.col2 <- c("cadetblue1", "springgreen", "yellow", "red")


#==============================================================================
# comparison of several performance statistics 
#==============================================================================

# names and labels of performance statistics

perfnms <- c("ssb","catch","Risk1.Blim","Risk3.Blim")

perflabels <- c("SSB","Catch","Risk1.Blim","Risk3.Blim")

# perfnms <- c("ssb","catch","catch.iyv","f","hr","f.Fmsy","ssb.B0",#"quotaUpt",
#              "Risk1.Collapse","Risk1.Blim",
#              "Risk2.Collapse","Risk2.Blim",
#              "Risk3.Collapse","Risk3.Blim",
#              "Risk.hrmax","catch.var")
# 
# perflabels <- c("SSB","Catch","IYV Catch",
#                 "F","HR (Catch/SSB)",
#                 "F/Fmsy","SSB/B0",
#                 # "Quota Uptake",
#                 "Risk1.Collapse","Risk1.Blim",
#                 "Risk2.Collapse","Risk2.Blim",
#                 "Risk3.Collapse","Risk3.Blim",
#                 "P(HR > HRmax)","Catch Var")

#==============================================================================
# PLOTS for each OM
#==============================================================================

# remove zero TAC
df <- df %>% filter(ADVT != "fix")

# check alternatives
cols <- c("OM", "OMnam","LHSC", "SIGR", "CVFH", "IDXT", "CVID", 
          "ADVT", "rule", "PBUF", "UC", "HCRI")

for (c in cols)
  print(paste0(c,": ", paste(unique(df[,c]),collapse=", ")))


# restrict to BSAFE cases

rls  <- grep( "_I", unique(df$rule), value = TRUE) %>% substr(1,3) %>% unique()
ucps <- df %>% filter(grepl( "_I", rule)) %>% .$UC %>% unique()

df <- df %>% filter(HCRT %in% rls & UC %in% ucps)


#----------------------------------------------
# comparison of several performance statistics
#----------------------------------------------

# effect of harvest control rule

for (st in unique(df$STKN)) {
  
  pdf(file.path(plot.dir,paste("plot_compare_by_hcrt_",st,".pdf",sep="")), onefile=T)
  for (i in 1:length(perfnms)){
    ind <- perfnms[i]
    aux <- subset(df, indicator %in% ind & STKN==st & ADVT!="fix")
    p <- ggplot(aux, aes(x=rule, y=value, fill=rule))+
      geom_bar(stat="identity")+
      facet_grid(UC + term ~  FHIST)+
      ylab(perflabels[i])+
      scale_fill_manual(values = ucp.col) +
      theme(axis.text.x=element_blank())
    if(length(grep("Risk", ind))>0){
      p <- p + geom_hline(yintercept = 0.05, linetype = "longdash")
      p <- p + ylim(c(0,1)) 
    }
    print(p)
  }
  dev.off()
  
}

for (tnm in unique(df$term)) {
  
  pdf(file.path(plot.dir,paste("plot_compare_by_hcrt_",tnm,".pdf",sep="")), onefile=T)
  for (i in 1:length(perfnms)){
    ind <- perfnms[i]
    aux <- subset(df, indicator %in% ind & term==tnm & ADVT!="fix")
    p <- ggplot(aux, aes(x=rule, y=value, fill=rule))+
      geom_bar(stat="identity")+
      facet_grid(UC + STKN ~  FHIST)+
      ylab(perflabels[i])+
      scale_fill_manual(values = ucp.col) +
      theme(axis.text.x=element_blank())
    if(length(grep("Risk", ind))>0){
      p <- p + geom_hline(yintercept = 0.05, linetype = "longdash")
      p <- p + ylim(c(0,1)) 
    }
    print(p)
  }
  dev.off()
  
}

for (tnm in unique(df$term)) for (st in unique(df$STKN)) {
  
  pdf(file.path(plot.dir,paste("plot_compare_by_hcrt_",tnm,"_",st,".pdf",sep="")), onefile=T)
    for (i in 1:length(perfnms)){
      ind <- perfnms[i]
      aux <- subset(df, indicator %in% ind & term==tnm & STKN==st & ADVT!="fix")
      p <- ggplot(aux, aes(x=rule, y=value, fill=rule))+
        geom_bar(stat="identity")+
        facet_grid(UC ~  FHIST)+
        ylab(perflabels[i])+
        scale_fill_manual(values = ucp.col) +
        theme(axis.text.x=element_blank())
      if(length(grep("Risk", ind))>0){
        p <- p + geom_hline(yintercept = 0.05, linetype = "longdash")
        p <- p + ylim(c(0,1)) 
      }
      print(p)
    }
  
  dev.off()
  
}


#----------------------------------------------
# interaction plots
#----------------------------------------------

for (omnm in unique(df$OM)) {

  aux <- subset(df, OM==omnm & term=="long")
  
  pdf(file.path(plot.dir,paste("plot_compare_by_factor_",omnm,".pdf",sep="")), onefile=T)
  p <- ggplot(aux, aes(x=factor(rule), y=value, 
                       group=interaction(ADVT,PBUF,UC,HCRI)))+
    geom_line()+
    facet_wrap(~indicator, scales="free")
  print(p)
  
  p <- ggplot(aux, aes(x=factor(UC), y=value, 
                       group=interaction(rule,ADVT,PBUF,HCRI)))+
    geom_line()+
    facet_wrap(~indicator, scales="free")
  print(p)
  
  dev.off()
  
}

#----------------------------------------------
# RISK vs CATCH
#----------------------------------------------

pdf(file.path(plot.dir,"plot_catch_vs_risk.pdf"), onefile=T)

  aux <- subset(df, indicator %in% c("catch","Risk3.Blim") & term=="long" & ADVT!="fix")
  
  aux <- aux %>% spread(indicator, value)
  
  p <- ggplot(aux, aes(x=Risk3.Blim, y=catch))+
    geom_point(aes(shape = FHIST, color = rule))+
    scale_color_manual(values = ucp.col)+
    geom_vline(xintercept=0.05, linetype="dashed")
  print(p)
  
  p <- ggplot(aux, aes(x=Risk3.Blim, y=catch))+
    geom_point(aes(shape = OMnam, color =rule))+
    scale_color_manual(values = ucp.col)+
    geom_vline(xintercept=0.05, linetype="dashed")
  print(p)
  
  p <- ggplot(aux, aes(x=Risk3.Blim, y=catch))+
    geom_point(aes(shape = UC, color = rule))+
    scale_color_manual(values = ucp.col)+
    geom_vline(xintercept=0.05, linetype="dashed")
  print(p)

dev.off()

for (st in unique(df$STKN)) {
  
  pdf(file.path(plot.dir,paste("plot_catch_vs_risk_",st,".pdf",sep="")), onefile=T)
  
  aux <- subset(df, indicator %in% c("catch","Risk3.Blim") & term=="long" & ADVT!="fix"& STKN==st)
  
  aux <- aux %>% spread(indicator, value)
  
  p <- ggplot(aux, aes(x=Risk3.Blim, y=catch))+
    geom_point(aes(shape = FHIST, color = rule))+
    scale_color_manual(values = ucp.col)+
    geom_vline(xintercept=0.05, linetype="dashed")
  print(p)
  
  p <- ggplot(aux, aes(x=Risk3.Blim, y=catch))+
    geom_point(aes(shape = UC, color = rule, size = FHIST))+
    geom_vline(xintercept=0.05, linetype="dashed")
  print(p)
  
  dev.off()
  
}

# comparing potential rules #! PENDING
for (tnm in unique(df$term)) for (st in unique(df$STKN)) {
  
  pdf(file.path(plot.dir,paste("plot_catch_vs_risk_",tnm,"_",st,"_selected.pdf",sep="")), onefile=T)
  
  aux <- subset(df, indicator %in% c("catch","Risk3.Blim") & term==tnm & ADVT!="fix"& STKN==st &
                  HCRT %in% c("1o2","1o2_Imin") & UCPL %in% c("0","0.8"))
  aux$HCR <- paste(aux$rule,aux$UC,sep="_")
  aux$SIGR <- as.factor(aux$SIGR)
  aux <- aux %>% spread(indicator, value)
  
  p <- ggplot(aux, aes(x=Risk3.Blim, y=catch))+
    geom_point(aes(shape = FHIST, color = UC))+
    facet_grid( ~ rule)+
    geom_vline(xintercept=0.05, linetype="dashed")
  print(p)
  
  p <- ggplot(aux, aes(x=Risk3.Blim, y=catch))+
    geom_point(aes(shape = FHIST, color = rule))+
    facet_grid( ~ UC)+
    geom_vline(xintercept=0.05, linetype="dashed")
  print(p)
  
  dev.off()
  
}

for (tnm in unique(df$term)) for (st in unique(df$STKN)) {
  
  pdf(file.path(plot.dir,paste("plot_catch_vs_risk_",tnm,"_",st,"_selected.pdf",sep="")), onefile=T)
  
  aux <- subset(df, indicator %in% c("catch","Risk3.Blim") & term==tnm & ADVT!="fix"& STKN==st &
                  HCRT == "1o2" & UCPL %in% c("0","0.8"))
  aux$HCR <- paste(aux$rule,"_UC",aux$UC,sep="")
  aux <- aux %>% spread(indicator, value)
  
  p <- ggplot(aux, aes(x=Risk3.Blim, y=catch))+
    geom_point(aes(shape = FHIST, color = rule))+
    facet_grid(. ~  UC)+
    geom_vline(xintercept=0.05, linetype="dashed")
  print(p)
  
  dev.off()
  
}

pdf(file.path(plot.dir,"plot_catch_vs_risk_wd.pdf"), onefile=T)
  
  aux <- subset(df, indicator %in% c("catch","Risk3.Blim") & 
                  HCRT == "1o2" & UCPL %in% c("0","0.8"))
  aux <- aux %>% spread(indicator, value)
  
  p <- ggplot(aux, aes(x=catch, y=Risk3.Blim))+
    geom_point(aes(shape = UC, color = rule))+
    scale_color_manual(values = ucp.col2)+
    geom_hline(yintercept=0.05, linetype="dashed")+
    facet_grid(term ~  OMnam, scales = "free")
  print(p)
  
  aux$HCR <- ordered(aux$HCR, levels=c("1o2_c0","1o2_Imin","1o2_c0.8"))
  rmc <- c()
  for (c in 1:ncol(aux))
    if (length(unique(aux[,c]))==1) rmc <- c(rmc, colnames(aux)[c])
  aux[,rmc] <- NULL
  aux$SCENARIO <- aux$OM <- aux$MP <- aux$HCRT <- aux$PBUF <- aux$UCPL <- aux$UCPU <- NULL
  
  aux2 <- aux %>%
    # Join both columns for allowing reshaping together
    nest(catch, Risk3.Blim, .key = "catch_risk") %>%
    # Reshape gender columns for easier summaries
    spread(HCR, catch_risk, sep="_") %>%
    # Separate previously nested columns
    unnest(HCR_1o2_c0, HCR_1o2_Imin, HCR_1o2_c0.8, .sep="_") %>%
    # Select grouping columns
    group_by(term, STKN, SIGR, FHIST) %>%
    # Compute ratios
    mutate(RC_0Imin = HCR_1o2_c0_catch - HCR_1o2_Imin_catch, rlC_OImin = RC_0Imin/HCR_1o2_Imin_catch, 
           RR_0Imin = HCR_1o2_c0_Risk3.Blim - HCR_1o2_Imin_Risk3.Blim, rlR_0Imin = RR_0Imin/HCR_1o2_Imin_Risk3.Blim, 
           Tper1percR_0Imin = RC_0Imin/RR_0Imin/100, 
           RC_Imin08 = HCR_1o2_Imin_catch - HCR_1o2_c0.8_catch, rlC_Imin08 = RC_Imin08/HCR_1o2_c0.8_catch, 
           RR_Imin08 = HCR_1o2_Imin_Risk3.Blim - HCR_1o2_c0.8_Risk3.Blim, rlR_Imin08 = RR_Imin08/HCR_1o2_c0.8_Risk3.Blim, 
           Tper1percR_Imin08 = RC_Imin08/RR_Imin08/100) %>%
    # Select colunms of interest
    select(term, STKN, SIGR, FHIST, IAV, Ratio, 
           rlC_OImin, rlR_0Imin, Tper1percR_0Imin, #RC_0Imin, RR_0Imin,
           rlC_Imin08, rlR_Imin08, Tper1percR_Imin08) #%>% head()
  
  aux2 <- reshape(aux2, direction='long', 
                  varying=c("rlC_OImin", "rlR_0Imin", "Tper1percR_0Imin", "rlC_Imin08", "rlR_Imin08", "Tper1percR_Imin08"), 
                  timevar='HCRratio',
                  times=c('0Imin', 'Imin08'),
                  v.names=c('rlC', 'rlR', 'Tper1percR'),
                  idvar=names(aux2)[1:4])

  ##! CHECK
  # aux2
  # aux %>% filter(term=="short", STKN=="STK1", SIGR==0.5, HCR=="1o2_c0", FHIST=="fopt") %>% select(catch)/
  #   aux %>% filter(term=="short", STKN=="STK1", SIGR==0.5, HCR=="1o2_Imin", FHIST=="fopt") %>% select(catch) -1
  # aux %>% filter(term=="short", STKN=="STK1", SIGR==0.5, HCR=="1o2_c0", FHIST=="fhigh") %>% select(Risk3.Blim)/
  #   aux %>% filter(term=="short", STKN=="STK1", SIGR==0.5, HCR=="1o2_Imin", FHIST=="fhigh") %>% select(Risk3.Blim) -1
  # (aux %>% filter(term=="short", STKN=="STK1", SIGR==0.5, HCR=="1o2_c0", FHIST=="flow") %>% select(catch)-
  #   aux %>% filter(term=="short", STKN=="STK1", SIGR==0.5, HCR=="1o2_Imin", FHIST=="flow") %>% select(catch)) / 
  #   (aux %>% filter(term=="short", STKN=="STK1", SIGR==0.5, HCR=="1o2_c0", FHIST=="flow") %>% select(Risk3.Blim)-
  #      aux %>% filter(term=="short", STKN=="STK1", SIGR==0.5, HCR=="1o2_Imin", FHIST=="flow") %>% select(Risk3.Blim))
  
  aux2$HCRratio <- as.factor(aux2$HCRratio)
  
  # Relative reductions: risks vs. catches
  p <- ggplot(aux2, aes(x=rlC, y=rlR))+
    geom_point(aes(shape=FHIST, color = HCRratio))+
    geom_abline(slope = 1, linetype="dashed")+
    facet_grid(term ~  STKN + SIGR, scales = "free")
  print(p)
  
  # Tons per 1% risk reductions
  p <- ggplot(aux2, aes(x=Ratio, y=Tper1percR))+
    geom_point(aes(shape=FHIST, color = HCRratio))+
    geom_abline(slope = 1, linetype="dashed")+
    facet_grid(term ~  STKN + SIGR, scales = "free")+
    labs(x = "CVID/IAV", y = "tons per 1% risk reduction")
  print(p)
  
  
dev.off()


pdf(file.path(plot.dir,"plot_catch_vs_risk_wd2.pdf"), onefile=T)

  aux <- subset(df, indicator %in% c("catch","Risk3.Blim") & ADVT!="fix" &
                  HCRT %in% c("1o2","1o2_Imin") & UCPL %in% c("0","0.8"))
  aux$HCR <- as.factor(paste(aux$HCRT,"_cap(",aux$UCPL,",",aux$UCPU,")",sep=""))
  levels(aux$HCR)[levels(aux$HCR)=="1o2_cap(0,0)"] <- "1o2"
  levels(aux$HCR)[levels(aux$HCR)=="1o2_Imin_cap(0,0)"] <- "1o2_Imin"
  aux$SIGR <- as.factor(aux$SIGR)
  aux <- aux %>% spread(indicator, value)
  
  p <- ggplot(aux, aes(x=catch, y=Risk3.Blim))+
    geom_point(aes(shape = FHIST, color = HCR))+
    geom_hline(yintercept=0.05, linetype="dashed")+
    facet_grid(term ~  STKN + SIGR, scales = "free")
  print(p)
  
  # aux$HCR <- ordered(aux$HCR, levels=c("1o2","1o2_Imin","1o2_(0.8,0.8)","1o2_(0.8,0.2)"))
  HCR.nams <- levels(aux$HCR)
  levels(aux$HCR)[levels(aux$HCR)=="1o2_cap(0.8,0.2)"] <- "1o2_capAss"
  levels(aux$HCR)[levels(aux$HCR)=="1o2_cap(0.8,0.8)"] <- "1o2_capSim"
  rmc <- c()
  for (c in 1:ncol(aux))
    if (length(unique(aux[,c]))==1) rmc <- c(rmc, colnames(aux)[c])
  aux[,rmc] <- NULL
  aux$SCENARIO <- aux$OM <- aux$MP <- aux$HCRT <- aux$PBUF <- aux$UCPL <- aux$UCPU <- NULL

  aux2 <- aux %>%
    # Join both columns for allowing reshaping together
    nest(catch, Risk3.Blim, .key = "catch_risk") %>%
    # Reshape gender columns for easier summaries
    spread(HCR, catch_risk, sep="_") %>%
    # Separate previously nested columns
    unnest(HCR_1o2, HCR_1o2_capAss, HCR_1o2_capSim, HCR_1o2_Imin, .sep="_") %>%
    # Select grouping columns
    group_by(term, STKN, SIGR, FHIST) %>%
    # Compute ratios
    mutate(RC_0Imin = HCR_1o2_catch - HCR_1o2_Imin_catch, rlC_OImin = RC_0Imin/HCR_1o2_Imin_catch,
           RR_0Imin = HCR_1o2_Risk3.Blim - HCR_1o2_Imin_Risk3.Blim, rlR_0Imin = RR_0Imin/HCR_1o2_Imin_Risk3.Blim,
           Tper1percR_0Imin = RC_0Imin/RR_0Imin/100,
           RC_IminSim = HCR_1o2_Imin_catch - HCR_1o2_capSim_catch, rlC_IminSim = RC_IminSim/HCR_1o2_capSim_catch,
           RR_IminSim = HCR_1o2_Imin_Risk3.Blim - HCR_1o2_capSim_Risk3.Blim, rlR_IminSim = RR_IminSim/HCR_1o2_capSim_Risk3.Blim,
           Tper1percR_IminSim = RC_IminSim/RR_IminSim/100, 
           RC_SimAss = HCR_1o2_capSim_catch - HCR_1o2_capAss_catch, rlC_SimAss = RC_SimAss/HCR_1o2_capAss_catch,
           RR_SimAss = HCR_1o2_capSim_Risk3.Blim - HCR_1o2_capAss_Risk3.Blim, rlR_SimAss = RR_SimAss/HCR_1o2_capAss_Risk3.Blim,
           Tper1percR_SimAss = RC_SimAss/RR_SimAss/100) %>%
    # Select colunms of interest
    select(term, STKN, SIGR, FHIST, IAV, Ratio,
           rlC_OImin, rlR_0Imin, Tper1percR_0Imin, #RC_0Imin, RR_0Imin,
           rlC_IminSim, rlR_IminSim, Tper1percR_IminSim, 
           rlC_SimAss, rlR_SimAss, Tper1percR_SimAss) #%>% head()

  aux2 <- reshape(aux2, direction='long',
                  varying=c("rlC_OImin", "rlR_0Imin", "Tper1percR_0Imin", 
                            "rlC_IminSim", "rlR_IminSim", "Tper1percR_IminSim",
                            "rlC_SimAss", "rlR_SimAss", "Tper1percR_SimAss"),
                  timevar='HCRratio',
                  times=c('0Imin', 'IminSim', 'SimAss'),
                  v.names=c('rlC', 'rlR', 'Tper1percR'),
                  idvar=names(aux2)[1:4])
  
  aux2$HCRratio <- as.factor(aux2$HCRratio)
  levels(aux2$HCRratio)[levels(aux2$HCRratio)=="0Imin"] <- "1o2 rel. 1o2_Imin"
  levels(aux2$HCRratio)[levels(aux2$HCRratio)=="IminSim"] <- "1o2_Imin rel. 1o2_cap(0.8,0.8)"
  levels(aux2$HCRratio)[levels(aux2$HCRratio)=="SimAss"] <- "1o2_cap(0.8,0.8) rel. 1o2_cap(0.8,0.2)"
  
  # Relative reductions: risks vs. catches
  p <- ggplot(aux2, aes(x=rlC, y=rlR))+
    geom_point(aes(shape=FHIST, color = HCRratio))+
    geom_abline(slope = 1, linetype="dashed")+
    facet_grid(term ~  STKN + SIGR, scales = "free")
  print(p)
  
  # Relative reductions: catches vs. risks
  p <- ggplot(aux2, aes(x=rlR, y=rlC))+
    geom_point(aes(shape=FHIST, color = HCRratio))+
    geom_abline(slope = 1, linetype="dashed")+
    facet_grid(term ~  STKN + SIGR, scales = "free")
  print(p)

  # Tons per 1% risk reductions
  p <- ggplot(aux2, aes(x=Ratio, y=Tper1percR))+
    geom_point(aes(shape=FHIST, color = HCRratio))+
    geom_abline(slope = 1, linetype="dashed")+
    facet_grid(term ~  STKN + SIGR, scales = "free")+
    labs(x = "CVID/IAV", y = "tons per 1% risk reduction")
  print(p)

dev.off()


#----------------------------------------------
# radar plots to compare several 
# performance statistics at the same time
#----------------------------------------------

# function to plot radar plot
# http://www.cmap.polytechnique.fr/~lepennec/R/Radar/RadarAndParallelPlots.html

coord_radar <- function (theta = "x", start = 0, direction = 1) {
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") "y" else "x"
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE)
}

# rescale each indicator from 0 to 1

library(dplyr)
library(scales)

df.scaled <- df %>% group_by(indicator, term) %>% 
  mutate( value2=rescale(value))

df.scaled <- as.data.frame(df.scaled)

# radar plot for UCPL

pdf(file.path(plot.dir,"radar_UCPL.pdf"), onefile=T)
for (st in unique(df$STKN)) for (sr in unique(df$SIGR)) for (fh in unique(df$FHIST)){
  for (aa in c("iny")){ #,"int","fpa"
    for (bb in c("1o2","1o2_Imin","2o3")){ #,"1o3","1o5"
      for (cc in c("nin")){ #"pyc","nin","pob"
        for (dd in c(0)){ #0, 0.2
          aux <- subset(df.scaled, STKN==st & term=="long" & SIGR==sr & FHIST==fh & 
                          ADVT==aa & HCRT==bb & HCRI==cc & PBUF==dd &
                          indicator %in% c("ssb","catch","catch.iyv","quotaUpt","Risk3.Collapse","Risk3.Blim","Risk.hrmax"))
          aux <- aux[order(aux$indicator), ] 
          p <- ggplot(data=aux, aes(x=indicator, y=value2, col=factor(UCPL), fill=factor(UCPL), group=factor(UCPL)))+
            #  geom_polygon(alpha=0.2, lwd=1)+
            geom_polygon(fill=NA, lwd=1)+
            geom_point(cex=1.5)+
            coord_radar()+
            theme_bw()+
            theme(text=element_text(size=14),
                  strip.text=element_text(size=14),
                  title=element_text(size=18,face="bold"))+
            ylab("")+
            ylim(c(0,1))+
            ggtitle(paste(st,sr,fh,aa,bb,cc,dd,sep="_"))
          print(p)
        }
      }
    }
  }
}
dev.off()
  
# radar plot for HCRT
  
pdf(file.path(plot.dir,"radar_HCRT.pdf"), onefile=T)
for (st in unique(df$STKN)) for (sr in unique(df$SIGR)) for (fh in unique(df$FHIST)){
  for (aa in c("iny")){ #,"int","fpa"
    for (bb in c(0, 0.8)){ #0,0.2, 0.5, 0.8
      for (cc in c("nin")){ #"pyc","nin","pob"
        for (dd in c(0)){ #0, 0.2
          aux <- subset(df.scaled, OM==omnm & term=="long" & SIGR==sr & FHIST==fh & 
                          ADVT==aa & UCPL==bb & HCRI==cc & PBUF==dd &
                          indicator %in% c("ssb","catch","catch.iyv","quotaUpt","Risk3.Collapse","Risk3.Blim","Risk.hrmax"))
          if (nrow(aux)==0)
            next()
          aux <- aux[order(aux$indicator), ] 
          p <- ggplot(data=aux, aes(x=indicator, y=value2, col=factor(HCRT), fill=factor(HCRT), group=factor(HCRT)))+
            #  geom_polygon(alpha=0.2, lwd=1)+
            geom_polygon(fill=NA, lwd=1)+
            geom_point(cex=1.5)+
            coord_radar()+
            theme_bw()+
            theme(text=element_text(size=14),
                  strip.text=element_text(size=14),
                  title=element_text(size=18,face="bold"))+
            ylab("")+
            ylim(c(0,1))+
            ggtitle(paste(st,sr,fh,aa,bb,cc,dd,sep="_"))
          print(p)
        }
      }
    }
  }
}
dev.off()


#----------------------------------------------
# mp's that fulfill risk restrictions
#----------------------------------------------

aux <- subset(df, term=="long" & OM %in% OMs)
aux2 <- reshape(aux, idvar=c("SCENARIO","term"), timevar="indicator", v.names="value", direction="wide")

pairs(aux2[, c(22,25,26,29,32,33,36)], col=(aux2$value.Risk3.Blim<0.05)+1)
pairs(aux2[, c(22,25,26,29,32,33,36)], col=aux2$ADVT, main="Colour ADVT")
pairs(aux2[, c(22,25,26,29,32,33,36)], col=aux2$HCRT, main="Colour HCRT")
pairs(aux2[, c(22,25,26,29,32,33,36)], col=aux2$PBUF, main="Colour PBUF")
pairs(aux2[, c(22,25,26,29,32,33,36)], col=aux2$UCPL, main="Colour UCPL")
pairs(aux2[, c(22,25,26,29,32,33,36)], col=aux2$HCRI, main="Colour HCRI")

pairs(aux2[aux2$value.Risk3.Blim<0.05, c(22,25,26,29,32,33,36)], col=aux2$ADVT, main="Colour ADVT")
pairs(aux2[aux2$value.Risk3.Blim<0.05, c(22,25,26,29,32,33,36)], col=aux2$HCRT, main="Colour HCRT")
pairs(aux2[aux2$value.Risk3.Blim<0.05, c(22,25,26,29,32,33,36)], col=aux2$PBUF, main="Colour PBUF")
pairs(aux2[aux2$value.Risk3.Blim<0.05, c(22,25,26,29,32,33,36)], col=aux2$UCPL, main="Colour UCPL")
pairs(aux2[aux2$value.Risk3.Blim<0.05, c(22,25,26,29,32,33,36)], col=aux2$HCRI, main="Colour HCRI")

table(aux2$value.Risk3.Blim<0.05, aux2$UCPL, aux2$HCRT, aux2$ADVT)

