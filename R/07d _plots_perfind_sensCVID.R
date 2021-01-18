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
plot.dir <- "./plots/sensCVID"

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

df <- read.table(file.path(res.dir,"output_stats","perfstats_sensCVID.csv"), header=T, sep=";")
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
                    RatioHist = CVIndex/IAVhist)

iavproj <- df %>% filter(indicator=="IAV") %>% select(SCENARIO, term, value) %>% dplyr::rename(IAVproj=value)

df <- df  %>% left_join(iavproj, by=c("SCENARIO","term")) %>% 
  mutate(RatioProj = CVIndex/IAVproj)

# factorize some values

ucpl.val <- as.character(sort(unique(df$UCPL)))
if (ucpl.val[1]=="0") ucpl.val <- c(ucpl.val[-1],ucpl.val[1])

ucpu.val <- as.character(sort(unique(df$UCPU)))
if (ucpu.val[1]=="0") ucpu.val <- c(ucpu.val[-1],ucpu.val[1])

df <- df %>% mutate(UCPL = factor(UCPL, ordered = TRUE, levels=ucpl.val), 
                    UCPU = factor(UCPU, ordered = TRUE, levels=ucpu.val),
                    UCP.l_u = paste(UCPL,UCPU,sep = "_"),
                    FHIST = factor(FHIST, levels=c("flow","fopt","fhigh")),
                    LHSC = factor(LHSC, levels = c("lowprod","bc","highprod")),
                    ADVT = ordered(ADVT, levels=c("fix","int","iny","fpa")), 
                    HCRI = ordered(HCRI, levels=c("fix","pob","pyc","nin")), 
                    CVID = ordered(CVID, levels=c("low","iav","high","iav2")),
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


#==============================================================================
# PLOTS for each OM
#==============================================================================

# effect of CVID by OM

pdf(file.path(plot.dir,paste("plot_compare_acrossOMs.pdf",sep="")), width = 12, onefile=T)

for (ind in perfnms)
  for (om in unique(df$OMnam)) {
  
    # if (ind == "Risk3.Blim") {
    #   jpeg(file.path(plot.dir,paste("risk_OM_",OMnam,".jpeg",sep="")), quality=100, width=1400, height=700)
    # } else if (ind == "catch.MSY")
    #   jpeg(file.path(plot.dir,paste("yield_OM_",OMnam,".jpeg",sep="")), quality=100, width=1400, height=700)

    aux <- subset(df, indicator %in% ind & OMnam == om & term %in% c("short", "long") & LHSC == "bc" & SIGR == 0.75) %>%
      mutate(rule = paste(HCRT,UCP.l_u,sep="_"))
    
    aux.fix <- aux %>% subset(ADVT=="fix") %>% select(-HCRT)
    rules <- unique(aux$HCRT)[unique(aux$HCRT)!="ft0"]
    aux.fix <- aux.fix[rep(1:nrow(aux.fix),times = length(rules)),]
    aux.fix$HCRT <- rep(rules, each=nrow(aux.fix)/length(rules))
    aux     <- aux %>% subset(HCRT!="ft0")
    
    p <- ggplot(aux, aes(x=CVIndex, y=value, fill=CVID))+
      geom_bar(stat="identity",position="dodge")+
      facet_grid(term ~ rule)+
      ylab(perflabels[perfnms==ind])+
      scale_fill_brewer(palette="Paired", type = "qual")+
      theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 9))+
      ggtitle(om)
    
    if(length(grep("Risk", ind))>0){
      p <- p + geom_hline(yintercept = 0.05, linetype = "longdash")
      p <- p + geom_hline(aes(yintercept = value), data = subset(aux.fix, indicator == "Risk3.Blim"), col="red")
      p <- p + ylim(c(0,1))
    }
    print(p)
    
    # dev.off()
    
  }

dev.off()
rm(ind, om)


# effect of CVID all OMs

pdf(file.path(plot.dir,paste("plot_compare_allOMs.pdf",sep="")), onefile=T)

  aux <- subset(df, indicator %in% perfnms & term %in% c("short", "long") & LHSC == "bc" & SIGR == 0.75) %>% 
    mutate(rule = paste(HCRT,UCP.l_u,sep="_"))
  
  p <- ggplot(aux, aes(x=CVIndex, y=value, col=rule))+
    geom_point()+
    geom_line(linetype="dashed")+
    geom_hline(lty = 2, data = data.frame(indicator = "Risk3.Blim", ref = 0.05), aes(yintercept = ref))+
    facet_grid(indicator + term ~ OMnam)+
    scale_fill_brewer(palette="Paired", type = "qual")
  print(p)

dev.off()


#==============================================================================
# CVID/IAV
#==============================================================================

pdf(file.path(plot.dir,paste("plot_RiskBlim_by_CVIDIAVrat.pdf",sep="")), onefile=T)

  # - by CVID and term
  aux <- subset(df, indicator=="Risk3.Blim" & UCPL == 0 & UCPU == 0)
  
  p <- ggplot(aux, aes(x=IAVproj, y=value, col=OMnam, shape=HCRT))+
    geom_point()+
    facet_grid(term ~ CVID)+
    # ylim(c(0,1))+
    xlab("IAVproj")+
    ylab("Risk3.Blim")
  print(p)
  
  # - term by term
  for (tt in unique(as.character(df$term))) {
    
    aux2 <- subset(aux, term==tt)
    
    # - LHSC=bc & SIGR=0.75
    p <- ggplot(filter(aux2, LHSC=="bc" & SIGR==0.75), aes(x=IAVproj, y=value, col=OMnam, shape=HCRT))+
      geom_point()+
      facet_grid( ~ CVID)+
      # ylim(c(0,1))+
      xlab("IAVproj")+
      ylab("Risk3.Blim")+
      ggtitle(paste0(tt,"-term : LHSC=bc & SIGR=0.75"))
    print(p)
    
    # - CVID=low
    p <- ggplot(filter(aux2,CVID=="low"), aes(x=IAVproj, y=value, col=OMnam, shape=HCRT))+
      geom_point()+
      facet_grid(LHSC ~ SIGR)+
      # ylim(c(0,1))+
      xlab("IAVproj")+
      ylab("Risk3.Blim")+
      ggtitle(paste0(tt,"-term : CVID=low"))
    print(p)
    
  }
  
  # for (tt in c("hist",unique(as.character(df$term)))) {
  #   
  #   if (tt == "hist") {
  #     
  #     aux <- subset(df, term=="short" & indicator=="Risk3.Blim" & UCPL == 0 & UCPU == 0) %>% 
  #       select(SCENARIO, OMnam, indicator, value, HCRT, RatioHist)
  #     
  #     p <- ggplot(aux, aes(x=RatioHist, y=value, col=OMnam, shape=HCRT))+
  #       geom_point()+
  #       ylim(c(0,1))+
  #       xlab("CVID/IAVhist")+
  #       ylab("Risk3.Blim")
  #     print(p)
  #     
  #   } else {
  #     aux <- subset(df, term==tt & indicator=="Risk3.Blim" & UCPL == 0 & UCPU == 0)
  #     
  #     p <- ggplot(aux, aes(x=RatioProj, y=value, col=OMnam, shape=HCRT))+
  #       geom_point()+
  #       ylim(c(0,1))+
  #       xlab("CVID/IAVproj")+
  #       ylab("Risk3.Blim")+
  #       ggtitle(paste0(tt,"-term : no UCPs"))
  #     print(p)
  #     
  #     p <- ggplot(aux, aes(x=RatioProj, y=value, col=OMnam, shape=HCRT))+
  #       geom_point()+
  #       facet_grid(LHSC ~ SIGR)+
  #       ylim(c(0,1))+
  #       xlab("CVID/IAVproj")+
  #       ylab("Risk3.Blim")+
  #       ggtitle(paste0(tt,"-term : no UCPs"))
  #     print(p)
  #     
  #     p <- ggplot(filter(aux, SIGR==0.75 & LHSC=="bc"), aes(x=RatioProj, y=value, col=OMnam, shape=HCRT))+
  #       geom_point()+
  #       facet_grid( ~ CVID)+
  #       ylim(c(0,1))+
  #       xlab("CVID/IAVproj")+
  #       ylab("Risk3.Blim")+
  #       ggtitle(paste0(tt,"-term : no UCPs + SIGR=0.75 & LHSC=bc"))
  #     print(p)
  #     
  #   }
  #   
  # }

dev.off()

aux2 <- subset(aux, term=="short" & STKN=="STK1" & FHIST=="flow" & HCRT=="1o2" &
                 LHSC=="bc" & SIGR==0.75) #

p <- ggplot(aux2, aes(x=IAVproj, y=value, col=as.character(CVIndex)))+
  geom_point()+
  # ylim(c(0,1))+
  xlab("IAVproj")+
  ylab("Risk3.Blim")
print(p)

p <- ggplot(aux2, aes(x=RatioProj, y=value, col=as.character(CVIndex)))+
  geom_point()+
  # ylim(c(0,1))+
  xlab("CVID/IAVproj")+
  ylab("Risk3.Blim")
print(p)

tt <- "mid"; st <- "STK1"; hcr <- "1o2"; lh <- "bc"; sigR <- 0.75
aux2 <- subset(aux, term==tt & STKN==st & HCRT==hcr &
                 LHSC==lh & SIGR==sigR) #

# p <- ggplot(aux2, aes(x=IAVproj, y=value, col=as.character(CVIndex)))+
#   geom_point()+
#   # ylim(c(0,1))+
#   xlab("IAVproj")+
#   ylab("Risk3.Blim")
# print(p)

p <- ggplot(aux2, aes(x=RatioProj, y=value, col=FHIST))+
  geom_point()+
  # ylim(c(0,1))+
  xlab("CVID/IAVproj")+
  ylab("Risk3.Blim")
print(p)





tt <- "mid"; st <- "STK1"; hcr <- "2o3"; lh <- "bc"; sigR <- 0.75
aux2 <- subset(aux, term==tt & STKN==st & HCRT==hcr &
                 LHSC==lh & SIGR==sigR) #

p <- ggplot(aux2, aes(x=RatioProj, y=value, col=as.character(round(CVIndex,2)), shape=FHIST))+
  geom_point()+
  # ylim(c(0,1))+
  xlab("CVID/IAVproj")+
  ylab("Risk3.Blim")+
  ggtitle(paste(tt,st,hcr,lh,sigR,sep="_"))
print(p)

p <- ggplot(aux2, aes(x=IAVproj, y=value, col=as.character(round(CVIndex,2)), shape=FHIST))+
  geom_point()+
  # ylim(c(0,1))+
  xlab("IAVproj")+
  ylab("Risk3.Blim")+
  ggtitle(paste(tt,st,hcr,lh,sigR,sep="_"))
print(p)

p <- ggplot(aux2, aes(x=IAVhist, y=value, col=as.character(round(CVIndex,2)), shape=FHIST))+
  geom_point()+
  # ylim(c(0,1))+
  xlab("IAVhist")+
  ylab("Risk3.Blim")+
  ggtitle(paste(tt,st,hcr,lh,sigR,sep="_"))
print(p)


tt <- "mid"; st <- "STK1"; hcr <- "2o3"; lh <- "bc"; sigR <- 0.75

pdf(file.path(plot.dir,paste("plot_RiskBlim_by_CVIDIAVrat_tests.pdf",sep="")), onefile=T)

for (tt in unique(aux$term))
  for (st in unique(aux$STKN))
    for (hcr in unique(aux$HCRT)) {
      
      lh <- "bc"; sigR <- 0.75
      
      aux2 <- subset(aux, term==tt & STKN==st & HCRT==hcr &
                       LHSC==lh & SIGR==sigR) #
      
      p <- ggplot(aux2, aes(x=RatioProj, y=value, col=as.character(round(CVIndex,2)), shape=FHIST))+
        geom_point()+
        xlab("CVID/IAVproj")+
        ylab("Risk3.Blim")+
        ggtitle(paste(tt,st,hcr,lh,sigR,sep="_"))
      print(p)
      
    }

dev.off()
