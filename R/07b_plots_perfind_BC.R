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
plot.dir <- "./plots/BC"

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
                    UCP.l_u = paste(UCPL,UCPU,sep = "_"),
                    FHIST = factor(FHIST, levels=c("flow","fopt","fhigh")),
                    ADVT = ordered(ADVT, levels=c("fix","int","iny","fpa")), 
                    HCRI = ordered(HCRI, levels=c("fix","pob","pyc","nin")),
                    BSAFE = ordered(BSAFE, levels=c("none","Imin","Iminpa","Inorm")),
                    CVID = ordered(CVID, levels=c("low","high","iav","iav2")),
                    OMnam = paste(STKN,FHIST,sep="_"), 
                    OMnam = factor(OMnam, 
                                   levels=apply(arrange(expand.grid(STKN=c("STK1","STK2"), 
                                                                    FHIST=c("flow","fopt","fhigh")),STKN), 1, paste, collapse="_")))

# Select only scenarios without BSAFE

df <- df %>% filter(BSAFE == "none")


#==============================================================================
# comparison of several performance statistics 
#==============================================================================

# names and labels of performance statistics

perfnms <- c("ssb.B0","f.Fmsy","Risk3.Blim","Risk3.Collapse","catch.MSY",
             "hr","Risk.hrmax")

perflabels <- c("SSB/B0","F/Fmsy","Risk3.Blim","Risk3.Collapse","catch/MSY", 
                "HR (Catch/SSB)","P(HR > HRmax)")

#==============================================================================
# PLOTS for each OM
#==============================================================================

# omnm <- "om001"

#aux <- subset(df, OM==omnm)

for (omnm in unique(df$OM)) {

  #----------------------------------------------
  # comparison of several performance statistics
  #----------------------------------------------
  
  # effect of management calendars
  
  pdf(file.path(plot.dir,paste("plot_compare_by_advt_",omnm,".pdf",sep="")), onefile=T)
  for (i in 1:length(perfnms)){
    ind <- perfnms[i]
    aux <- subset(df, OM==omnm & indicator %in% ind & term=="long")
    aux.fix <- aux %>% subset(ADVT=="fix")
    aux     <- aux %>% subset(ADVT!="fix")
    p <- ggplot(aux, aes(x=ADVT, y=value, fill=ADVT))+
      geom_bar(stat="identity")+
      facet_grid(UCP.l_u ~ HCRT)+
      ylab(perflabels[i])+
    scale_fill_brewer(palette="Set2", type = "qual")
    if(length(grep("Risk", ind))>0){
      p <- p + geom_hline(yintercept = 0.05, linetype = "longdash")
      p <- p + geom_hline(aes(yintercept = value), data = subset(aux.fix, indicator == "Risk3.Blim"), col="red")
      p <- p + ylim(c(0,1)) 
    }
    print(p)
  }
  
  # Risk vs. catch
  aux <- subset(df, OM==omnm & indicator %in% c("catch.MSY","Risk3.Blim") & term=="long") %>% 
    spread(indicator, value)
  aux.fix <- aux %>% subset(ADVT=="fix")
  aux     <- aux %>% subset(ADVT!="fix")
  p <- ggplot(aux, aes(x=catch.MSY, y=Risk3.Blim, shape=ADVT, col=HCRT))+
    geom_point(stat="identity")+
    # facet_grid(UCP.l_u ~ .)+
    geom_hline(yintercept = 0.05, linetype = "longdash")+
    geom_hline(yintercept = aux.fix$Risk3.Blim, col = "red")+
    scale_fill_brewer(palette="Set2", type = "qual")
  print(p)
  
  dev.off()
  
  # effect of uncertainty cap
  
  pdf(file.path(plot.dir,paste("plot_compare_by_ucp_",omnm,".pdf",sep="")), width=14, onefile=T)
  for (i in 1:length(perfnms)){
    ind <- perfnms[i]
    aux <- subset(df, OM==omnm & indicator %in% ind & term=="long")
    aux.fix <- aux %>% subset(ADVT=="fix")
    aux     <- aux %>% subset(ADVT!="fix")
    p <- ggplot(aux, aes(x=UCPU, y=value, fill=UCPL))+
      geom_bar(stat="identity", position="dodge")+
      facet_grid(ADVT ~  HCRT)+
      ylab(perflabels[i])+
      scale_fill_brewer(palette="Set2", type = "qual")
    if(length(grep("Risk", ind))>0){
      p <- p + geom_hline(yintercept = 0.05, linetype = "longdash")
      p <- p + geom_hline(aes(yintercept = value), data = subset(aux.fix, indicator == "Risk3.Blim"), col="red")
      p <- p + ylim(c(0,1))
    }
    print(p)
  }
  dev.off()
  
  # effect of harvest control rule
  
  pdf(file.path(plot.dir,paste("plot_compare_by_hcrt_",omnm,".pdf",sep="")), width=14, onefile=T)
  for (i in 1:length(perfnms)){
    ind <- perfnms[i]
    aux <- subset(df, OM==omnm & indicator %in% ind & term=="long")
    aux.fix <- aux %>% subset(ADVT=="fix")
    aux     <- aux %>% subset(ADVT!="fix")
    p <- ggplot(aux, aes(x=HCRT, y=value, fill=HCRT, alpha=ADVT))+
      geom_bar(stat="identity", position="dodge")+
      facet_grid(UCPL ~ UCPU )+
      ylab(perflabels[i])+
      scale_fill_brewer(palette="Set2", type = "qual")
    if(length(grep("Risk", ind))>0){
      p <- p + geom_hline(yintercept = 0.05, linetype = "longdash")
      p <- p + geom_hline(aes(yintercept = value), data = subset(aux.fix, indicator == "Risk3.Blim"), col="red")
      p <- p + ylim(c(0,1)) 
    }
    print(p)
  }
  dev.off()
  
  
  #----------------------------------------------
  # interaction plots
  #----------------------------------------------
  
  aux <- subset(df, OM==omnm & term=="long" & HCRT != "fix" & HCRT != "ft0" & indicator %in% perfnms) %>% 
    mutate(indicator = ordered(indicator, levels=perfnms))
  
  pdf(file.path(plot.dir,paste("plot_compare_by_factor_",omnm,".pdf",sep="")), width = 12, onefile=T)
  p <- ggplot(aux, aes(x=factor(HCRT), y=value, 
                       group=interaction(ADVT,PBUF,UCPL,UCPU,HCRI)))+
    geom_line()+
    facet_wrap(~indicator, scales="free")
  print(p)
  
  p <- ggplot(aux, aes(x=factor(ADVT), y=value, 
                       group=interaction(HCRT,PBUF,UCPL,UCPU,HCRI)))+
    geom_line()+
    facet_wrap(~indicator, scales="free")
  print(p)
  
  p <- ggplot(aux, aes(x=factor(UCPL), y=value, 
                       group=interaction(HCRT,ADVT,PBUF,UCPU,HCRI)))+
    geom_point()+
    facet_wrap(~indicator, scales="free")
  print(p)
  
  p <- ggplot(aux, aes(x=factor(UCPU), y=value, 
                       group=interaction(HCRT,ADVT,PBUF,UCPL,HCRI)))+
    geom_line()+
    facet_wrap(~indicator, scales="free")
  print(p)
  
  p <- ggplot(aux, aes(x=factor(UCP.l_u), y=value, 
                       group=interaction(HCRT,ADVT,PBUF,HCRI)))+
    geom_line()+
    facet_wrap(~indicator, scales="free")+
    theme(axis.text.x=element_text(angle=45,hjust=1))
  print(p)
  
  dev.off()
  
  # # fig for presentation:
  # 
  # library(RColorBrewer)
  # 
  # pdf(file.path(plot.dir,paste("plot_yield&risk_",omnm,".pdf",sep="")), width = 12, onefile=T)
  # 
  # aux <- subset(df, OM==omnm & term=="long")
  # aux.fix <- aux %>% subset(ADVT == "fix" & indicator=="Risk3.Blim")
  # 
  # p <- ggplot(subset(aux, ADVT!= "fix" & indicator %in% c("catch.MSY","Risk3.Blim")),
  #             aes(x=ADVT, y=value, group=interaction(HCRT,PBUF,UCP.l_u,HCRI), col=UCP.l_u, lty=HCRT))+
  #   geom_line()+
  #   geom_hline(aes(yintercept = value), data = subset(aux.fix, indicator == "Risk3.Blim"), col="red") +
  #   geom_hline(aes(yintercept = 0.05), data = subset(aux.fix, indicator == "Risk3.Blim"), linetype="dashed") +
  #   facet_wrap(~indicator, scales="free")+
  #   scale_colour_manual(values = brewer.pal(10,"Paired"))
  # print(p)
  # 
  # dev.off()

}

# fig for presentation:

library(RColorBrewer)

pdf(file.path(plot.dir,"plot_yield&risk_allOMs.pdf",sep=""), width = 12, onefile=T)

for (tt in unique(df$term)) {
  aux <- subset(df, term==tt)
  aux.fix <- aux %>% subset(ADVT == "fix" & indicator=="Risk3.Blim")
  
  p <- ggplot(subset(aux, ADVT!= "fix" & indicator %in% c("catch.MSY","Risk3.Blim")),
              aes(x=ADVT, y=value, group=interaction(HCRT,PBUF,UCP.l_u,HCRI), col=UCP.l_u, lty=HCRT))+
    geom_line()+
    geom_hline(aes(yintercept = value), data = subset(aux.fix, indicator == "Risk3.Blim"), col="red") +
    geom_hline(aes(yintercept = 0.05), data = subset(aux.fix, indicator == "Risk3.Blim"), linetype="dashed") +
    facet_grid(indicator ~ OMnam, scales="free")+
    scale_colour_manual(values = brewer.pal(10,"Paired"))+
    ggtitle(paste("TERM =", tt))
  print(p)
}

# risks < 0.10

sc.risk <- aux %>% subset(indicator=="Risk3.Blim" & value<=0.10) %>% .$SCENARIO
aux <- subset(aux, SCENARIO %in% sc.risk & term=="long")
aux.fix <- aux %>% subset(ADVT == "fix" & indicator=="Risk3.Blim")

p <- ggplot(subset(aux, ADVT!= "fix" & indicator %in% c("catch.MSY","Risk3.Blim")),
            aes(x=ADVT, y=value, group=interaction(HCRT,PBUF,UCP.l_u,HCRI), col=UCP.l_u, lty=HCRT))+
  geom_line()+
  geom_hline(aes(yintercept = value), data = subset(aux.fix, indicator == "Risk3.Blim"), col="red") +
  geom_hline(aes(yintercept = 0.05), data = subset(aux.fix, indicator == "Risk3.Blim"), linetype="dashed") +
  facet_grid(indicator ~ OMnam, scales="free")+
  scale_colour_manual(values = brewer.pal(10,"Paired"))
print(p)

dev.off()


#----------------------------------------------
# long-term
#----------------------------------------------

# effect of uncertainty cap (short-term only risks & yield)

perfnms2   <- c("Risk3.Blim","catch.MSY")
perflabels2 <- c("Risk3.Blim","catch/MSY")

pdf(file.path(plot.dir,"plot_compare_by_ucp_short_allOMs.pdf",sep=""), width=14, onefile=T)

for (i in 1:length(perfnms2)){
  for (omnm in unique(df$OM)) {
    ind <- perfnms2[i]
    aux <- subset(df, OM==omnm & indicator %in% ind & term=="short")
    aux.fix <- aux %>% subset(ADVT=="fix") %>% select(-HCRT)
    rules <- unique(aux$HCRT)[unique(aux$HCRT)!="ft0"]
    advts <- unique(aux$ADVT)[unique(aux$ADVT)!="fix"]
    aux.fix <- aux.fix[rep(1:nrow(aux.fix),times = length(rules)),]
    aux.fix$HCRT <- rep(rules, each=nrow(aux.fix)/length(rules))
    aux.fix <- aux.fix[rep(1:nrow(aux.fix),times = length(advts)),]
    aux.fix$ADVT <- rep(advts, each=nrow(aux.fix)/length(advts))
    aux     <- aux %>% filter(ADVT!="fix" & HCRT != "ft0")
    sc <- aux %>% select(STKN, LHSC, SIGR, FHIST) %>% 
      mutate(FHIST = as.character(FHIST)) %>% unique()
    p <- ggplot(aux, aes(x=UCPU, y=value, fill=UCPL))+
      geom_bar(stat="identity", position="dodge")+
      facet_grid(ADVT ~ HCRT)+
      ylab(perflabels2[i])+
      scale_fill_brewer(palette="Set2", type = "qual") +
      ggtitle(paste(paste(names(sc),as.character(sc),sep="=", collapse = " "),"-", perflabels2[i]))
    if(length(grep("Risk", ind))>0){
      p <- p + geom_hline(yintercept = 0.05, linetype = "longdash")
      p <- p + geom_hline(aes(yintercept = value), data = subset(aux.fix, indicator == "Risk3.Blim"), col="red")
      p <- p + ylim(c(0,1)) 
    }
    print(p)
  }
}

dev.off()


#==============================================================================
# COMPARISON ACROSS OMs
#==============================================================================

perfnms    <- c("catch.MSY", "catch.var", "ssb.B0", "f.Fmsy", "Risk3.Blim")
perflabels <- c("catch/MSY", "catch_var", "SSB/B0", "F/Fmsy", "Risk3.Blim")

# 2-over-3 rule

pdf(file.path(plot.dir,paste("plot_2o3_across_OMs.pdf",sep="")), onefile=T)

  for (ind in perfnms[c(1,5)]){
    aux <- subset(df, indicator == ind & term %in% c("short", "long"))
    aux.fix <- aux %>% subset(ADVT=="fix")
    aux     <- aux %>% subset(HCRT == "2o3" & ADVT=="iny")
    p <- ggplot(aux, aes(x=UCP.l_u, y=value, fill=UCP.l_u))+
      geom_bar(stat="identity")+
      facet_grid(FHIST + term ~ STKN)+
      ylab(perflabels[perfnms==ind])+
      scale_fill_brewer(palette="Paired", type = "qual")+
      theme(axis.text.x=element_blank())
    if(length(grep("Risk", ind))>0){
      p <- p + geom_hline(yintercept = 0.05, linetype = "longdash")
      p <- p + geom_hline(aes(yintercept = value), data = subset(aux.fix, indicator == "Risk3.Blim"), col="red")
      p <- p + ylim(c(0,1)) 
    }
    print(p)
  }
  
  for (tt in unique(df$term)){ #c("short", "long")
    for (ind in perfnms){
      aux <- subset(df, indicator %in% ind & term == tt)
      aux.fix <- aux %>% subset(ADVT=="fix")
      aux     <- aux %>% subset(HCRT == "2o3" & ADVT=="iny")
      p <- ggplot(aux, aes(x=UCP.l_u, y=value, fill=UCP.l_u))+
        geom_bar(stat="identity")+
        facet_grid(FHIST ~ STKN)+
        ylab(perflabels[perfnms==ind])+
        scale_fill_brewer(palette="Paired", type = "qual")+
        theme(axis.text.x=element_blank())+
        ggtitle(paste("TERM =", tt))
      if(length(grep("Risk", ind))>0){
        p <- p + geom_hline(yintercept = 0.05, linetype = "longdash")
        p <- p + geom_hline(aes(yintercept = value), data = subset(aux.fix, indicator == "Risk3.Blim"), col="red")
        p <- p + ylim(c(0,1)) 
      }
      print(p)
    }
  }
  
dev.off()

# geom_bars

pdf(file.path(plot.dir,paste("plot_compare_across_OMs.pdf",sep="")), width=14, onefile=T)

  # all together
  
  for (ind in perfnms[c(1,5)]){ #!XXX
    aux <- subset(df, indicator %in% ind & term %in% c("short", "long"))
    aux.fix <- aux %>% subset(ADVT=="fix") %>% select(-HCRT)
    rules <- unique(aux$HCRT)[unique(aux$HCRT)!="ft0"]
    aux.fix <- aux.fix[rep(1:nrow(aux.fix),times = length(rules)),]
    aux.fix$HCRT <- rep(rules, each=nrow(aux.fix)/length(rules))
    aux     <- aux %>% subset(HCRT!="ft0" & ADVT=="iny")
    p <- ggplot(aux, aes(x=UCP.l_u, y=value, fill=UCP.l_u))+
      geom_bar(stat="identity")+
      facet_grid(HCRT + term ~ OMnam)+
      ylab(perflabels[perfnms==ind])+
      scale_fill_brewer(palette="Paired", type = "qual")+
      theme(axis.text.x=element_text(angle = 45, hjust = 1, size = 9))
    if(length(grep("Risk", ind))>0){
      p <- p + geom_hline(yintercept = 0.05, linetype = "longdash")
      p <- p + geom_hline(aes(yintercept = value), data = subset(aux.fix, indicator == "Risk3.Blim"), col="red")
      p <- p + ylim(c(0,1))
    }
    print(p)
  }
  
  # separate plots
  
  for (aa in unique(df$STKN)){
    for (bb in unique(df$FHIST)){
      for (ind in perfnms){
        aux <- subset(df, STKN==aa & FHIST==bb & indicator %in% ind & term %in% c("short", "long"))
        aux.fix <- aux %>% subset(ADVT=="fix") %>% select(-HCRT)
        rules <- unique(aux$HCRT)[unique(aux$HCRT)!="ft0"]
        aux.fix <- aux.fix[rep(1:nrow(aux.fix),times = length(rules)),]
        aux.fix$HCRT <- rep(rules, each=nrow(aux.fix)/length(rules))
        aux     <- aux %>% subset(HCRT!="ft0" & ADVT=="iny")
        p <- ggplot(aux, aes(x=UCP.l_u, y=value, fill=UCP.l_u))+
          geom_bar(stat="identity")+
          facet_grid(HCRT ~ term)+
          ylab(perflabels[perfnms==ind])+
          scale_fill_brewer(palette="Paired", type = "qual")+
          ggtitle(paste("STOCK =", aa, ", FHIST =", bb))
        if(length(grep("Risk", ind))>0){
          p <- p + geom_hline(yintercept = 0.05, linetype = "longdash")
          p <- p + geom_hline(aes(yintercept = value), data = subset(aux.fix, indicator == "Risk3.Blim"), col="red")
          p <- p + ylim(c(0,1))
        }
        print(p)
      }
    }

}
dev.off()

# # radar plots
# 
# pdf(file.path(plot.dir,paste("radar_across_OMs.pdf",sep="")), onefile=T)
# for (aa in c("STK1","STK2")){
#   for (bb in c("fopt","flow","fhigh")){
#     for (cc in c(0.75)){ #c(0.5, 0.75, 1)
#       for (dd in c("low")){ #c("low","high","iav","iav2")
#         aux <- subset(df.scaled, term=="long" & STKN==aa & FHIST==bb & SIGR==cc & HCRT %in% c("1o2","2o3") & CVID==dd)
#         aux <- aux[order(aux$indicator), ] 
#         p <- ggplot(data=aux, aes(x=indicator, y=value2, col=UCP.l_u, fill=UCP.l_u, group=UCP.l_u))+
#           #  geom_polygon(alpha=0.2, lwd=1)+
#           geom_polygon(fill=NA, lwd=1)+
#           geom_point(cex=1.5)+
#           coord_radar()+
#           facet_grid( ~ HCRT)+
#           theme_bw()+
#           theme(text=element_text(size=14),
#                 strip.text=element_text(size=14),
#                 title=element_text(size=18,face="bold"))+
#           ylab("")+
#           ylim(c(0,1))+
#           ggtitle(paste(aa,bb,cc,dd,sep="_"))
#         print(p)
#       }
#     }
#   }
# }
# dev.off()
# 
# # heatmaps
# 
# ggplot(subset(df, term=="long" & indicator=="Risk3.Blim"), aes(x = MP, y = OM, fill = value)) + 
#   geom_tile()+
#   scale_fill_gradient(limits=c(0,1), direction=1) 


# #==============================================================================
# # CVID/IAV
# #==============================================================================
# 
# pdf(file.path(plot.dir,paste("plot_RiskBlim_by_CVIDIAVrat.pdf",sep="")), onefile=T)
#   ggplot(subset(df, term=="long" & indicator=="Risk3.Blim"), aes(x=Ratio, y=value, col=interaction(STKN,FHIST)))+
#     geom_point()+
#     ylim(c(0,1))
#   ggplot(subset(df, term=="long" & indicator=="Risk3.Blim" & UCPL == 0 & UCPU == 0), aes(x=Ratio, y=value, col=interaction(STKN,FHIST)))+
#     geom_point()+
#     ylim(c(0,1))
#   ggplot(subset(df, term=="long" & indicator=="Risk3.Blim" & value<0.1), 
#          aes(x=Ratio, y=value, col=interaction(STKN,FHIST)))+
#     geom_point()+
#     ylim(c(0,.1))
# dev.off()


#==============================================================================
# COMPARISON CALENDARS
#==============================================================================

perfnms    <- c("Risk3.Blim","catch.MSY")
perflabels <- c("Risk3.Blim","catch/MSY")

aux <- df %>% select(SCENARIO:term, indicator:value, OMnam, ADVT, HCRT, UCP.l_u) %>% 
  filter(indicator %in% perfnms & ADVT != "fix") %>% 
  spread("indicator","value") %>% 
  mutate(ADVT = as.character(ADVT))

# lapply(aux, unique)
# # term, OMnam, ADVT, HCRT, UCP.l_u

pdf(file.path(plot.dir,paste("plot_compare_by_advt_all.pdf",sep="")), onefile=T)

  # Risk vs. catch

  p <- ggplot(aux, aes(x=catch.MSY, y=Risk3.Blim, shape=ADVT, col=UCP.l_u))+
    geom_point(stat="identity")+
    facet_grid(OMnam ~ HCRT)+
    geom_hline(yintercept = 0.05, linetype = "longdash")+
    scale_fill_brewer(palette="Set2", type = "qual")
  
  print(p)
  
  for(tt in unique(aux$term)) for (ucp in unique(aux$UCP.l_u)) {
    
    aux2 <- aux %>% filter(term == tt & UCP.l_u == ucp) %>% 
      mutate(slope.or = Risk3.Blim/catch.MSY)
    
    p <- ggplot(aux2, aes(x=catch.MSY, y=Risk3.Blim, shape=ADVT))+
      geom_point(stat="identity")+
      facet_grid(OMnam ~ HCRT)+
      geom_hline(yintercept = 0.05, linetype = "longdash")+
      geom_abline(data = aux2, aes(intercept = 0, slope = slope.or, col = ADVT))+
      scale_fill_brewer(palette="Set2", type = "qual")+ 
      ggtitle(paste0("UCP: ", ucp, " - term: ", tt))
    
    print(p)
    
  }
  
dev.off()

