################################################################################
#  Summary functions for WKDLSSLS - summary results                            #
#                                                                              #
#  based on previous functions used in anchovy and Ibpil MPs                   #
#------------------------------------------------------------------------------#
#   Sonia Sanchez & Leire Ibaibarriaga (AZTI-Tecnalia)                         #
#   created:  26/07/2019                                                       #  
################################################################################

# perfInd_wkdlssls.R - summary statistics for each scenario
# wkdlssls2019/fun/perfInd_wkdlssls.R

# Copyright: AZTI, 2019
# Author: Sonia Sanchez (AZTI) (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3

#------------------------------------------------------------------------------#
# perfInd(obj) :: performance indicators for WKDLSSLS  
#------------------------------------------------------------------------------#


perfInd <- function(file.RData, scenario, yrs.hrmax=c(21:30), yrs.hist=c(1:30)){

  # FOR TRIALS 
  # scenario <- "sc000002"
  # file.RData <- file.path("output","output_stats",paste("outstats_",scenario,".RData", sep=""))

  
  # load library
  require(dplyr)
  
  # load results ("biosem","bio","adv","pars","idx","refpts") 

  load(file.RData)  
  obj.bio <- get( paste(scenario,"bio",sep="_") )
  obj.biosem <- get( paste(scenario,"biosem",sep="_") )
  obj.adv <- get( paste(scenario,"adv",sep="_") )
  obj.pars <- get( paste(scenario,"pars",sep="_") )
  obj.idx <- get( paste(scenario,"idx",sep="_") )
  obj.refpts <- get( paste(scenario,"refpts",sep="_") )
  
  # make compatible outputs
  obj.bio    <- obj.bio %>% ungroup() %>% 
    mutate(year = as.character(year), iter = as.character(iter))
  obj.biosem <- obj.biosem %>%  ungroup() %>%
    mutate(year = as.character(year), season = as.character(season), iter = as.character(iter))
  obj.adv    <- obj.adv %>% ungroup() %>% 
    mutate(year = as.character(year), iter = as.character(iter))
  
  # add refpts
  
  obj.bio <- obj.bio %>% mutate(R0=obj.refpts["R0", iter],
                      B0=obj.refpts["B0", iter],
                      Blim=obj.refpts["Blim", iter],
                      Fmsy=obj.refpts["Fmsy", iter],
                      MSY=obj.refpts["MSY", iter],
                      SSBini=subset(obj.bio, year == yrs.hist[length(yrs.hist)])$ssb[iter])
  
  # add index
  
  if (!is.null(obj.idx)){
    obj.idx <- obj.idx %>% select(year, iter, data)%>%
               mutate(year=as.character(year),
                      iter=as.character(iter)) %>%
               rename(index=data)
    
    obj.bio <- left_join(obj.bio, obj.idx, by=c("year","iter")) 
  }else{
    obj.bio$index <- NA
  } 
  
  # compute hr
  
  obj.bio <- obj.bio %>% mutate(hr=catch/index)
  
  # seasonal values 
  
  tmp <- obj.biosem %>% group_by(stock, scenario, iter, year) %>%
    summarize(catch.sem1=catch[season==1], 
              catch.sem2=catch[season==2], 
              f.sem1=f[season==1], 
              f.sem2=f[season==2])
  
  obj.bio <- left_join(obj.bio, tmp, by=c("stock", "scenario", "iter", "year"))
  
  # compute catch from the mp calendar
  
  if(obj.pars$ADVT=="iny"){
    obj.bio <- obj.bio %>% 
      group_by(stock, scenario, iter) %>% 
      arrange(as.numeric(year)) %>%
      mutate(catch.mp=catch.sem2 + lead(catch.sem1))  
  }else{
    obj.bio <- obj.bio %>% mutate(catch.mp=catch)
  }
  
  # add tac 
  
  obj.bio <-  left_join(obj.bio, select(obj.adv, stock, scenario, iter, year, tac), by=c("stock", "scenario", "iter", "year")) 
  
  # compute quotaUp based on management calendar
  
  obj.bio <- obj.bio %>% mutate(quotaUpt=catch.mp/tac)

# statistics by year
  
  out.yr <- obj.bio %>% 
    group_by(stock, scenario, year) %>%
    summarize(rec=mean(rec, na.rm=TRUE),
              ssb=mean(ssb, na.rm=TRUE),
              index=0, #! PENDING
              tac=mean(tac,na.rm=TRUE),
              catch=mean(catch, na.rm=TRUE),
              catch.sem1=mean(catch.sem1, na.rm=TRUE),
              catch.sem2=mean(catch.sem2, na.rm=TRUE),
              f=mean(f, na.rm=TRUE),
              f.sem1=mean(f.sem1, na.rm=TRUE),
              f.sem2=mean(f.sem2, na.rm=TRUE))
  
  # return(as.data.frame(out.yr))

      
# statistics in historical part (NOT in output now)

  out.hist <- obj.bio %>% subset(year %in% max(yrs.hist)) %>%
            group_by(stock, scenario) %>%
            summarize(depl.ini= mean(ssb/B0))
  
  # historic IAV
  out.iav <- obj.bio %>%  subset(year %in% yrs.hist) %>% 
    group_by(stock, scenario, iter) %>% 
    arrange(as.numeric(year)) %>%
    mutate(yr.diff = (log(lead(biomass)) - log(biomass))^2) %>%
    summarize(IAV.iter = sqrt(mean(yr.diff, na.rm = TRUE))) %>%
    group_by(stock, scenario) %>%
    summarize(IAVhist=mean(IAV.iter))
  
  out.hist <- full_join(out.hist, out.iav)
  
  # risk in last historic year
  out.risk <- obj.bio %>% subset(year %in% max(yrs.hist)) %>%
    group_by(stock, scenario) %>%
    summarize(RiskBlim.ini=mean(ssb<Blim))
  
  out.hist <- full_join(out.hist, out.risk)
  
# statistics in short-, medium- and long-term
    
  term.list <- list(short=c(31:35), mid=c(36:40), long=c(51:60))  

  out <- NULL
  
  for (i in 1:length(term.list)){
    yrs.term <- term.list[[i]]
    out.mean <- obj.bio %>% subset(year %in% yrs.term) %>%
      group_by(stock, scenario) %>%
      summarize(term=names(term.list)[i],
                catch=mean(catch, na.rm=TRUE),
                f=mean(f, na.rm=TRUE),
                hr=mean(hr, na.rm=TRUE),
                ssb=mean(ssb, na.rm=TRUE),
                catch.iyv=median(catch.iyv, na.rm=TRUE),
                catch.MSY=median(catch/MSY, na.rm=TRUE),
                ssb.B0=median(ssb/B0, na.rm=TRUE),
                f.Fmsy=median(f/Fmsy, na.rm=TRUE), 
                quotaUpt=mean(quotaUpt, na.rm=TRUE))
    
    out.iav <- obj.bio %>%  
      group_by(stock, scenario, iter) %>%
      arrange(as.numeric(year)) %>%
      mutate(yr.diff = (log(lead(biomass)) - log(biomass))^2) %>%
      subset(year %in% yrs.term) %>% 
      summarize(IAV.iter = sqrt(mean(yr.diff, na.rm = TRUE))) %>%
      group_by(stock, scenario) %>%
      summarize(IAV=mean(IAV.iter))
    
    out.r1r3 <- obj.bio %>% subset(year %in% yrs.term) %>%
      group_by(stock, scenario, year) %>%
      summarize(Risk.yr.Collapse=mean(ssb<(0.1*B0) ),
                Risk.yr.Blim=mean(ssb<Blim) ) %>%
      group_by(stock, scenario) %>%
      summarize(Risk1.Collapse=mean(Risk.yr.Collapse),
                Risk1.Blim=mean(Risk.yr.Blim),
                Risk3.Collapse=max(Risk.yr.Collapse),
                Risk3.Blim=max(Risk.yr.Blim) )
    
    out.r2 <- obj.bio %>% subset(year %in% yrs.term) %>%
      group_by(stock, scenario, iter) %>%
      summarize(Risk.iter.Collapse=any(ssb<(0.1*B0) ),
                Risk.iter.Blim=any(ssb<Blim) ) %>%
      group_by(stock, scenario) %>%
      summarize(Risk2.Collapse=mean(Risk.iter.Collapse),
                Risk2.Blim=mean(Risk.iter.Blim))
    
    out.hr <- obj.bio %>% group_by(stock, scenario, iter) %>% 
      mutate(hrmax=max(hr[year %in% yrs.hrmax])) %>%
      subset(year %in% yrs.term) %>%
      group_by(stock, scenario) %>%
      summarize(Risk.hrmax=mean( hr > hrmax, na.rm=TRUE))
    
    out.cvar <- obj.bio %>% subset(year %in% yrs.term) %>%
      group_by(stock, scenario, iter) %>%
      summarize(Cvar.iter=var(catch)) %>%
      group_by(stock, scenario) %>%
      summarize(catch.var=mean(Cvar.iter))
    
    out.lowc <- obj.bio %>% subset(year %in% yrs.term) %>%
      group_by(stock, scenario, iter) %>%
      summarize(catch.iter.low=any(catch<=1000)) %>%
      group_by(stock, scenario) %>%
      summarize(catch.low=mean(catch.iter.low))
    
    out.ucap <- obj.bio %>% 
      group_by(stock, scenario, iter) %>%
      arrange(as.numeric(year)) %>%
      mutate(tac.pdiff = (tac/lag(tac))-1, 
             tac.low = case_when(tac.pdiff < 0 ~ -tac.pdiff, 
                                 TRUE          ~ 0*NA),
             tac.upp = case_when(tac.pdiff > 0 ~ tac.pdiff, 
                                 TRUE          ~ 0*NA)) %>%
      subset(year %in% yrs.term) %>% 
      group_by(stock, scenario) %>% 
      summarize(ucplow = ifelse(obj.pars$UCPL>0, mean(round(tac.low,2) == obj.pars$UCPL, na.rm = TRUE), 0),
                ucpup  = ifelse(obj.pars$UCPU>0, mean(round(tac.upp,2) == obj.pars$UCPU, na.rm = TRUE), 0))
    
    out.icv <- obj.bio %>% 
      group_by(stock, scenario, iter) %>%
      arrange(as.numeric(year)) %>%
      mutate(catch.pdiff = (catch.mp/lag(catch.mp))-1) %>%
      subset(year %in% yrs.term) %>% 
      group_by(stock, scenario) %>%
      summarize(ICVperc_average = mean(catch.pdiff, na.rm = TRUE),
                ICVabsperc_average = mean(abs(catch.pdiff), na.rm = TRUE),
                ICVperc_median = median(catch.pdiff, na.rm = TRUE),
                ICVabsperc_median  = median(abs(catch.pdiff), na.rm = TRUE))

    out.ibv <- obj.bio %>% 
      group_by(stock, scenario, iter) %>%
      arrange(as.numeric(year)) %>%
      mutate(ssb.pdiff = (ssb/lag(ssb))-1) %>%
      subset(year %in% yrs.term) %>% 
      group_by(stock, scenario) %>%
      summarize(IBVperc_average = mean(ssb.pdiff, na.rm = TRUE),
                IBVabsperc_average = mean(abs(ssb.pdiff), na.rm = TRUE), 
                IBVperc_median = median(ssb.pdiff, na.rm = TRUE),
                IBVabsperc_median  = median(abs(ssb.pdiff), na.rm = TRUE))
    
    out.term <- left_join(out.mean, out.hist, by=c("stock","scenario")) # initial depletion
    out.term <- left_join(out.term, out.iav, by=c("stock","scenario"))  # IAV in the historical period
    out.term <- left_join(out.term, out.r1r3, by=c("stock","scenario"))
    out.term <- left_join(out.term, out.r2, by=c("stock","scenario"))
    out.term <- left_join(out.term, out.hr, by=c("stock","scenario"))
    out.term <- left_join(out.term, out.cvar, by=c("stock","scenario"))
    out.term <- left_join(out.term, out.lowc, by=c("stock","scenario"))
    out.term <- left_join(out.term, out.ucap, by=c("stock","scenario"))
    out.term <- left_join(out.term, out.icv, by=c("stock","scenario"))
    out.term <- left_join(out.term, out.ibv, by=c("stock","scenario"))
    
    out <- rbind(out, out.term)
  }
  
  return(as.data.frame(out))
}