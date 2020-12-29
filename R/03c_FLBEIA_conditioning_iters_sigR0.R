################################################################################
#  WKDLSSLS - FLBEIA conditioning (initial population with iterations)         # 
#------------------------------------------------------------------------------#
#   Sonia Sanchez (AZTI-Tecnalia)                                              #
#   created:  16/14/2019                                                       #
#   modified:                                                                  #
################################################################################

# 03_FLBEIA_conditioning_iters.R - conditioning FLBEIA
# ~/WKDLSSLS_2019/R/03_FLBEIA_conditioning_iters.R

# Copyright: AZTI, 2019
# Author: Sonia Sanchez (AZTI) (<ssanchez@azti.es>)
#         Based on http://www.flr-project.org/doc/Data_Poor_MSE_in_FLBEIA.html
#
# Distributed under the terms of the GNU GPLv3


#==============================================================================
#                                                                          ----
#==============================================================================

rm(list=ls())


#==============================================================================
# WORKING DIRECTORY                                                        ----
#==============================================================================

# wd <- "C:/use/GitHub/ssanchezAZTI/WKDLSSLS_2019" # main directory
# setwd(wd)


#==============================================================================
# LOAD LIBRARIES AND FUNCTIONS                                             ----
#==============================================================================

# load libraries
library(FLBEIA)


#==============================================================================

#==============================================================================

t1 <- Sys.time()

# get the job id from the environment
it <- as.numeric(Sys.getenv("SGE_TASK_ID"))
cat("Starting run i = ",it,"\n")

# Set seed for reproducible results:
set.seed(86314)


#==============================================================================
# OM                                                                 ----
#==============================================================================

# om <- "om001" # om <- as.character(Sys.getenv("SC"))

pars <- read.table( file.path("./input", "list_oms_sigR0.csv"), sep = ",", header = TRUE)

om <- ac(pars$OM[it])

om.pars <- subset(pars, OM == om)

# STOCK
# - STK1 : anchovy and Norway pout type
# - STK2 : sprat and sardine type

stkn.opt <- c("STK1", "STK2")

stkn <- ac(om.pars$STKN)
if (!stkn %in% stkn.opt)
  stop(paste("Check 'STKN' value for OM ", om, " in './input/list_oms.csv' file."))

# LIFE HISTORY
# - bc       : base case (steepness = 0.75)
# - lowprod  : low productivity (steepness = 0.5)
# - highprod : high productivity (steepness = 0.9)

lhsc.opt <- c("bc", "lowprod", "highprod")

lhsc <- ac(om.pars$LHSC)
if (!lhsc %in% lhsc.opt)
  stop(paste("Check 'LHSC' value for OM ", om, " in './input/list_oms.csv' file."))


# RECRUITMENT uncertainty
# 0.5, 0.75, 1

sigR <- om.pars$SIGR


# INITIAL EXPLOITATION
# (increase linearly on first 10 years and then keep at that level during the next 20 years)
# - fopt : Fmsy
# - flow : 0.5*fopt
# - fhigh: 2*fopt

fhist.opt <- c( "fopt", "flow", "fhigh")

fhist <- ac(om.pars$FHIST)
if (!fhist %in% fhist.opt)
  stop(paste("Check 'FHIST' value for OM ", om, " in './input/list_oms.csv' file."))


cvfh  <- om.pars$CVFH


# # INDEX
# # - b1p : biomass 1+
# 
# idxt.opt <- "b1p"
# 
# idxt <- ac(om.pars$IDXT)
# if (!idxt %in% idxt.opt)
#   stop(paste("Check 'IDXT' value for OM ", om, " in './input/list_oms.csv' file."))
# 
# 
# # OBSERVATION ERROR INDEX
# # - low  : 0.25
# # - high : 0.5
# # - iav  : IAV
# # - iav2 : 2*IAV
# 
# cvid.opt <- c( "low", "high", "iav", "iav2")
# 
# cvid <- ac(om.pars$CVID)
# if (!cvid %in% cvid.opt)
#   stop(paste("Check 'CVID' value for OM ", om, " in './input/list_oms.csv' file."))
# 
# cvid <- ac(om.pars$CVID)


#==============================================================================
# LOAD DATA                                                                ----
#==============================================================================

load( file.path("./input", paste(stkn,lhsc,"_data.RData",sep="")))
# biols, SRs, fleets, indices, advice, 
# main.ctrl, biols.ctrl, fleets.ctrl, obs.ctrl, assess.ctrl, advice.ctrl


#==============================================================================
# FLBEIA input objects (for selected om)                             ----
#==============================================================================

ages     <- dimnames(biols[[1]]@n)$age
yrs      <- dimnames(biols[[1]]@n)$year
proj.yrs <- ac(main.ctrl$sim.years[1]:main.ctrl$sim.years[2])
hist.yrs <- yrs[!yrs %in% proj.yrs]

na <- length(ages)
ny <- length(yrs)
ns <- dim(biols[[1]]@n)[4]
ni <- dim(biols[[1]]@n)[6]


# RECRUITMENT UNCERTAINTY
  
  SRs[[1]]@uncertainty[,-1,,,] <- rlnorm((ny-1)*ns*ni,0,sigR)
  
# INITIAL EXPLOITATION
  
  Fmsy <- advice.ctrl[[1]][['ref.pts']]["Fmsy",1]
  
  if (fhist == "fopt") {         Ftgt <- Fmsy
  } else if (fhist == "flow")  { Ftgt <- Fmsy * 0.5 
  } else if (fhist == "fhigh") { Ftgt <- Fmsy * 2
  } else stop("'fhist' value not valid")
  
  # Introducing uncertainty in the Ftgt 
  # (same F at age in both seasons)
  
  fyr <- FLQuant( c(seq(0,Ftgt,length.out = 10), rep(Ftgt, length(hist.yrs)-10)), dimnames = list(age="all", year=hist.yrs, iter=1:ni)) *
    rlnorm(length(hist.yrs) * ni, 0, sqrt(log(cvfh^2+1))) 
  
  # - Fmult given Fbar selectivity
  
  msel <- quantMeans(fleets[[1]]@metiers[[1]]@catches[[1]]@catch.q[ac(biols[[1]]@range["minfbar"]:biols[[1]]@range["maxfbar"]),hist.yrs])
  
  Fmult_s1 <- fyr/msel[,,,1,]; Fmult_s1[is.na(Fmult_s1)] <- 0
  Fmult_s2 <- fyr/msel[,,,2,]; Fmult_s2[is.na(Fmult_s2)] <- 0
  
  # - different F percentages by semester
  
  fleets[[1]]@metiers[[1]]@catches[[1]]@catch.q[,hist.yrs,,1,] <- 
    sweep(fleets[[1]]@metiers[[1]]@catches[[1]]@catch.q[,hist.yrs,,1,], 2:6, Fmult_s1, "*") * fprop_s1
  fleets[[1]]@metiers[[1]]@catches[[1]]@catch.q[,hist.yrs,,2,] <- 
    sweep(fleets[[1]]@metiers[[1]]@catches[[1]]@catch.q[,hist.yrs,,2,], 2:6, Fmult_s2, "*") * (1-fprop_s1)
  
  
  # Generating historical population
  
  for(yr in 1:length(hist.yrs)) {
    
    for (s in 1:ns) {
      
      if (yr > 1 | s > 1) {
        
        yy <- ifelse( s==1, yr-1,  yr)
        ss <- ifelse( s==1,   ns, s-1)
        
        f0 <- fleets[[1]]@metiers[[1]]@catches[[1]]@catch.q[,yy,,ss,]
        z0 <- biols[[1]]@m[,yy,,ss,] + f0
        
        # nage: adults
        
        if (s==1) { aa <- ages[-c(na-1,na)]
        } else aa <- ages[-c(1,na)]
        
        biols[[1]]@n[-c(1,na),yr,,s,] <- biols[[1]]@n[aa,yy,,ss,]*exp(-z0[aa,])
        
        if (s==1) {
          biols[[1]]@n[na,yr,,s,]     <- biols[[1]]@n[na-1,yy,,ss,]*exp(-z0[na-1,]) + 
                                          biols[[1]]@n[na,yy,,ss,]*exp(-z0[na,])
        } else 
          biols[[1]]@n[na,yr,,s,]     <- biols[[1]]@n[na,yy,,ss,]*exp(-z0[na,])
        
        # nage: recruits
        
        yr.rec <- yr-SRs[[1]]@timelag["year",s]
        ss.ssb <- SRs[[1]]@timelag["season",s]
        
        SRs[[1]]@ssb[,yr.rec,,ss.ssb,] <- quantSums(n(biols[[1]]) * wt(biols[[1]]) * fec(biols[[1]])*mat(biols[[1]]) * 
                                              exp(-biols[[1]]@m*spwn(biols[[1]])), na.rm=TRUE)[,yr.rec,,ss.ssb,]
        
        biols[[1]]@n[1,yr,,s] <- predict( predictModel(model=SRs[[1]]@model, params=FLPar(SRs[[1]]@params[,yr,s,])), 
                                        ssb=SRs[[1]]@ssb[,yr.rec,,ss.ssb,]) * 
                                SRs[[1]]@proportion[,yr,,s,] * SRs[[1]]@uncertainty[,yr,,s,]
                                
        # equivalent to: FLBEIA:::SRsim(SRs[[1]], year = yr, season = s)@rec[,yr,,s,]
        
      }
      
      # cage
      
      f1 <- fleets[[1]]@metiers[[1]]@catches[[1]]@catch.q[,yr,,s,]
      z1 <- biols[[1]]@m[,yr,,s,] + f1
      
      fleets[[1]]@metiers[[1]]@catches[[1]]@landings.n[,yr,,s,] <-
        (f1/z1) * (1-exp(-z1)) * biols[[1]]@n[,yr,,s,]
      
    }
    
  }
  
  fleets[[1]]@metiers[[1]]@catches[[1]]@landings[,hist.yrs,] <- 
    quantSums(fleets[[1]]@metiers[[1]]@catches[[1]]@landings.n[,hist.yrs,] * 
                fleets[[1]]@metiers[[1]]@catches[[1]]@landings.wt[,hist.yrs,])
  
  # # Check total landings by semester
  # fleets[[1]]@metiers[[1]]@catches[[1]]@landings
  # fleets[[1]]@metiers[[1]]@catches[[1]]@landings[,hist.yrs,,1,] / 
  #   seasonSums(fleets[[1]]@metiers[[1]]@catches[[1]]@landings[,hist.yrs,])
  # fleets[[1]]@metiers[[1]]@catches[[1]]@landings[,hist.yrs,,2,] / 
  #   seasonSums(fleets[[1]]@metiers[[1]]@catches[[1]]@landings[,hist.yrs,])
  
  advice$TAC[,hist.yrs,] <- seasonSums(quantSums(catchWStock(fleets, stock = stkn)))[,hist.yrs,]
  
  
  # Cobb-Douglas: q
  
  Cage <- catchWStock(fleets, stock = stkn)[,hist.yrs,]
  Bage <- (n(biols[[1]])*wt(biols[[1]])*exp(-m(biols[[1]])/2))[,hist.yrs,]
  E    <- (effort(fleets[[1]]) * fleets[[1]]@metiers[[1]]@effshare)[,hist.yrs,]
  
  # - historical
  catch.q(fleets[[1]]@metiers[[1]]@catches[[1]])[,hist.yrs,] <- 
    Cage / sweep(Bage, c(2:6), E, "*")
  catch.q(fleets[[1]]@metiers[[1]]@catches[[1]])[1,hist.yrs,,1] <- 0
  
  # - projection
  fleets[[1]]@metiers[[1]]@catches[[1]]@catch.q[,proj.yrs,] <- 
    yearMeans(fleets[[1]]@metiers[[1]]@catches[[1]]@catch.q[,hist.yrs[(length(hist.yrs)-5+1):length(hist.yrs)],])
  
  # - Catches by semester
  fleets[[1]]@metiers[[1]]@catches[[1]]@landings[,hist.yrs,,1,] / 
    seasonSums(fleets[[1]]@metiers[[1]]@catches[[1]]@landings[,hist.yrs,])
  
  
# OBSERVATION ERROR INDEX
  
  B <- quantSums(n(biols[[1]])*wt(biols[[1]]))[,hist.yrs,,1]
  
  IAV <- as.numeric(iterMeans(sqrt(yearSums((log(B[,-1,]) - log(B[,-length(hist.yrs),]))^2) / (length(hist.yrs)-1))))
  
  cvidx <- 0 
  # if (cvid == "low")         { cvidx <- 0.25
  # } else if (cvid == "high") { cvidx <- 0.5
  # } else if (cvid == "iav")  { cvidx <- IAV
  # } else if (cvid == "iav2") { cvidx <- IAV*2
  # }
  
  idxyr <- dimnames(indices[[1]][[1]]@index)$year
  
  indices[[1]][[1]]@index.q <- indices[[1]][[1]]@index.q * 
    rlnorm( length(idxyr)*ni, meanlog = 0, sdlog = sqrt(log(cvidx^2+1)))
  
  
  # observed indices
  
  yr <- idxyr[idxyr %in% hist.yrs]
  
  a1plus <- which(dimnames(biols[[1]]@n)$age=='1'):dim(biols[[1]]@n)[1]
  
  
  idxss <- findInterval( mean(range(indices[[1]][[1]])[c("startf","endf")]), 
                         seq(0,1,length = ns+1))
  
  
  indices[[1]][[1]]@index[,yr,] <- indices[[1]][[1]]@index.q[,yr,] * 
    quantSums( n(biols[[1]])[a1plus,yr,,idxss,] * wt(biols[[1]])[a1plus,yr,,idxss,])
  
  indices[[1]][[1]]@index[,proj.yrs,] <- NA
  
  # # MSY Btrigger proxy (from ICES DLS Guidance Report 2012, page 14)
  # 
  # Bmsy <- round(min(indices[[1]][[1]]@index[,yr,], na.rm = TRUE),0) 
  # #Bmsy <- apply(indices[[1]][[1]]@index[,yr,], c(3:6), quantile, 0.25, na.rm = TRUE)



#==============================================================================
# Save FLBEIA inputs                                                       ----
#==============================================================================

save( biols, SRs, fleets, indices, advice, 
      main.ctrl, biols.ctrl, fleets.ctrl, obs.ctrl, assess.ctrl, advice.ctrl, 
      IAV, fyr, 
      file = file.path("./input/iters/sigR0",paste("data_",om,".RData",sep="")))


t2 <- Sys.time()

t2 - t1
  
