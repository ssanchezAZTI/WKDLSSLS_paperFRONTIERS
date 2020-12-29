################################################################################
#  WKDLSSLS - FLBEIA conditioning                                              # 
#------------------------------------------------------------------------------#
#   Sonia Sanchez (AZTI-Tecnalia)                                              #
#   created:  16/14/2019                                                       #
#   modified:                                                                  #
################################################################################

# 02_FLBEIA_conditioning.R - conditioning FLBEIA
# ~/WKDLSSLS_2019/R/02_FLBEIA_conditioning.R

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

# # directory with results
# res.dir  <- file.path("./output")
# # directory with plots
# plot.dir <- file.path("./plots")

#==============================================================================
# LOAD LIBRARIES AND FUNCTIONS                                             ----
#==============================================================================

# load libraries
library(FLBEIA)


#==============================================================================
# SCENARIO                                                                 ----
#==============================================================================

# STOCK
# - STK1 : anchovy and Norway pout type
# - STK2 : sprat and sardine type

stkn.opt <- c("STK1", "STK2")  

# LIFE HISTORY
# - bc       : base case (steepness = 0.75)
# - lowprod  : low productivity (steepness = 0.5)
# - highprod : high productivity (steepness = 0.9)

lhsc.opt <- c("bc", "lowprod", "highprod")

# LI: These parameters are not needed in this script
# idxt <- "b1p" # b1+ index
# sigR <- 0.75


for (stkn in stkn.opt) for (lhsc in lhsc.opt) {
    
  #==============================================================================
  # LOAD DATA                                                                ----
  #==============================================================================
  
  load( file.path("./input",paste(stkn,lhsc,"_dataLH.RData",sep="")))
  # stk; sr_model; sr_params; ref.pts
  
  #==============================================================================
  # GENERAL OBJECTS                                                          ----
  #==============================================================================
  
  # years
  
  hist.yrs  <- dimnames(stk)$year
  hist.nyr  <- length(hist.yrs)
  
  first.yr <- as.numeric(hist.yrs[1])
  proj.yr  <- as.numeric(hist.yrs[hist.nyr])+1
  proj.nyr <- 30
  last.yr <- proj.yr+(proj.nyr-1)
  
  hist.yrs <- first.yr:(proj.yr-1)
  proj.yrs <- proj.yr:last.yr
  yrs <- first.yr:last.yr
  nyr <- length(yrs)
  
  # ages
  
  ages <- dimnames(stk@stock.n)$age
  nage <- length(ages)
  
  # iterations
  
  ns  <- dim(stk)[4]
  
  # iterations
  
  nit  <- dim(stk)[6]
  
  
  # empty FLQuants
  
  flq  <- FLQuant(1, dim = c(1,nyr,1,ns,1,nit), 
                  dimnames = list(quant = "all", year = yrs, iter = 1:nit))
  flq0 <- FLQuant(0, dim = c(1,nyr,1,ns,1,nit), 
                  dimnames = list(quant = "all", year = yrs, iter = 1:nit))
  flqa  <- FLQuant(1, dim = c(nage,nyr,1,ns,1,nit), 
                   dimnames = list(age = ages, year = yrs, iter = 1:nit))
  flqa0 <- FLQuant(0, dim = c(nage,nyr,1,ns,1,nit), 
                   dimnames = list(age = ages, year = yrs, iter = 1:nit))
  flq1s <- FLQuant(1, dim = c(1,nyr,1,1,1,nit), 
                   dimnames = list(quant = "all", year = yrs, iter = 1:nit))
  
  # Expand years in FLStock
  stk <- window( stk, start=first.yr, end=last.yr)
  # rm(stk)
  
  
  #==============================================================================
  # FLBEIA objects                                                           ----
  #==============================================================================
  
  
  # biols  ----
  #~~~~~~~~~~~~~~
  
  # age structured stock
  
  minage  <- as.numeric(ages[1])
  maxage  <- as.numeric(ages[nage])
  minfbar <- range(stk)[["minfbar"]]
  maxfbar <- range(stk)[["maxfbar"]]
  
  biols <- FLBiols( stk = FLBiol(name = name(stk), 
                                 desc = "",
                                 range = c(min = minage, max = maxage, plusgroup = maxage, 
                                           minyear = first.yr, maxyear = last.yr, minfbar = minfbar, maxfbar = maxfbar),
                                 n    = stk@stock.n, 
                                 wt   = stk@stock.wt, 
                                 fec  = predictModel(fec = flqa, model = ~ fec), 
                                 mat  = predictModel(mat = stk@mat, model = ~ mat), 
                                 m    = stk@m,
                                 spwn = stk@m.spwn
  ))
  
  names(biols) <- name(stk)
  
  m(biols[[1]])[,ac(proj.yrs)]   <- m(biols[[1]])[,ac(proj.yr-1)]
  fec(biols[[1]])[,ac(proj.yrs)] <- fec(biols[[1]])[,ac(proj.yr-1)]
  mat(biols[[1]])[,ac(proj.yrs)] <- mat(biols[[1]])[,ac(proj.yr-1)]
  wt(biols[[1]])[,ac(proj.yrs)]  <- wt(biols[[1]])[,ac(proj.yr-1)]
  
  spwn(biols[[1]])[,ac(proj.yrs)]<- spwn(biols[[1]])[,ac(proj.yr-1)]
  
  
  
  # SRs / BDs  ----
  #~~~~~~~~~~~~~~~~~~
  
  SRs <- list(stk = FLSRsim( name = name(stk), desc = "", 
                             ssb=flq, model = sr_model))
  
  ssb.ss <- 2
  rec.ss <- 2
  
  names(SRs) <- name(stk)
  
  # - data
  
  SRs[[1]]@ssb[]            <- ssb(stk)
  SRs[[1]]@ssb[,,,-ssb.ss,] <- 0
  
  SRs[[1]]@rec[]                    <- stock.n(stk)[1,]
  SRs[[1]]@rec[,,,c(1:(rec.ss-1)),] <- 0
  
  # - rectruiment distribution
  
  SRs[[1]]@proportion[,,,-rec.ss,] <- 0
  SRs[[1]]@proportion[,,, rec.ss,] <- 1
  
  # - time lag
  
  SRs[[1]]@timelag['year',]         <- dims(biols[[1]])$min  # =0, because age 0 recuitment
  SRs[[1]]@timelag['season',rec.ss] <- ssb.ss
  
  # - parameters
  
  SRs[[1]]@params[] <- sr_params
  
  
  # - uncertainty
  
  yr <- dimnames(SRs[[1]])$year[1]
  Rest <- FLBEIA:::SRsim( SRs[[1]], year = yr, season = rec.ss, iter = 'all')@rec[,yr,,rec.ss,]
  SRs[[1]]@uncertainty[,yr,,rec.ss,] <- SRs[[1]]@rec[,yr,,rec.ss,] / Rest
  
  # LI: I comment these lines out, because uncertainty is going to be defined in script 03
  #     In this way, we avoid using sigR in this script
  # yr <- ac(hist.yrs[-1],proj.yrs)
  # SRs[[1]]@uncertainty[,yr,,rec.ss,] <- rlnorm( length(yr) * nit, 0, sigR)
  
  
  # fleets  ----
  #~~~~~~~~~~~~~~~
  
  catch.obj <- FLCatchExt( name = name(stk), 
                           alpha = flqa, beta = flqa, landings.n = stk@catch.n,
                           landings = stk@catch, landings.n = stk@catch.n, 
                           landings.wt = stk@landings.wt, 
                           discards = flq0, discards.n = flqa0, 
                           discards.wt = stk@discards.wt, 
                           landings.sel = flqa, discards.sel = flqa0)
  
  landings.wt(catch.obj)[,ac(proj.yrs)]  <- landings.wt(catch.obj)[,ac(proj.yr-1)]
  discards.wt(catch.obj)[,ac(proj.yrs)]  <- discards.wt(catch.obj)[,ac(proj.yr-1)]
  
  catches.obj <- FLCatchesExt(stk = catch.obj)
  names(catches.obj) <- name(stk)
  
  
  ## Age Structured
  fleets <- FLFleetsExt(fl = FLFleetExt(name = "fl", effort= flq, capacity = flq*1e12,
                                        metiers =  FLMetiersExt(mt = FLMetierExt(name = "mt", effshare = flq, 
                                                                                 catches = catches.obj))))
  
  fleets[[1]]@metiers[[1]]@catches[[1]]@catch.q[,ac(hist.yrs),,,,] <- stk@harvest[,ac(hist.yrs)]
  
  fleets[[1]]@metiers[[1]]@catches[[1]]@catch.q[,ac(proj.yrs)] <- yearMeans(fleets[[1]]@metiers[[1]]@catches[[1]]@catch.q[,ac(hist.yrs)]) 
  
  
  # covars  ----
  #~~~~~~~~~~~~~~~
  
  # covars <- NULL
  
  
  # indices  ----
  #~~~~~~~~~~~~~~~~
  
  idxyr <- (proj.yr-10):last.yr
  
  flqidx <- window(trim(flq, season=ssb.ss), start=idxyr[1], end=idxyr[length(idxyr)])
  
  indices <- list( stk = FLIndices(B1plusIdx = FLIndex(name = name(stk), catch.wt = flqidx, 
                                                       effort = flqidx, index = flqidx)))
  
  names(indices) <- name(stk)
  
  indices[[1]]$B1plusIdx@type <- "biomass"
  
  yr <- idxyr[idxyr %in% hist.yrs]
  
  indices[[1]][[1]]@index.q[] <- 1 #*rlnorm(nyr*nit, 0, sqrt(log(cvIDX^2+1)))
  indices[[1]][[1]]@index[,1,] <- indices[[1]][[1]]@index.q[,1,] * 
    quantSums(wt(biols[[1]][-which(dimnames(biols[[1]])$age == "0"),1,,ssb.ss,]) * 
                n(biols[[1]][-which(dimnames(biols[[1]])$age == "0"),1,,ssb.ss,]))
  
  indices[[1]][[1]]@range[c("startf","endf")] <- 0.5 # B1+ in the middle of the year
  
  
  # advice  ----
  #~~~~~~~~~~~~~~~
  
  # ** FLAdvice Object **
  # TAC is given in biomass so the same object is valid for 
  # biomass and age structured stocks.
  advice <- list(TAC = flq1s,  quota.share = list(stk = flq1s))
  
  dimnames(advice$TAC)$quant <- names(advice$quota.share) <- name(stk)
  dimnames(advice$quota.share[[1]])$quant <- names(fleets)
  
  quant(advice$TAC) <- "stock"
  quant(advice$quota.share[[1]]) <- "fleet"
  
  advice$TAC[,ac(hist.yrs)] <- seasonSums(stk@catch)[,ac(hist.yrs)]
  
  
  #==============================================================================
  # FLBEIA control                                                           ----
  #==============================================================================
  
  
  # main.ctrl  ----
  #~~~~~~~~~~~~~~~~
  
  main.ctrl  <- list(sim.years = c(initial = proj.yrs[1], final = proj.yrs[proj.nyr]))
  
  
  # biols.ctrl  ----
  #~~~~~~~~~~~~~~~~~
  
  biols.ctrl <- create.biols.ctrl(stksnames = name(stk), growth.model = "ASPG")
  
  
  # fleets.ctrl  ----
  #~~~~~~~~~~~~~~~~~~
  
  fleets.ctrl <- create.fleets.ctrl(fls = "fl", fls.stksnames = list(fl = name(stk)), flq = flq, 
                                    effort.models = c(fl = "SMFB"), n.fls.stks = c(fl = 1), 
                                    capital.models = c(fl = "fixedCapital"), price.models = c(fl = "fixedPrice"), 
                                    catch.models = c("CobbDouglasAge"))
  
  
  # covars.ctrl  ----
  #~~~~~~~~~~~~~~~~~~
  
  # covars.ctl <- NULL
  
  
  # obs.ctrl  ----
  #~~~~~~~~~~~~~~~
  
  obs.ctrl <- create.obs.ctrl(stksnames = name(stk), n.stks.inds = c(1), stks.indsnames = names(indices[[1]]))
  
  obs.ctrl[[1]][["stkObs"]][["stkObs.model"]] <- "NoObsStock"
  obs.ctrl[[1]][["indObs"]][[1]][["indObs.model"]] <- "bio1plusInd"
  
  
  # assess.ctrl  ----
  #~~~~~~~~~~~~~~~~~~
  
  assess.ctrl <- create.assess.ctrl(stksnames = name(stk), assess.models = "NoAssessment")
  
  
  # advice.ctrl  ----
  #~~~~~~~~~~~~~~~~~~
  
  # advice.ctrl <- create.advice.ctrl(stksnames = name(stk), HCR.models = "annexIVHCR", iter = nit, 
  #                                   first.yr = first.yr, last.yr = last.yr) 
  #! FALTA EL CREATOR DE LA NUEVA REGLA
  
  advice.ctrl <- list()
  
  advice.ctrl[[stkn]] <- list(HCR.model = "IcesCat3HCR", index = names(indices[[1]]))
  
  advice.ctrl[[1]][['refnyrs']] <- c(now=2, past=3)
  
  advice.ctrl[[1]]$unc.cap     <- NULL
  advice.ctrl[[1]]$prec.buffer <- NULL
  
  advice.ctrl[[1]][['ref.pts']] <- matrix( ref.pts, nrow = length(ref.pts), ncol = nit, 
                                           dimnames = list(names(ref.pts),1:nit))
  
  # advice.ctrl[[1]]$AdvCatch[] <- TRUE
  
  
  
  #==============================================================================
  # Save FLBEIA inputs                                                       ----
  #==============================================================================
  
  save( biols, SRs, fleets, indices, advice, 
        main.ctrl, biols.ctrl, fleets.ctrl, obs.ctrl, assess.ctrl, advice.ctrl, 
        fprop_s1, 
        file = file.path("./input",paste(stkn,lhsc,"_data.RData",sep="")))
  
  rm(list = ls()[!ls() %in% c('stkn', 'stkn.opt', 'lhsc', 'lhsc.opt')])
  
}


