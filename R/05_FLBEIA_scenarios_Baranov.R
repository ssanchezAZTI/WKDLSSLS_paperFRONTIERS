################################################################################
#  WKDLSSLS - FLBEIA conditioning and MSE simulations for scenarios            # 
#------------------------------------------------------------------------------#
#   Sonia Sanchez (AZTI-Tecnalia)                                              #
#   created:  16/04/2019                                                       #
#   modified:                                                                  #
################################################################################

# 03_FLBEIA_scenarios.R - conditioning FLBEIA scenarios and MSE simulations
# ~/WKDLSSLS_2019/R/03_FLBEIA_scenarios.R

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

# directory with results

res.dir <- "./output"


#==============================================================================
# LOAD LIBRARIES AND FUNCTIONS                                             ----
#==============================================================================

# load libraries
library(FLBEIA, lib.loc="./Rlibs/")
# library(FLBRP)  # FLBRP: function to estimate reference points
library(dplyr)
n <- FLCore::n


#==============================================================================

#==============================================================================

t1 <- Sys.time()

# get the job id from the environment
it <- as.numeric(Sys.getenv("SGE_TASK_ID"))
cat("Starting run i = ",it,"\n")

# Set seed for reproducible results:
set.seed(it)

#==============================================================================
# SCENARIOS                                                                ----
#==============================================================================

pars <- read.table( file.path("./input", "list_scenarios.csv"), sep = ",", header = TRUE)

scenario <- ac(pars$SCENARIO[it])
sc.pars <- subset(pars, SCENARIO == scenario)

om <- ac(sc.pars$OM)
mp <- ac(sc.pars$MP)

stkn <- ac(sc.pars$STKN)


cat("- scenario : ",scenario,"\n")
sc.pars
cat("- OM       : ",om,"\n")
cat("- MP       : ",mp,"\n")
cat("- stock    : ",stkn,"\n")


# ADVICE
# - iny : in-year advice (at the middle of the year)
# - fpa : full population advice
# - int : interim year advice
# - fix : fixed TAC (natural year)

advt.opt <- c( "iny", "fpa", "int", "fix")

advt <- ac(sc.pars$ADVT)

if (!advt %in% advt.opt)
  stop(paste("Check 'ADVT' value for MP ", mp, " in './input/list_scenarios.csv' file."))

if (advt == "fix" & ac(sc.pars$HCRT) != "ft0")
  stop(paste("Check 'ADVT' value for MP ", mp, " in './input/list_scenarios.csv' file. \n 
             'ADVT = fix' only valid if HCRT = ft0'"))


# HCR
# - ft0 : fixed TAC to 0
# - 1o2 : 1 over 2
# - 1o3 : 1 over 3
# - 2o3 : 2 over 3
# - 1o5 : 1 over 5

hcrt.opt <- c( "1o2", "1o3", "2o3", "1o5", "ft0")

hcrt <- ac(sc.pars$HCRT)

if (!hcrt %in% hcrt.opt)
  stop(paste("Check 'HCRT' value for MP ", mp, " in './input/list_scenarios.csv' file."))

# BUFFER
# - 0.20 : with 20% buffer the 1st simulation year
# - 0.00 : without any buffer

pbuf <- sc.pars$PBUF

if(pbuf < 0 | pbuf > 1) 
    stop(paste("Check 'PBUF' value for MP ", mp, " in './input/list_scenarios.csv' file."))


# UNCERTAINTY CAP (lower and upper)
# - 0.00 : no uncertainty cap
# - 0.20 : 20% uncertainty cap
# - 0.50 : 50% uncertainty cap
# - 0.80 : 80% uncertainty cap

ucap.low <- sc.pars$UCPL
ucap.upp <- sc.pars$UCPU


# HCR initialization
# - fix : for fixed TAC, no initialization required
# - pyc : no information on current exploitation --> previous year catch
# - nin : no information on current exploitation --> avg of 2/3 last years' catches
# - pob : perfect observation on current exploitation --> Fmsy/avgF(last5yr)*avgC(last5yr)

hcri.opt <- c( "pyc", "nin", "pob", "fix")

hcri <- ac(sc.pars$HCRI)

if (!hcri %in% hcri.opt)
  stop(paste("Check 'HCRI' value for MP ", mp, " in './input/list_scenarios.csv' file."))

if (hcri == "fix" & hcrt != "ft0")
  stop(paste("Check 'HCRI' value for MP ", mp, " in './input/list_scenarios.csv' file. \n 
              'HCRI = fix' only valid if HCRT = 'ft0'"))

# Biomass safeguard:

# none
# Imin  : minimum value
# Iminpa: 1.4 * Imin
# Inorm : 

bsafe.opt <- c("none","Imin","Iminpa","Inorm") 

bsafe <- ac(sc.pars$BSAFE)

if (! bsafe %in% bsafe.opt)
  stop(paste("Check 'BSAFE' value for MP ", mp, " in './input/list_scenarios.csv' file."))


#==============================================================================
# LOAD DATA                                                                ----
#==============================================================================

#load( file.path("./input/iters", paste("data_",om,"_2it.RData",sep=""))) # for tests with 2 iterations
load( file.path("./input/iters", paste("data_",om,".RData",sep="")))

# biols; SRs; fleets; indices; advice; 
# main.ctrl; biols.ctrl; fleets.ctrl; obs.ctrl; assess.ctrl; advice.ctrl;
# IAV


#==============================================================================
# FLBEIA input objects (for selected scenario)                             ----
#==============================================================================

ages     <- dimnames(biols[[1]]@n)$age
yrs      <- dimnames(biols[[1]]@n)$year
proj.yrs <- ac(main.ctrl$sim.years[1]:main.ctrl$sim.years[2])
hist.yrs <- yrs[!yrs %in% proj.yrs]

na <- length(ages)
ny <- length(yrs)

nit  <- dims(biols[[1]])$iter # number of iterations


cat("\n \n")
cat("#######################################################\n")
cat("####  SCENARIO: ", mp, " ####\n")
cat("#######################################################\n")


  # For setting TACs also in the 1st proj.yr
  init.yr <- main.ctrl$sim.years[["initial"]] - 1
  main.ctrl$sim.years["initial"] <- init.yr
  
  # Remove historical historical TACs (as has been set equal to historical catches)
  # (required TACs for initial year will be set as a function of the calendar)
  advice$TAC[,hist.yrs,] <- NA
  
  
  fleets.ctrl0 <- fleets.ctrl
  indices0     <- indices
  obs.ctrl0    <- obs.ctrl
  advice0      <- advice
  advice.ctrl0 <- advice.ctrl
  
# ADVICE
  
  if (advt == "int") {         # - int : interim year advice
    
    # Required 1st TAC
    advice0$TAC[,init.yr,] <- seasonSums(quantSums(catchWStock(fleets, stock = stkn)))[,init.yr,]
    
    obs.ctrl0[[1]]$obs.curryr <- FALSE
    
  } else if (advt == "fpa") {  # - fpa : full population advice
    
    # Required 1st TAC
    advice0$TAC[,init.yr,] <- seasonSums(quantSums(catchWStock(fleets, stock = stkn)))[,init.yr,]
    
    # observed indices at the beggining of TAC year
    
    indices0[[1]][[1]] <- # lag one year back
      window(indices[[1]][[1]], start=indices[[1]][[1]]@range[["minyear"]]-1, end=indices[[1]][[1]]@range[["maxyear"]]-1)
    
    indices0[[1]][[1]]@range[c("startf","endf")] <- 0 # B1+ in 1st January
    
    idxyr <- dimnames(indices0[[1]][[1]])$year
    yr    <- idxyr[idxyr %in% hist.yrs[-length(hist.yrs)]]
    yr1   <- as.character(as.numeric(yr)+1)
    idxss <- 1
    
    a1plus <- which(dimnames(biols[[1]]@n)$age=='1'):dim(biols[[1]]@n)[1]
    
    indices0[[1]][[1]]@index.q[,yr,] <- indices[[1]][[1]]@index.q[,yr1,]
    
    indices0[[1]][[1]]@index[,yr,] <- indices0[[1]][[1]]@index.q[,yr,] * 
      quantSums( n(biols[[1]])[a1plus,yr1,,idxss,] * wt(biols[[1]])[a1plus,yr1,,idxss,]) # index at the beginning of the following year
    indices0[[1]][[1]]@index[,hist.yrs[length(hist.yrs)],] <- NA
    
    # controls
    
    obs.ctrl0[[1]]$obs.curryr  <- TRUE
    obs.ctrl0[[1]]$indObs[[1]]$indObs.model <- "bio1plusfwdInd"
    
    advice.ctrl0[[1]]$inyr.idx <- TRUE
    
  } else if (advt == "iny") {  # - iny : in-year advice at the middle of the year
    
    # Required 1st TAC
    c2s.yr1 <- quantSums(catchWStock(fleets, stock = stkn))[,init.yr-1,,2,]
    C1s.yr2 <- seasonMeans(quantSums(catchWStock(fleets, stock = stkn)))[,init.yr,,,]
    advice0$TAC[,init.yr-1,] <- c2s.yr1 + C1s.yr2
    
    fleets.ctrl0$seasonal.share[[1]][,init.yr-1,,2,] <- c2s.yr1 / advice0$TAC[,init.yr-1,]
    fleets.ctrl0$seasonal.share[[1]][,init.yr,,1,]   <- C1s.yr2 / advice0$TAC[,init.yr-1,]
    
    # TAC calendar (July y - June y+1)
    
    advice.ctrl0[[1]]$adv.season <- 1
    
    obs.ctrl0[[1]]$obs.curryr  <- TRUE
    obs.ctrl0[[1]]$indObs[[1]]$indObs.model <- "bio1plusfwdInd"
    
    advice.ctrl0[[1]]$inyr.idx <- TRUE
    
    
    # assumed 50% catches in each semester (default value)
    
  } else if (advt == "fix") {
    
    # Required 1st TAC
    advice0$TAC[,init.yr,] <- seasonSums(quantSums(catchWStock(fleets, stock = stkn)))[,init.yr,]
    
    # No indices
    
    indices0 <- NULL
    
    # controls
    
    obs.ctrl0[[1]]$indObs[[1]]$indObs.model <- "NoOsbIndex"
    
    advice.ctrl0[[1]]$HCR.model <- "fixedAdvice"
    advice.ctrl0[[1]]$index <- advice.ctrl0[[1]]$refnyrs <- NULL
    #    advice.ctrl0[[1]] <- list(HCR.model = "fixedAdvice") # we need the ref pts. cannot restart the ctrl object
  
  }
  

# HCR
  
  # - reference years
  
  if (hcrt=="ft0") {
    advice0$TAC[1,proj.yrs,] <- 0
  } else if (hcrt=="1o2") {
    advice.ctrl0[[1]][['refnyrs']] <- c(now=1, past=2)
  } else if (hcrt=="1o3") {
    advice.ctrl0[[1]][['refnyrs']] <- c(now=1, past=3)
  } else if (hcrt=="2o3") {
    advice.ctrl0[[1]][['refnyrs']] <- c(now=2, past=3)
  } else if (hcrt=="1o5") {
    advice.ctrl0[[1]][['refnyrs']] <- c(now=1, past=5)
  } else
    stop("Check values in 'hcrt'")
  
  # - precautionay buffer
  
  advice.ctrl0[[1]]$prec.buffer <- list( value = pbuf, 
                                         year = as.character(main.ctrl$sim.years[["initial"]]))
  # - uncertainty caps
  
  if (ucap.low > 0) advice.ctrl0[[1]]$unccap.low <- ucap.low
  if (ucap.upp > 0) advice.ctrl0[[1]]$unccap.upp <- ucap.upp

  # - bsafe options 
  
  if (bsafe != "none") { 
    advice.ctrl0[[1]]$HCR.model <- "IcesCat3HCR_bsafe_hrcap"
  }
  
  if(bsafe=="Imin"){
    advice.ctrl0[[1]]$Itrigger <- apply(indices0[[1]]$B1plusIdx@index, c(1,3:6), min, na.rm=TRUE)[drop=TRUE]
  } else if (bsafe=="Iminpa"){
    advice.ctrl0[[1]]$Itrigger <- 1.4 * apply(indices0[[1]]$B1plusIdx@index, c(1,3:6), min, na.rm=TRUE)[drop=TRUE]
  } else if (bsafe=="Inorm"){
    mulog <- yearMeans(log(indices0[[1]]$B1plusIdx@index))[drop=TRUE]
    siglog <- sqrt(yearVars(log(indices0[[1]]$B1plusIdx@index))[drop=TRUE])
    # mulog <- apply(log(indices0[[1]]$B1plusIdx@index), c(1,3:6), mean, na.rm=TRUE)[drop=TRUE]
    # siglog <- apply(log(indices0[[1]]$B1plusIdx@index), c(1,3:6), sd, na.rm=TRUE)[drop=TRUE]
    advice.ctrl0[[1]]$Itrigger <- exp(mulog-1.645*siglog) 
  }else if (bsafe=="none"){
    advice.ctrl0[[1]]$Itrigger <- NULL
  }else{
    stop("Check values in 'bsafe'")
  }
  
  # - initialization
  
  if (hcri != "fix") {
    
    advice.ctrl0[[1]]$cref.year <- main.ctrl$sim.years["initial"]
    lyr <- main.ctrl$sim.years["initial"] - 1
    
  }
  
  if (hcrt != "ft0") { # for all n-over-m
    if (hcri == "pyc") {         # - pyc : no information on current exploitation --> previous year catch
      
      advice.ctrl0[[1]]$cref.value <- quantSums(seasonSums(catchWStock(fleets, stock = stkn)))[,lyr,]
      
    } else if (hcri == "nin") {  # - nin : no information on current exploitation --> avg of 2/3 last years' catches
      
      nyr <- advice.ctrl0[[1]][['refnyrs']]["past"]
      
      advice.ctrl0[[1]]$cref.value <- yearMeans(quantSums(seasonSums(catchWStock(fleets, stock = stkn)))[,lyr - c(0:(nyr-1)),])
      
    } else if (hcri == "pob") {  # - pob : perfect observation on current exploitation --> Fmsy/avgF(last5yr)*avgC(last5yr)
      
      nyr <- advice.ctrl0[[1]][['refnyrs']]["past"]
      
      Fmsy <- advice.ctrl0[[1]][['ref.pts']]["Fmsy",]

      #! does it work with iterations now???
      
      Flast <- yearMeans(fyr[1,lyr - c(0:(nyr-1)),])
      
      advice.ctrl0[[1]]$cref.value <- Flast/Fmsy * 
        yearMeans(quantSums(seasonSums(catchWStock(fleets, stock = stkn)))[,lyr - c(0:(nyr-1)),])
      
    }
  }
  
  
#==============================================================================
# Baranov catch equation                                                   ----
#==============================================================================

biols.ctrl[[1]]$growth.model <- "ASPG_Baranov"
  
fleets.ctrl0$fl[[names(biols)]]$catch.model <- "Baranov"
biols[[1]]@m[1,,,1,] <- 1e-08 # replace 0 to a very small value to avoid NaN when estimating F at C=0
  
hl5.yr <- hist.yrs[(length(hist.yrs)-5+1):length(hist.yrs)]
  
fleets <- calculate.q.sel.flrObjs( biols = biols, fleets = fleets, fleets.ctrl = fleets.ctrl0, 
                                   mean.yrs = hl5.yr, sim.yrs = proj.yrs)

fleets[[1]]@metiers[[1]]@catches[[1]]@catch.q[,proj.yrs,] <- 
  yearMeans(fleets[[1]]@metiers[[1]]@catches[[1]]@catch.q[,hl5.yr,])


    
#==============================================================================
# FLBEIA run                                                               ----
#==============================================================================

  # stk <- biolfleets2flstock(biols[[1]], fleets)
  # stk@harvest
  # fbar(stk)
  

source(file.path("R/fun","MP_3d_HCR_ICES_Cat3.R"))
source(file.path("R/fun","MP_3d_HCR_ICES_Cat3_bsafe_hrcap.R"))

# checkFLBEIAData(biols, SRs, BDs = NULL, fleets, covars = NULL,
#                 indices = indices0, advice = advice0, 
#                 main.ctrl, biols.ctrl, fleets.ctrl0,
#                 covars.ctrl = NULL, obs.ctrl0, assess.ctrl, advice.ctrl0)

proj.obj <- FLBEIA(biols, SRs, BDs = NULL, fleets, covars = NULL,
                   indices = indices0, advice = advice0, 
                   main.ctrl, biols.ctrl, fleets.ctrl0,
                   covars.ctrl = NULL, obs.ctrl0, assess.ctrl, advice.ctrl0)

assign(scenario, proj.obj)


#==============================================================================
#  Indicators
#==============================================================================

# summaries in wide format
assign( paste(scenario,"biosem",sep="_"), bioSum(proj.obj, long=F, scenario=scenario, byyear=F)) # byyear=F to keep info by semesters
assign( paste(scenario,"bio",sep="_"), bioSum(proj.obj, long=F, scenario=scenario, byyear=T, ssb_season = 2))
assign( paste(scenario,"adv",sep="_"), advSum(proj.obj, long=F, scenario=scenario))

# summary quantiles in wide format
assign( paste(scenario,"bioQ",sep="_"), bioSumQ( get(paste(scenario,"bio",sep="_")) ))
assign( paste(scenario,"advQ",sep="_"), advSumQ( get(paste(scenario,"adv",sep="_")) ))

# Other indicators not needed in this case study
# For risk calculation, use reference points in advice control object
# assign( paste(scenario,"flt",sep="_"), fltSum(proj.obj, long=F, scenario=scenario))
# assign( paste(scenario,"fltstk",sep="_"), fltStkSum(proj.obj, long=F, scenario=scenario))
# assign( paste(scenario,"risk",sep="_"), riskSum(proj.obj, Bpa=c(stk=advice.ctrl0[[1]]$ref.pts["Bpa",]), 
#                                                 Blim=c(stk=advice.ctrl0[[1]]$ref.pts["Blim",]), 
#                                                 Prflim=c(fl=NA), scenario=scenario))


#==============================================================================
#  Other things to save
#==============================================================================

# scenario parameters

assign( paste(scenario,"pars",sep="_"), sc.pars) 

# index values

if (advt == "fix"){
  assign( paste(scenario,"idx",sep="_"), NULL)
}else{
  assign( paste(scenario,"idx",sep="_"), as.data.frame(index(proj.obj$indices[[1]][[1]]))) 
}

# reference points
# it is a matrix with dims 4 x 1000. Rows are B0, R0, Blim and Fmsy 

assign( paste(scenario,"refpts",sep="_"), advice.ctrl0[[1]][['ref.pts']]) 


#==============================================================================
#  Save outputs
#==============================================================================

# we don't save the output object from FLBEIA to save some space on disk.

out.obj <- paste( scenario, c("biosem","bio","adv","bioQ","advQ","pars","idx","refpts"), sep="_")
#out.obj <- c(scenario, paste( scenario, c("pars","idx","refpts"), sep="_"))
out.name <- paste("out_",scenario,".RData",sep="")

if(!dir.exists(file.path(res.dir,"output_scenarios"))){
  dir.create(file.path(res.dir,"output_scenarios"))
}
out.file <- file.path(res.dir,"output_scenarios",out.name)

cat("Saving objects in: ",out.file,"\n")
save(list=out.obj, file=out.file)

# Saving the names of the scenarios runned
sc.file <- file.path(res.dir,"scenario_list.RData")

  if (!file.exists(sc.file)) {
    scenario_list <- scenario
    save(scenario_list, file=sc.file)
  } else {
    load(sc.file)
    scenario_list <- unique(c(scenario_list, scenario))
    save(scenario_list, file=sc.file)
  }


t2 <- Sys.time()

t2 - t1

