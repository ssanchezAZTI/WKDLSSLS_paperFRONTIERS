################################################################################
#  WKDLSSLS (conditioning an OM based on life history traits)                  # 
#            Pop type 2: sprat / sardine (high productivity)                   #
#------------------------------------------------------------------------------#
#   Sonia Sanchez (AZTI-Tecnalia)                                              #
#   created:  16/04/2019                                                       #
#   modified: 2019-07-01 11:27:03 adapted for specific stock                   #
################################################################################

# 01_lifeHistoryTraits_STK1.R - conditioning an OM based on life history traits
# ~/WKDLSSLS_2019/R/01_lifeHistoryTraits_STK1.R

# Copyright: AZTI, 2019
# Author: Sonia Sanchez (AZTI) (<ssanchez@azti.es>)
#         Based on http://www.flr-project.org/doc/Data_Poor_MSE_in_FLBEIA.html
#
# Distributed under the terms of the GNU GPLv3


#==============================================================================
#                                                                          ----
#==============================================================================

rm(list=ls())

nit <- 1000


#==============================================================================
# WORKING DIRECTORY                                                        ----
#==============================================================================

# wd <- "C:/use/GitHub/ssanchezAZTI/WKDLSSLS_2019" # main directory
# setwd(wd)

# directory with results
inp.dir  <- file.path("./input")
# directory with results
res.dir  <- file.path("./output")
# directory with plots
plot.dir <- file.path("./plots")


#==============================================================================
# LOAD LIBRARIES AND FUNCTIONS                                             ----
#==============================================================================

# load libraries
library(FLBEIA)
library(fishmethods) # for M.empirical function
library(gtools)      # for rdirichlet function
library(FLife)       # teleost data
library(ggplotFL)
library(FLBRP)
library(dplyr)       # for manipulating data.frames


#==============================================================================
# SCENARIO                                                                 ----
#==============================================================================

# Populations type 2: sprat + sardine

stkn <- "STK2"

# Characteristics:

Mage <- "gl1" # Gislason (with correction for age 0)
matm <- "lg1" # Logistic (0.5 at age 1 and full at age 2)
selm <- "lg1" # Logistic (0.5 at age 1 and full at age 2)
recm <- "bhm" # Beverton-Holt + medium steepness (0.75)
# sigR <- "075" # SD residuals around SR
ares <- "not" # no autocorrelation in residuals
# idxt <- "b1p" # b1+ index

sc <- "highprod"
# sc <- paste("RECM", recm, "_MAGE", Mage, "_SELM", selm, 
#             "_MATM", matm, "_IDXT", idxt, sep="")


#==============================================================================
# LIFE HISTORY PARAMETERS                                                  ----
#==============================================================================

# PARAMETERS

ages <- 0:6                 # general ranges adopted for all cases
fbar.ages <- c(min=1,max=3)


# SR             : s (s); v (virgin biomass); spr0 (spawners per recruit at F=0)

s  <- 0.9
B0 <- 100000

spwn <- 0.5 # i.e. spawning time: 1st July


# Von Bertalanffy: linf; k; t0

# Source: from AZTI database.
# (fitting to mean size at age in annual sardine catches from 8.abd in the Basque Country - 2002 to 2018)

linf <- 22.8327418823817; lmax <- NULL
k    <- 0.562725139983291
t0   <- -0.797539090141085

# library(FLife)
# data("teleost")
# teleost[,"Sardina pilchardus"]
# # An object of class "FLPar"
# # params
# #     linf        k       t0      l50        a        b
# # 20.20000  0.44967 -0.82867 15.27000  0.00579  3.05977 
# # units:  NA 


# Length-weight  : a; b

# Source: parameters in "teleost" object in the R library FLife ()

lw_a <- 0.005793333
lw_b <- 3.059766667


# Maturity       : a50

a50 <- l50 <- NULL

# - logistic(a50, ato95, asym)
a50_mat   <- 1  # age at which 50% of individuals are mature
asym_mat  <- 1  # fixed maturity for older ages
ato95_mat <- 1  # lag between a50 and first age with full maturity
# a1_mat    <- 1  # age at full maturity  (required if Knife-edge maturity selected)


# Selectivity at age

# equal to maturity

# # - dnormal(a1, sl, sr)
# a1_sel <- 4     # age for full selectiviy
# sl_sel <- 2     # age lag between a50 and age at full selectivity
# sr_sel <- 50000 # shaping parameter


# Natural mortality

# tmax <- ages[length(ages)] # only required for Hoening (1983)


#==============================================================================
# INDIVIDUAL GROWTH                                                        ----
#==============================================================================

# Von-Bertalanffy growth
# linf; k; t0

if (is.null(linf)) {
  if (!is.null(lmax)) {
    linf <- 0.95 * lmax
  } else 
    stop("A value for linf or lmax is required.")
}
 
if (is.null(k))  # Gislason, Pope et al. (2008)
  k <- 3.15 * linf^(-0.64)

if (is.null(t0))
  t0 <- - exp(- 0.3922 - 0.2752 * log(linf) - 1.038 * log(k))


# Von Bertalanffy model
vonB <- function(age, linf, k, t0) return(linf*(1-exp(-k*(age-t0))))
invVonB <- function (L, linf, k, t0)  return(t0 - 1/k * log(1-L/linf))

# Length-Weight relationship
# lw_a; lw_b

# Mean weight at age: from Von Bertalanffy + length-weight relationship

# mid-year
ages_yr <- ages+c(0.75,rep(0.5,length(ages)-1))
mla <- vonB(ages_yr, linf, k, t0)
mwa <- lw_a*mla^lw_b

# - stock
#   1st semester  (weights at age at the begging of the year)
ages_stks1 <- ages
mla_stks1 <- vonB(ages_stks1, linf, k, t0)
mwa_stks1 <- lw_a*mla_stks1^lw_b
#   2nd semester (weights at age at spawning time)
ages_stks2 <- ages + spwn
mla_stks2 <- vonB(ages_stks2, linf, k, t0)
mwa_stks2 <- lw_a*mla_stks2^lw_b


# - catch
#   1st semester (weights at age in the middle of the season)
ages_cats1 <- ages + 0.25
mla_cats1 <- vonB(ages_cats1, linf, k, t0)
mwa_cats1 <- lw_a*mla_cats1^lw_b
#   2nd semester (weights at age in the middle of the season)
ages_cats2 <- ages + 0.75
mla_cats2 <- vonB(ages_cats2, linf, k, t0)
mwa_cats2 <- lw_a*mla_cats2^lw_b


# # CHECK:
mla
# 13.27495 16.56565 19.26266 20.79903 21.67423 22.17279 22.45680
mla_stks1
#  8.256393 14.529258 18.102625 20.138209 21.297789 21.958349 22.334640
mla_stks2
# 11.83118 16.56565 19.26266 20.79903 21.67423 22.17279 22.45680
mla_cats1
# 10.16930 15.61896 18.72338 20.49182 21.49923 22.07310 22.40001
mla_cats2
# 13.27495 17.38810 19.73118 21.06592 21.82627 22.25940 22.50613
# 
mwa
# 15.81785 31.14741 49.41522 62.49321 70.89317 76.00195 79.02009
mwa_stks1
#  3.699075 20.850778 40.862427 56.614543 67.192713 73.775215 77.712224
mwa_stks2
# 11.12099 31.14741 49.41522 62.49321 70.89317 76.00195 79.02009
mwa_cats1
#  6.998493 26.015079 45.303019 59.711651 69.156258 74.961221 78.410249
mwa_cats2
# 15.81785 36.12525 53.18566 64.97944 72.42577 76.91395 79.55248



#==============================================================================
# MATURITY                                                                 ----
#==============================================================================

# # L50: length at which 50% of individuals are mature
# if (is.null(a50)) {
#   if (is.null(l50)) l50 <- 0.72 * linf^0.93
#   a50 <- invVonB(l50, linf, k, t0)
# }
  
# Logistic maturity ogive (from FLife - logistic function)
# asym; ato95

FLife:::logisticFn

mata <- logistic( age=FLQuant(ages,dimnames=list(age=ages)),
                  params=FLPar(a50=a50_mat, ato95=ato95_mat, asym=asym_mat))[drop=TRUE]

# # CHECK
# mata
# #         0         1         2         3         4         5         6 
# # 0.0500000 0.5000000 0.9500000 0.9972376 0.9998542 0.9999923 0.9999996

#! Fixed values a priori as problems with SSB when > 0 for age 0
mata[] <- c(0, 0.5, rep(1,length(ages)-2))


#==============================================================================
# NATURAL MORTALITY                                                        ----
#==============================================================================

# Use the function M.empirical in fishmethods package to calculate natural 
# mortality based on the life history parameters defined above.

# fishmethods::M.empirical


# # Constant M based on tmax: Hoening (1983) 
# 
# M <- M.empirical(Linf = linf, tmax = tmax, method = 3)[2] # tmax is required


# variable M@age based on Gislason et al. (2010)

Mage <- mla * NA
for (i in 1:length(Mage))
  Mage[i] <- M.empirical(Linf = linf, Kl = k, Bl=mla[i], method = 9)


# # CHECK
# Mage
# # 1.372 0.961 0.754 0.666 0.623 0.601 0.589

Mage <- Mage * 0.72
# # CHECK
# Mage
# # 0.98784 0.69192 0.54288 0.47952 0.44856 0.43272 0.42408


#==============================================================================
# PRODUCTIVITY: Stock recruitment relationship                             ----
#==============================================================================

# s; v; spr0
# steepness     : proportion of  recruits produced by 20% of the virgin spawning stock.
#                 High steepness --> resilient population.
# virgin biomass: e.g 20 times maximum observed catches
# spawning per recruit

# Function to estimate biomass at age given:
# - ages: ages values (numeric vector)
# - Mage: maturity at age (numeric vector)
# - R0  : initial recruits in mass (numeric)

nage <- function(ages, Mage, R) {
  
  N <- numeric(length(ages))
  
  # - age 0
  N[1] <- R
  
  # - intermediate ages
  for (a in 2:length(ages))
    N[a] <- N[a-1] * exp(-Mage[a-1])
  
  # - plusgroup
  N[length(ages)] <- sum(N[length(ages)] * 
                           exp(-(0:(100-ages[length(ages)]))*Mage[length(ages)]))
  
  return(N)
  
}

# Estimate R0 as a function of B0
# (as R0/B is constant)

if (spwn == 0.5) mwa_spwn <- mwa_stks2/1000 # spawning at the beginnig of the 2nd semester

R0ini  <- 1000 # should be at spawning time (i.e. 1st July, as spwn=0.5) 
               # --> Options:
               #      - a = sr_params["a"] * exp(-Mage[1]/2) to move to spawning time; or 
               #      - Mage[1] = Mage[1]/2 for generating Nini
Mage[1] <- Mage[1]/2
Nini   <- nage(ages, Mage = Mage, R = R0ini) # numbers
SSBini <- sum(Nini * exp(-spwn*Mage) * mata * mwa_spwn) # ssb in kg
spr0   <- SSBini/R0ini # ratio R0/B is constant
R0     <- B0 * 1/spr0


# Use 'abPars' function in FLCore to obtain the parameters of the 
# classical parameterization of beverton & holt SR model.

sr_model  <- "bevholt"

# using FLCore
# Relationship: a = (v + (v - s * v)/(5 * s - 1))/spr0
#               b = (v - s * v)/(5 * s - 1) * spr0/spr0

sr_params <- unlist(abPars(s = s, v = B0, spr0 = spr0, model = sr_model))
srm <- FLSR(name = stkn, params = FLPar(unlist(sr_params)), model = sr_model)


# sr_params["a"] <- sr_params["a"] * exp(-Mage[1]/2) # for moving recruitment to spawning time

# # CHECK
# 
# # estimated by hand
# 
# alpha <- (4 * s * R0)/(5 * s - 1)
# beta  <- B0 * (1 - s)/(5 * s - 1) 
# 
# sr_params1 <- c(a = alpha, b = beta)
# srm1 <- FLSR(name = stkn, params = FLPar(sr_params1), model = sr_model)
# 
# 
# # --> both are equal
# sr_params
# #           a           b
# # 2240883.962    2857.143 
# sr_params1
# #           a           b
# # 2240883.962    2857.143 

# ssb <- seq(0,1e+05,1000)
# rec <- sr_params[["a"]] * ssb / (sr_params[["b"]] + ssb)
# plot( ssb, rec)
# abline(h=R0, col="red")


#==============================================================================
# Blim                                                      ----
#==============================================================================

# In WKLIFEXX Blim was defined as the biomass that according to the SR model 
# leads to a recruitment equal to 0.7R0

# Based on a Beverton-Holt SR model parameterized as a function of s (steepness), B0 and spr0
# the resulting biomass is a function of the steepness parameter and B0.

# Blim <- 0.7*(1-s)/(4*s-0.7*(5*s-1)) * B0

# Finally, we adopt Blim=20%B0, because otherwise there are inconsistencies between F totally exploited (F40B0) and Blim

Blim <- 0.2 * B0

#==============================================================================
# SELECTIVITY                                                              ----
#==============================================================================

# selectivity equal to maturity
sela <- mata

# # double normal selectivity ogive (from FLife - dnormal function)
# 
# FLife:::dnormalFn
# 
# sela <- dnormal( age=FLQuant(ages,dimnames=list(age=ages)), 
#                  params=FLPar(a1=a1_sel, sl=sl_sel, sr=sr_sel))[drop=TRUE]
# 
# #! ALTERNATIVES FOR SEL
# # (1) Knife-Edge
# sela1 <- knife( age=FLQuant(ages,dimnames=list(age=ages)), 
#                 params=FLPar(a1=round(a50,0)))[drop=TRUE] #! to be decided a1 value
# # (2) Logistic (same as maturity pattern)
# sela2 <- logistic( age=FLQuant(ages,dimnames=list(age=ages)), 
#                    params=FLPar(a50=a50, ato95=ato95_mat, asym=asym_mat))[drop=TRUE]
# # (3) Slower than maturity
# #! how???


#==============================================================================
# PRISTINE BIOMASS                                                         ----
#==============================================================================

# Calculate the pristine biomass using the SRR and the natural mortality.

pristineN <- nage(ages, Mage = Mage, R = R0) # R = sr_params["a"] aproximation


# CHECK
B0                               # virgin biomass
# 1e+05
sum(pristineN * exp(-spwn*Mage) * mwa_spwn * mata) # pristine SSB in kg
# 1e+05


#==============================================================================
# STARTING POPULATION                                                      ----
#==============================================================================

init.nyr <- 30

# with 2 seasons
stk <- FLStock( name = stkn, 
                stock.wt = FLQuant(mwa/1000, dim = c(length(ages),init.nyr,1,2,1,1), 
                                   dimnames = list(age=ages,year=1:init.nyr)))

# nit iterations
stk <- propagate( stk, nit)

units(harvest(stk)) <- "f"
units(stock.wt(stk)) <- "kg"

# Mean weight at age by semester
stock.wt(stk)[,,,1,] <- mwa_stks1/1000
stock.wt(stk)[,,,2,] <- mwa_stks2/1000

# F range to estimate mean F
range(stk)[c("minfbar","maxfbar")] <- fbar.ages

# Prop of spawn before M and F
ssb.ss <- rec.ss <- 2
# (assumed spawning in the middle of the year)
if (spwn <= 0.5) {
  harvest.spwn(stk)[,,,1,] <- m.spwn(stk)[,,,1,] <- spwn/0.5
  harvest.spwn(stk)[,,,2,] <- m.spwn(stk)[,,,2,] <- 0
}

# Selectivity
harvest(stk) <- sela
harvest(stk)[,1,] <- 0  # no catches in pristine status

# Natural mortality and maturity
m(stk)     <- Mage/2
m(stk)[1,,,1,] <- 0    # different for age 0, as rec occurs mid-year (not 1st Jan)
m(stk)[1,,,2,] <- Mage[1]
mat(stk) <- mata

# Initial numbers: virgin biomass
stock.n(stk)[ ,1,,1,] <- pristineN
stock.n(stk)[ ,1,,2,] <- stock.n(stk)[,1,,1,] * exp(-m(stk)[,1,,1,])
stock.n(stk)[1,1,,1,] <- 0 # recruitment occurs in the 2nd semester
units(stock.n(stk)) <- "1"
stock(stk) <- quantSums(stock.n(stk) * stock.wt(stk))

# Catches: 0 in the 1st simulation year
catch.wt(stk)[,,,1,] <- landings.wt(stk)[,,,1,] <- discards.wt(stk)[,,,1,] <- mwa_cats1/1000
catch.wt(stk)[,,,2,] <- landings.wt(stk)[,,,2,] <- discards.wt(stk)[,,,2,] <- mwa_cats2/1000
# catch.n(stk)  <- landings.n(stk) <- discards.n(stk) <- 0
# catch(stk)    <- quantSums(catch.n(stk) * catch.wt(stk))
# landings(stk) <- quantSums(landings.n(stk) * landings.wt(stk))
# discards(stk) <- quantSums(discards.n(stk) * discards.wt(stk))

#==============================================================================
# REFERENCE POINTS - final                                                 ----
#==============================================================================

# Estimate refernce points as a function of the percentage of F in each semester

source(file.path("./R/01b_brps.R"))


# BRPS: with 50% catches in each semester

brps <- read.csv(brps.file, sep=";")

# unique(brps$refpt)
brps$refpt <- factor( brps$refpt,
                      levels = c("virgin", "msy", "f0.1", "spr.20", "spr.30", "spr.35", "spr.40", "spr.50",
                                 "F.20B0", "F.30B0", "F.35B0", "F.40B0", "F.50B0", "F.50R0", "F.90msy"))

brps <- brps %>%
  group_by(refpt) %>%
  filter(abs(pY_s1 - 0.5) == min(abs(pY_s1 - 0.5), na.rm = TRUE)) %>%
  arrange(refpt) %>%
  filter(refpt != "virgin")

as.data.frame(brps)
#    fprop_s1 fprop_s2   refpt      fbar    yield     pY_s1     pY_s2       ssb     ratioB   ratioSPR    ratioR hr_b1plus    hr_ssb
# 1       0.4      0.6     msy 1.2333333 23437.52 0.5413264 0.4586736 16557.983 0.16557983 0.18875816 0.8772062 0.6135548 1.4154817
# 2       0.4      0.6    f0.1 0.5000000 20595.80 0.4828034 0.5171966 35703.194 0.35703194 0.37489217 0.9523590 0.3457494 0.5768616
# 3       0.4      0.6  spr.20 1.1500000 23417.28 0.5354146 0.4645854 17753.368 0.17753368 0.20037996 0.8859852 0.5907185 1.3190333
# 4       0.4      0.6  spr.30 0.6916667 22214.97 0.4996371 0.5003629 27955.395 0.27955395 0.29956634 0.9331955 0.4337831 0.7946579
# 5       0.4      0.6  spr.35 0.5583333 21211.18 0.4880517 0.5119483 32979.372 0.32979372 0.34841056 0.9465664 0.3744002 0.6431652
# 6       0.4      0.6  spr.40 0.4500000 19953.10 0.4782173 0.5217827 38373.249 0.38373249 0.40085103 0.9572945 0.3197179 0.5199742
# 7       0.4      0.6  spr.50 0.3083333 17299.38 0.4647915 0.5352085 48289.034 0.48289034 0.49725450 0.9711131 0.2374514 0.3582466
# 8       0.4      0.6  F.20B0 1.0166667 23292.08 0.5256041 0.4743959 19983.874 0.19983874 0.22206544 0.8999092 0.5510633 1.1655438
# 9       0.4      0.6  F.30B0 0.6333333 21834.16 0.4946383 0.5053617 29976.662 0.29976662 0.31921755 0.9390669 0.4087749 0.7283721
# 10      0.4      0.6  F.35B0 0.5166667 20785.11 0.4843141 0.5156859 34885.410 0.34885410 0.36694148 0.9507077 0.3541174 0.5958110
# 11      0.4      0.6  F.40B0 0.4250000 19584.00 0.4758941 0.5241059 39845.212 0.39845212 0.41516178 0.9597514 0.3061530 0.4915019 ***
# 12      0.4      0.6  F.50B0 0.2916667 16876.21 0.4631712 0.5368288 49761.431 0.49761431 0.51156947 0.9727209 0.2268427 0.3391424
# 13      0.3      0.7  F.50R0 3.3333333 20937.59 0.5461127 0.4538873  6478.267 0.06478267 0.09076093 0.7137727 0.9325297 3.2319746
# 14      0.3      0.7 F.90msy 3.1250000 21369.11 0.5352808 0.4647192  7035.477 0.07035477 0.09617824 0.7315039 0.9137079 3.0373368


# Fmsy not considered valid for short-lived stocks
# (as ususally allows very high exploitation near to F_20%B0, 
#  which might be used as Blim)
# In this case Blim is the biomass that leads to 0.7R0 according to the SR model (WKLIFE)

F40B0 <- brps %>% filter(refpt=="F.40B0")
F40B0$fbar # 0.425

ref.pts <- c( B0 = B0, R0 = R0, Blim = Blim, Fmsy = F40B0$fbar, Bmsy=F40B0$ssb, MSY=F40B0$yield)

# F proportion in each semester (for 50% catches in each semester)
fprop_s1 <- F40B0$fprop_s1


#==============================================================================
# SAVE DATA                                                                ----
#==============================================================================

save( stk, sr_model, sr_params, ref.pts, fprop_s1,  
      file=file.path(inp.dir,paste(stkn,sc,"_dataLH.RData",sep="")))

