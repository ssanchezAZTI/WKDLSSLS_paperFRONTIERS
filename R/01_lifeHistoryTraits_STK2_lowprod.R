################################################################################
#  WKDLSSLS (conditioning an OM based on life history traits)                  # 
#            Pop type 2: sprat / sardine (low productivity)                    #
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

sc <- "lowprod"
# sc <- paste("RECM", recm, "_MAGE", Mage, "_SELM", selm, 
#             "_MATM", matm, "_IDXT", idxt, sep="")


#==============================================================================
# LIFE HISTORY PARAMETERS                                                  ----
#==============================================================================

# PARAMETERS

ages <- 0:6                 # general ranges adopted for all cases
fbar.ages <- c(min=1,max=3)


# SR             : s (s); v (virgin biomass); spr0 (spawners per recruit at F=0)

s  <- 0.5
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
# #          a          b  
# # 2904849.58   33333.33
# sr_params1
# #          a          b  
# # 2904849.58   33333.33 

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
#    fprop_s1 fprop_s2   refpt      fbar      yield     pY_s1     pY_s2        ssb      ratioB  ratioSPR     ratioR hr_b1plus    hr_ssb
# 1       0.4      0.6     msy 0.2916667 11827.9071 0.4631712 0.5368288 34875.9270 0.348759270 0.5115695 0.68174374 0.2268427 0.3391424
# 2       0.4      0.6    f0.1 0.2166667 11365.0773 0.4557792 0.5442208 44940.3222 0.449403222 0.5870524 0.76552487 0.1763373 0.2528927
# 3       0.4      0.6  spr.20 0.8666667   480.2357 0.5140903 0.4859097   483.1784 0.004831784 0.2536238 0.01905099 0.5012025 0.9939096
# 4       0.4      0.6  spr.30 0.6916667  5251.7717 0.4996371 0.5003629  6608.8465 0.066088465 0.2995663 0.22061378 0.4337831 0.7946579
# 5       0.4      0.6  spr.35 0.5583333  8439.2323 0.4880517 0.5119483 13121.4076 0.131214076 0.3484106 0.37660764 0.3744002 0.6431652
# 6       0.4      0.6  spr.40 0.4500000 10458.4854 0.4782173 0.5217827 20113.4695 0.201134695 0.4008510 0.50176920 0.3197179 0.5199742
# 7       0.4      0.6  spr.50 0.3083333 11810.4099 0.4647915 0.5352085 32967.2626 0.329672626 0.4972545 0.66298575 0.2374514 0.3582466
# 8       0.4      0.6  F.20B0 0.4500000 10458.4854 0.4782173 0.5217827 20113.4695 0.201134695 0.4008510 0.50176920 0.3197179 0.5199742
# 9       0.4      0.6  F.30B0 0.3333333 11717.5938 0.4672062 0.5327938 30288.4635 0.302884635 0.4771635 0.63476073 0.2529726 0.3868666
# 10      0.4      0.6  F.35B0 0.2916667 11827.9071 0.4631712 0.5368288 34875.9270 0.348759270 0.5115695 0.68174374 0.2268427 0.3391424
# 11      0.4      0.6  F.40B0 0.2500000 11692.0450 0.4590844 0.5409156 40139.2608 0.401392608 0.5510445 0.72842146 0.1993618 0.2912870 ***
# 12      0.4      0.6  F.50B0 0.1833333 10800.0245 0.4524433 0.5475567 50376.2964 0.503762964 0.6278222 0.80239747 0.1523295 0.2143870
# 13      0.4      0.6  F.50R0 0.4500000 10458.4854 0.4782173 0.5217827 20113.4695 0.201134695 0.4008510 0.50176920 0.3197179 0.5199742
# 14      0.4      0.6 F.90msy 0.1750000 10615.6713 0.4516048 0.5483952 51849.0890 0.518490890 0.6388682 0.81157728 0.1461672 0.2047417


# Fmsy not considered valid for short-lived stocks
# (as usually allows very high exploitation near to F_20%B0, 
#  which might be used as Blim)
# In this case Blim is the biomass that leads to 0.7R0 according to the SR model (WKLIFE)

F40B0 <- brps %>% filter(refpt=="F.40B0")
F40B0$fbar # 0.25

ref.pts <- c( B0 = B0, R0 = R0, Blim = Blim, Fmsy = F40B0$fbar, Bmsy=F40B0$ssb, MSY=F40B0$yield)

# F proportion in each semester (for 50% catches in each semester)
fprop_s1 <- F40B0$fprop_s1


#==============================================================================
# SAVE DATA                                                                ----
#==============================================================================

save( stk, sr_model, sr_params, ref.pts, fprop_s1, 
      file=file.path(inp.dir,paste(stkn,sc,"_dataLH.RData",sep="")))

