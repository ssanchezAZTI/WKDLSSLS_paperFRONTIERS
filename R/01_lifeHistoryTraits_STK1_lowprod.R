################################################################################
#  WKDLSSLS (conditioning an OM based on life history traits)                  # 
#            Pop type 1: anchovies / NW pout (low productivity)                #
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

# Populations type 1: anchovies + Norway pout

stkn <- "STK1"

# Characteristics:

Mage <- "gl1" # Gislason (with correction for age 0)
matm <- "kn1" # Knife-edge maturity (full at age 1)
selm <- "kn1" # Knife-edge maturity (full at age 1)
recm <- "bhm" # Beverton-Holt + medium steepness (0.75)
sigR <- "075" # SD residuals around SR
ares <- "not" # no autocorrelation in residuals
idxt <- "b1p" # b1+ index

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

# Source: Bellido et al. (2000). Use of frequency analysis methods to estimate growth
# of anchovy (Engraulis encrasicolus L. 1758) in the Gulf of Cadiz (SW, Spain).
# Fisheries Research 48: 107-115.

linf <- 18.69; lmax <- NULL
k    <- 0.89
t0   <- -0.02

# library(FLife)
# data("teleost")
# teleost[,"Engraulis encrasicolus"]
# # An object of class "FLPar"
# # params
# # linf       k      t0     l50       a       b 
# # 19.5381  0.5800 -0.5910 10.9762  0.0048  3.1344 
# # units:  NA 


# Length-weight  : a; b

# Source: parameters in "teleost" object in the R library FLife ()

lw_a <- 0.004799048
lw_b <- 3.134380952


# Maturity       : a50

a50 <- l50 <- NULL

# - knife(a1)
a1_mat    <- 1  # age at full maturity

# # - logistic(a50, ato95, asym)
# asym_mat  <- 1  # fixed maturity for older ages
# ato95_mat <- 1  # lag between a50 and first age with full maturity
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
# mla
# # 9.27138 13.85835 16.70585 17.87520 18.35540 18.55259 18.63357
# mla_stks1
# # 0.3297386 11.1502530 15.5937595 17.4185110 18.1678557 18.4755785 18.6019466
# mla_stks2
# # 6.924299 13.858347 16.705854 17.875199 18.355397 18.552593 18.633573
# mla_cats1
# # 3.992335 12.654319 16.211413 17.672154 18.272016 18.518352 18.619512
# mla_cats2
# # 9.27138 14.82219 17.10166 18.03774 18.42215 18.58000 18.64483
# 
# mwa
# # 5.158863 18.185015 32.665657 40.381869 43.880568 45.375184 45.998867
# mwa_stks1
# # 0.000148223  9.199079706 26.321913716 37.235434133 42.490568211 44.787402842 45.754597194
# mwa_stks2
# # 2.066393 18.185015 32.665657 40.381869 43.880568 45.375184 45.998867
# mwa_cats1
# # 0.3678151 13.6770165 29.7299780 38.9614854 43.2588070 45.1132102 45.8901543
# mwa_cats2
# # 5.158863 22.451248 35.153361 41.544007 44.382661 45.585643 46.086019



#==============================================================================
# MATURITY                                                                 ----
#==============================================================================

# L50: length at which 50% of individuals are mature
if (is.null(a50)) {
  if (is.null(l50)) l50 <- 0.72 * linf^0.93
  a50 <- invVonB(l50, linf, k, t0)
}
  
# # Logistic maturity ogive (from FLife - logistic function)
# # asym; ato95
# 
# FLife:::logisticFn
# 
# mata <- logistic( age=FLQuant(ages,dimnames=list(age=ages)), 
#                   params=FLPar(a50=a50, ato95=ato95, asym=asym))[drop=TRUE]
# 
# 
# mata[1] <- 0 #! problems with SSB when > 0 for age 0

# Knife-edge maturity (from FLife - knife function)
# a1=1

# FLife:::knifeFn

mata <- knife( age=FLQuant(ages,dimnames=list(age=ages)), 
                params=FLPar(a1=a1_mat))[drop=TRUE]


# # CHECK
# mata
# # 0 1 2 3 4 5 6 
# # 0 1 1 1 1 1 1


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
# # 2.899 1.518 1.123 1.008 0.965 0.949 0.942


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
# # 36653020.69    33333.33 
# sr_params1
# #           a           b 
# # 36653020.69    33333.33

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
#    fprop_s1 fprop_s2   refpt fbar     yield     pY_s1     pY_s2        ssb      ratioB  ratioSPR     ratioR hr_b1plus    hr_ssb
# 1       0.3      0.7     msy 0.91 27731.543 0.4657241 0.5342759 35363.5240 0.353635240 0.5152264 0.68636859 0.5202770 0.7841849
# 2       0.4      0.6    f0.1 0.60 25527.825 0.5460208 0.4539792 43984.8080 0.439848080 0.5798861 0.75850777 0.3893461 0.5803782
# 3       0.2      0.8  spr.20 3.92  1096.182 0.5911279 0.4088721   460.6352 0.004606352 0.2534548 0.01817426 1.0025497 2.3797180
# 4       0.2      0.8  spr.30 3.13 12850.568 0.5285142 0.4714858  6660.8441 0.066608441 0.2999563 0.22206046 0.9485553 1.9292702
# 5       0.2      0.8  spr.35 2.46 20949.737 0.4712912 0.5287088 13342.8934 0.133428934 0.3500717 0.38114744 0.8772349 1.5701045
# 6       0.3      0.7  spr.40 1.45 24347.124 0.5219931 0.4780069 19986.4453 0.199864453 0.3998983 0.49978815 0.7047728 1.2181818
# 7       0.3      0.7  spr.50 0.97 27692.760 0.4719820 0.5280180 33256.5955 0.332565955 0.4994245 0.66589840 0.5445140 0.8326998
# 8       0.3      0.7  F.20B0 1.45 24347.124 0.5219931 0.4780069 19986.4453 0.199864453 0.3998983 0.49978815 0.7047728 1.2181818
# 9       0.3      0.7  F.30B0 1.07 27406.140 0.4824232 0.5175768 30010.2410 0.300102410 0.4750768 0.63169240 0.5826526 0.9132262
# 10      0.3      0.7  F.35B0 0.92 27732.592 0.4667665 0.5332335 35003.3789 0.350033789 0.5125253 0.68295899 0.5243894 0.7922833
# 11      0.3      0.7  F.40B0 0.79 27458.810 0.4532399 0.5467601 39996.5436 0.399965436 0.5499741 0.72724416 0.4685698 0.6865296 ***
# 12      0.4      0.6  F.50B0 0.50 24184.207 0.5351381 0.4648619 50025.9366 0.500259366 0.6251945 0.80016594 0.3346288 0.4834334
# 13      0.3      0.7  F.50R0 1.45 24347.124 0.5219931 0.4780069 19986.4453 0.199864453 0.3998983 0.49978815 0.7047728 1.2181818
# 14      0.3      0.7 F.90msy 0.54 24915.338 0.4274774 0.5725226 51996.7490 0.519967490 0.6399756 0.81248016 0.3457598 0.4791711


# Fmsy not considered valid for short-lived stocks
# (as usually allows very high exploitation near to F_20%B0, 
#  which might be used as Blim)
# In this case Blim is the biomass that leads to 0.7R0 according to the SR model (WKLIFE)

F40B0 <- brps %>% filter(refpt=="F.40B0")
F40B0$fbar # 0.79

ref.pts <- c( B0 = B0, R0 = R0, Blim = Blim, Fmsy = F40B0$fbar, Bmsy=F40B0$ssb, MSY=F40B0$yield)

# F proportion in each semester (for 50% catches in each semester)
fprop_s1 <- F40B0$fprop_s1


#==============================================================================
# SAVE DATA                                                                ----
#==============================================================================

save( stk, sr_model, sr_params, ref.pts, fprop_s1,  
      file=file.path(inp.dir,paste(stkn,sc,"_dataLH.RData",sep="")))

