################################################################################
#  Script to prepare csv files with list of OMs, MPs and scenarios             # 
#------------------------------------------------------------------------------#
#   AZTI-Tecnalia                                                              #
#   created:  11/06/2020                                                       #
#   modified:                                                                  #
################################################################################

# Copyright: AZTI, 2020
# Author: Sonia Sanchez, AZTI (<ssanchez@azti.es>)
#
# Distributed under the terms of the GNU GPLv3


library(dplyr)


#==============================================================================
# CREATE CSV WITH ALL THE OPERATING MODELS + OBSERVATION ERROR             ----
#==============================================================================

# this is the list of options we want to explore regarding the operating models and observation error:
# the population (past and future) is going to be generated only once, and then all the MP's will be applied on the same populations

# STOCK
# - STK1 : anchovy and Norway pout type
# - STK2 : sprat and sardine type

stkn.opt <- c("STK1", "STK2")  

# LIFE HISTORY SCENARIO: 
# - bc       : base case (steepness = 0.75)
# - lowprod  : low productivity (steepness = 0.5)
# - highprod : high productivity (steepness = 0.9)

lhsc.opt <- c("bc", "lowprod", "highprod")

# RECRUITMENT uncertainty
# 0.75, 1, 0.5

sigr.opt <- c(0.75, 1, 0.5)

# INITIAL EXPLOITATION: HISTORICAL PERIOD
# (increase linearly on first 10 years and then keep at that level during the next 20 years)
# - fopt : Fmsy ~ F40B0
# - flow : 0.5*fopt
# - fhigh: 2*fopt

fhist.opt <- c( "fopt", "flow", "fhigh")

# CV of F in the HISTORICAL PERIOD

cvfh.opt <- c(0.1)

# TYPE OF INDEX
# - b1p: Biomass 1+

idxt.opt <- "b1p"

# CV of INDEX
# - low: 0.25
# - high: 0.5
# - iav: same as interannual variation (that depends on sigr and fhist)
# - iav2:  2*iav: two times the iav

cvid.opt <- c("low","high","iav","iav2")


# create list of parameters for scenarios

om.csv <- expand.grid(STKN=stkn.opt,	
                      LHSC=lhsc.opt,
                      SIGR=sigr.opt,	
                      FHIST=fhist.opt, 
                      CVFH=cvfh.opt,
                      IDXT=idxt.opt,
                      CVID=cvid.opt)

# order them

idx <- order(om.csv$STKN, om.csv$LHSC, om.csv$SIGR, om.csv$FHIST, om.csv$CVFH, om.csv$IDXT, om.csv$CVID)
om.csv <- om.csv[idx, ]


# Select specific scenarios to run:
#------------------------------------

# - BC run: LHSC = "bc" + SIGR = 0.75 + IDXT = "b1p" + CVID = "low"
om.bc       <- om.csv %>% filter(LHSC == "bc" & SIGR == 0.75 & IDXT == "b1p" & CVID == "low")

# - Sens. to the OM:
om.sensOM   <- om.csv %>% filter((LHSC != "bc" | SIGR != 0.75) & IDXT == "b1p" & CVID == "low")
# om.sensLHSC <- om.csv %>% filter(LHSC != "bc" & SIGR == 0.75 & IDXT == "b1p" & CVID == "low")
# om.sensSIGR <- om.csv %>% filter(LHSC == "bc" & SIGR != 0.75 & IDXT == "b1p" & CVID == "low")

# - Sens. to sampling quality
om.sensCVID <- om.csv %>% filter(LHSC == "bc" & SIGR == 0.75 & IDXT == "b1p" & CVID != "low")

# Number of cases in each
n.bc       <- nrow(om.bc)
n.sensOM   <- nrow(om.sensOM)
n.sensCVID <- nrow(om.sensCVID)


# Final scenarios  
#------------------

bc       <- 1:n.bc
sensOM   <- (n.bc+1):(n.bc+n.sensOM)
sensCVID <- (n.bc+n.sensOM+1):(n.bc+n.sensOM+n.sensCVID)

# add columns that give OM names sequentially
om.csv <- cbind( OM=paste0("om", sprintf("%03d",1:(n.bc+n.sensOM+n.sensCVID))), 
                 bind_rows(om.bc, om.sensOM, om.sensCVID))


# save in an external file

write.csv(om.csv, file=file.path("input", "list_oms.csv"), row.names=FALSE)


#==============================================================================
# CREATE CSV WITH ALL THE MPs:                                             ----
# HCR, CALENDAR, INITIALIZATION, UNCERTAINTY CAP, PRECAUTIONARY BUFFER, ETC
#==============================================================================

# this is the list of options we want to explore regarding the management procedures:
# all of them are going to be tested for the BC scenario (population + obs error)

# ADVICE
# - iny : in-year advice (at the middle of the year)
# - fpa : full population advice
# - int : interim year advice
# - fix : only for TAC=0 (hcrt=="ft0")

advt.opt <- c( "iny", "fpa", "int")

# TYPE OF HCR
# - ft0 : fixed TAC to 0
# - 1o2 : 1 over 2
# - 1o3 : 1 over 3
# - 2o3 : 2 over 3
# - 1o5 : 1 over 5

hcrt.opt <- c( "1o2", "1o3", "2o3", "1o5")

# BUFFER
# - 0.20 : with 20% buffer the 1st simulation year
# - 0.00 : without any buffer

pbuf.opt <- c(0, 0.2)

# UNCERTAINTY CAP (lower and upper)
# - (0.00,0.00) : no uncertainty cap
# - (0.20,0.20) : 20% symmetric uncertainty ca
# - (0.50,0.50) : 50% symmetric uncertainty cap
# - (0.80,0.80) : 80% symmetric uncertainty cap
# - (0.20,0.25) : 20% low and 25% up uncertainty cap
# - (0.50,1.00) : 50% low and 100% up uncertainty cap
# - (0.50,1.50) : 50% low and 150% up uncertainty cap
# - (0.80,2.75) : 80% low and 275% up uncertainty cap
# - (0.80,4,00) : 80% low and 400% up uncertainty cap
# - (0.80,5.25) : 80% low and 525% up uncertainty cap

# ucp.opt <- data.frame( c(0,0), 
#                        c(0.2,0.2), c(0.2,0.25), 
#                        c(0.5,0.5), c(0.5,1), c(0.5,1.5), 
#                        c(0.8,0.8), c(0.8,2.75), c(0.8,4), c(0.8,5.25))

ucp.opt <- data.frame( low = c(0, 0.2, 0.20, 0.5, 0.5, 0.5, 0.8, 0.80, 0.8, 0.80), 
                       up  = c(0, 0.2, 0.25, 0.5, 1.0, 1.5, 0.8, 2.75, 4.0, 5.25))

ucpRef.opt       <- 1:nrow(ucp.opt)
ucpRef.opt_Itrig <- c( 1:2, 7, 9)

# HCR initialization
# - fix : for fixed TAC, no initialization required
# - pyc : no information on current exploitation --> previous year catch
# - nin : no information on current exploitation --> avg of 2/3 last years' catches
# - pob : perfect observation on current exploitation --> Fmsy/avgF(last5yr)*avgC(last5yr)

# alternatives: pyc, ave (average), opt (optimum)

hcri.opt <- c( "pyc", "nin", "pob")

# Biomass safeguard: min(I_last/Itrig)
# - none   : without biomass safeguard
# - Imin   : Itrig = minimum observed value
# - Iminpa : Itrig = 1.4 * Imin
# - Inorm  : Itrig = exp(mu(logI)-1.645*sig(logI))

bsafe.opt <- c("Imin","Iminpa","Inorm") 

# create list of parameters for scenarios
# first TAC=0

mp.csv <- expand.grid(ADVT="fix",
                      HCRT="ft0",
                      PBUF=0,
                      UCP=NA, # to be deleted
                      UCPL=0,
                      UCPU=0,
                      HCRI="fix", 
                      BSAFE="none")

# The rest HCRs without biomass safeguard (without buffer: PBUF=0 & one specific initializing)

mp.csv <- rbind(mp.csv,
                expand.grid(ADVT=advt.opt,
                            HCRT=hcrt.opt,
                            PBUF=0,          # no precautionary buffer
                            UCP=ucpRef.opt,
                            UCPL=NA,
                            UCPU=NA,
                            HCRI="nin",      # mean of m last years (for the n-over-m rule)
                            BSAFE="none"))

# HCRs 1o2 & 2o3 with biomass safeguard (without buffer: PBUF=0 & one specific initializing)

mp.csv <- rbind(mp.csv,
                expand.grid(ADVT=advt.opt,
                            HCRT=c("1o2","2o3"),
                            PBUF=0,          # no precautionary buffer
                            UCP=ucpRef.opt_Itrig,
                            UCPL=NA,
                            UCPU=NA,
                            HCRI="nin",      # mean of m last years (for the n-over-m rule)
                            BSAFE=bsafe.opt))


# set lower and upper uncertainty caps (given ucpRef.opt)

mp.csv <- mp.csv %>% mutate(UCPL = case_when(!is.na(UCP) ~ ucp.opt$low[UCP],
                                             TRUE ~ UCPL), 
                            UCPU = case_when(!is.na(UCP) ~ ucp.opt$up[UCP],
                                             TRUE ~ UCPU)) %>% 
  select(-UCP)

# order them

# idx <- order(mp.csv$ADVT, mp.csv$HCRT, mp.csv$HCRI, mp.csv$PBUF, mp.csv$UCPL, mp.csv$BSAFE)
# mp.csv <- mp.csv[idx, ]

mp.csv_bsafe0 <- mp.csv %>% filter(BSAFE == "none") %>% arrange(ADVT, HCRT, HCRI, PBUF, UCPL, BSAFE)
mp.csv_bsafe1 <- mp.csv %>% filter(BSAFE != "none") %>% arrange(ADVT, HCRT, HCRI, PBUF, UCPL, BSAFE)
mp.csv02 <- bind_rows(mp.csv_bsafe0, mp.csv_bsafe1)

# add columns that give MP names sequentially

mp.csv <- cbind(MP=paste0("mp", sprintf("%04d",1:nrow(mp.csv))), mp.csv)

# save in an external file

write.csv(mp.csv, file=file.path("input","list_mps.csv"), row.names=FALSE)

# # mp.bc: only cases with BSAFE == "none"
# mp.sens <- mp.csv[1:nrow(mp.csv_bsafe0),]


#==============================================================================
# CREATE CSV WITH ALL THE SCENARIOS:                                       ----
# COMBINATION OF OMs and MPs
#==============================================================================

# create all combinations of base case OMs and MPs

om.bc <- om.csv[bc,]
sc.csv <- expand.grid(OM=unique(om.bc$OM),
                      MP=unique(mp.csv$MP))

# order them

idx <- order(sc.csv$OM, sc.csv$MP)
sc.csv <- sc.csv[idx, ]

# add columns that give scenario names sequentially

sc.csv <- cbind(SCENARIO=paste0("sc", sprintf("%06d",1:nrow(sc.csv))), sc.csv)

# merge the parameters of each OM
# we don't use merge because it rearranges columns and rows
# sc.csv <- merge(sc.csv, om.csv, by="OM", sort=FALSE)

idx <- match(sc.csv$OM, om.csv$OM)
sc.csv <- cbind(sc.csv, om.csv[idx, -1])

# merge the parameters of each MP
# sc.csv <- merge(sc.csv, mp.csv, by="MP", sort=FALSE)

idx <- match(sc.csv$MP, mp.csv$MP)
sc.csv <- cbind(sc.csv, mp.csv[idx, -1])

# save in an external file

write.csv(sc.csv, file=file.path("input","list_scenarios.csv"), row.names=FALSE)


#==============================================================================
# EXPAND CSVs WITH SENSITIVITY SCENARIOS: 
# - sensitivity to OM
# - sensitivity to CVID
#==============================================================================

# OM: only sensitivity scenarios

om.sensOM   <- om.csv[sensOM, ]
om.sensCVID <- om.csv[sensCVID, ]


# MP: only selected rules

mp.sensOM   <- mp.csv %>% filter( ADVT == "iny" & HCRI == "nin" & PBUF == 0 & 
                                    ((HCRT == "1o2" &  UCPL == 0.8 & UCPU == 0.8 ) | (HCRT == "1o2" &  UCPL == 0 & UCPU == 0 ) | 
                                       (HCRT == "1o2" &  UCPL == 0.8 & UCPU == 4 ) | (HCRT == "2o3" &  UCPL == 0.8 & UCPU == 0.8)) & 
                                    BSAFE=="none")

mp.sensCVID <- mp.csv %>% filter( ADVT == "iny" & HCRI == "nin" & PBUF == 0 & 
                                    ((HCRT == "1o2" &  UCPL == 0.8 & UCPU == 0.8 ) | (HCRT == "2o3" &  UCPL == 0.8 & UCPU == 0.8) | 
                                       (UCPL == 0 & UCPU == 0 )) & BSAFE=="none")


# new scenarios

nsc <- rbind( expand.grid(OM=om.sensOM$OM, MP=mp.sensOM$MP), 
              expand.grid(OM=om.sensCVID$OM, MP=mp.sensCVID$MP))


sc.csv <- rbind(sc.csv, data.frame(SCENARIO=paste0("sc", sprintf("%06d",nrow(sc.csv)+(1:nrow(nsc)))),
                                   nsc,
                                   om.csv[match(nsc$OM, om.csv$OM),-1],
                                   mp.csv[match(nsc$MP, mp.csv$MP),-1]))
# save in an external file

write.csv(sc.csv, file=file.path("input","list_scenarios.csv"), row.names=FALSE)


