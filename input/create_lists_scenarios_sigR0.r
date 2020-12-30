
library(dplyr)

#==============================================================================
# CREATE CSV WITH ALL THE OPERATING MODELS WITH SIGR=0                                                                ----
#==============================================================================

# this is the list of options we want to explore regarding the operating models with SIGR=0

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

sigr.opt <- 0

# INITIAL EXPLOITATION: HISTORICAL PERIOD
# (increase linearly on first 10 years and then keep at that level during the next 20 years)
# - fopt : F40B0
# - flow: 0.5*fopt
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


# add columns that give OM names sequentially
om.csv <- cbind( OM=paste0("omSIGR", sprintf("%03d",1:nrow(om.csv))), 
                 om.csv)


# save in an external file

write.csv(om.csv, file=file.path("input", "list_oms_sigR0.csv"), row.names=F)

