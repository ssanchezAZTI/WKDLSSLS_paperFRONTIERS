#==============================================================================
# CREATE CSV WITH ALL THE OPERATING MODELS with F=0                                                                ----
#==============================================================================

# this is the list of options we want to explore regarding the operating models with F=0

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

sigr.opt <- c(0.75, 1, 0.5, 0)

# INITIAL EXPLOITATION: HISTORICAL PERIOD - no exploitation
# - f0   : 0

fhist.opt <- c( "f0")

# CV of F in the HISTORICAL PERIOD

cvfh.opt <- 0

# TYPE OF INDEX

idxt.opt <- NA

# CV of INDEX

cvid.opt <- NA


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

om.csv <- cbind(OM=paste0("om00", sprintf("%01s",letters[1:nrow(om.csv)])), om.csv)

# save in an external file

write.csv(om.csv, file=file.path("input", "list_oms_f0.csv"), row.names=F)


