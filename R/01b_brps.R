################################################################################
#  WKDLSSLS (estimating reference points based on life history traits)         #
#     Note: file should be called from 01_lifeHistoryTraits_*.R                #
#           (otherwise additional parameters should be defined)                #
#------------------------------------------------------------------------------#
#   Sonia Sanchez (AZTI-Tecnalia)                                              #
#   created:  02/12/2019                                                       #
#   modified: 2019-12-04 11:27:03 generalised for any stock                    #
################################################################################

# 01b_brps.R - estimating reference points based on life history traits
# ~/WKDLSSLS_2019/R/01b_brps.R

# Copyright: AZTI, 2019
# Author: Sonia Sanchez (AZTI) (<ssanchez@azti.es>)
#         Based on http://www.flr-project.org/doc/Data_Poor_MSE_in_FLBEIA.html
#
# Distributed under the terms of the GNU GPLv3


#==============================================================================
# LOAD LIBRARIES AND FUNCTIONS                                             ----
#==============================================================================

require(dplyr)

source(file.path("./R/fun","brps_seasonal.R"))


#==============================================================================
# DATA                                                                     ----
#==============================================================================

# take 1st year with information on selectivity (i.e. year=2)

stk1 <- iter(window(stk, start = 2, end = 2), 1)

runs.file <- file.path(res.dir,"brps",paste(stkn,sc,"_msyruns.csv",sep=""))
brps.file <- file.path(res.dir,"brps",paste(stkn,sc,"_brps.csv",sep=""))

if (file.exists(runs.file)) file.remove(runs.file)
if (file.exists(brps.file)) file.remove(brps.file)


#==============================================================================
# BRPS estimation                                                          ----
#==============================================================================

# Loop by percentage of catches in each season

for (p in seq(0.1,0.9,0.1)) {
  
  print(paste("p =",p))
  
  fruns <- brpsson( stk1, B0=B0, R0=R0, rec.ss=rec.ss, ssb.ss=ssb.ss, 
                    sr_model="bevholt", sr_params=sr_params, 
                    Fprop = c(p, 1-p))
  
  # simulations
  
  if (file.exists(runs.file)) {
    write.table(fruns$runs, file=runs.file, sep=";", row.names=F, append=T, col.names=F)
  } else {
    write.table(fruns$runs, file=runs.file, sep=";", row.names=F, append=F)
  }
  
  
  # reference points
  
  if (file.exists(brps.file)) {
    write.table(fruns$refpts, file=brps.file, sep=";", row.names=F, append=T, col.names=F)
  } else {
    write.table(fruns$refpts, file=brps.file, sep=";", row.names=F, append=F)
  }
  
}


#==============================================================================
# PLOTS                                                                    ----
#==============================================================================

# Fbar vs. %SPR & catch (plots)

runs <- read.csv(runs.file, sep=";")

brpsson_plot( runs, pdfflnm=file.path(plot.dir,"brps",paste(stkn,sc,"_Fbar_vs_SPR.pdf",sep="")) )


#==============================================================================
# DELETE OBJECTS                                                           ----
#==============================================================================

rm( stk1, runs.file, fruns, runs)

