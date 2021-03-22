################################################################################
#  Shiny App - code to Launch Shiny App                                        # 
#------------------------------------------------------------------------------#
#   AZTI-Tecnalia                                                              #
#   created:  17/03/2021                                                       #
#   modified:                                                                  #
################################################################################

# Copyright: AZTI, 2021
# Author: Sonia Sanchez, AZTI (<ssanchez@azti.es>)
#         from Maria Korta's template
#
# Distributed under the terms of the GNU GPLv3



#==============================================================================
# Load libraries                                                           ----
#==============================================================================

library(shiny)
library(shinyBS)
library(shinythemes)
library(shinyjs)
library(markdown)

library(dplyr)


#==============================================================================
# Load data                                                                ----
#==============================================================================

# Global variables

load(file.path("data","plotinputs.RData"))
# dat_bio, dat_bioQ, df_bc, df_cvid, df_om, ucp.col, ucp.col2, ucp.col3, perfnms, perflabels

plabs <- setNames(perflabels, perfnms)


# - Base Case

df_bc <- df_bc %>% filter(ADVT != "fix")# & BSAFE == "none" & indicator %in% perfnms)

# Options in variables

STKNnms  <- unique(df_bc$STKN)
FHISTnms <- levels(df_bc$FHIST)
TERMnms  <- levels(df_bc$term)
HCRTnms  <- unique(df_bc$HCRT)
UCnms    <- unique(df_bc$UC)

TERMdef <- setNames(c("first 5 projection years","next 5 projection years","last 10 projection years"), 
                    c("short","mid","long"))


# BSAFE

HCRTnms_bsafe <- df_bc %>% filter(BSAFE != "none") %>% .$HCRT %>% unique()
UCnms_bsafe   <- df_bc %>% filter(BSAFE != "none") %>% .$UC %>% unique()



# - Sensitivity to CVID

STKNnms_cvid  <- unique(df_cvid$STKN)
FHISTnms_cvid <- levels(df_cvid$FHIST)
TERMnms_cvid  <- levels(df_cvid$term)
HCRTnms_cvid  <- unique(df_cvid$HCRT)
UCnms_cvid    <- unique(df_cvid$UC)
CVIDnms_cvid  <- levels(df_cvid$CVID)


# - Sensitivity to CVID

STKNnms_om  <- unique(df_om$STKN)
FHISTnms_om <- levels(df_om$FHIST)
TERMnms_om  <- levels(df_om$term)
HCRTnms_om  <- unique(df_om$HCRT)
UCnms_om    <- unique(df_om$UC)


#==============================================================================
# App                                                                      ----
#==============================================================================

# run app

runApp()

# for deployment

# library(rsconnect)
# deployApp()
