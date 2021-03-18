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


df_bc <- df_bc %>% filter(ADVT != "fix")# & BSAFE == "none" & indicator %in% perfnms)

# Options in variables

STKNnms  <- unique(df_bc$STKN)
FHISTnms <- levels(df_bc$FHIST)
TERMnms  <- levels(df_bc$term)
HCRTnms  <- unique(df_bc$HCRT)
UCnms    <- unique(df_bc$UC)



#==============================================================================
# App                                                                      ----
#==============================================================================

# run app

runApp()

# for deployment

# library(rsconnect)
# deployApp()
