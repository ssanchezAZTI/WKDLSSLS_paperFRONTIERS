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
library(R.utils)


#==============================================================================
# App                                                                      ----
#==============================================================================

# run app

runApp()

# for deployment

library(rsconnect)
# deployApp()
deployApp(appName = "Sanchezetal2021_FMS")

# to check errors in case of deployment. Alternatively, it's also possible to see the log file of the app in the Shinyapps.io account
# rsconnect::showLogs(appName = "Sanchezetal2021_FMS")
