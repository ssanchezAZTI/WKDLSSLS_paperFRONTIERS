# ----------------------------------
# Global file
# ANICHO 
# February 2021 
# -----------------------------------
# options(encoding = "UTF-8")

# ---------------------
# load packages
# ---------------------

library(shiny)
library(shinyjs)
library(shinythemes) 
# library(leaflet)
library(ggplot2)
# library(shinyWidgets) 
# library(shinycssloaders)
# library(rhandsontable)
# library(plotly)
# library(sf)
library(tidyverse)
# library(shinyalert)
library(rintrojs)
library(R.utils)

# ---------------------
# ggplot bw background
# ---------------------

theme_set(theme_bw(base_size=16))

# ---------------------
# password
# ---------------------


# ---------------------
# R scripts
# ---------------------

source("funs/appParts.R")

# ---------------------
# data
# ---------------------

# Global variables

load(file.path("data","plotinputs.RData"))
# dhist, dat_bio, dat_bioQ, df_bc, df_cvid, df_om, ucp.col, ucp.col2, ucp.col3, perfnms, perflabels

plabs <- setNames(perflabels, perfnms)


# - Historical part

STKNnms_hist  <- unique(dhist$STKN)
LHSCnms_hist  <- levels(dhist$LHSC)
SIGRnms_hist  <- levels(dhist$SIGR)
FHISTnms_hist <- levels(dhist$FHIST)


# - Trajectories

dat_bioQ <- dat_bioQ %>% filter(ADVT != "fix")

STKNnms_q  <- unique(dat_bioQ$STKN)
FHISTnms_q <- levels(dat_bioQ$FHIST)


# - Base Case

df_bc <- df_bc %>% filter(ADVT != "fix")

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

# - Use named vector to fix the colours for UCs

names(ucp.col) <- c("(0.2,0.2)","(0.2,0.25)", "(0.5,0.5)","(0.5,1)","(0.5,1.5)",
                    "(0.8,0.8)", "(0.8,2.75)","(0.8,4)","(0.8,5.25)","(NA,NA)")
names(ucp.col2) <- c("(0.5,0.5)","(0.8,0.8)", "(0.8,2.75)","(0.8,4)","(0.8,5.25)","(NA,NA)") 
names(ucp.col3) <- c("(0.2,0.2)","(0.8,0.8)", "(0.8,4)","(NA,NA)")
  


