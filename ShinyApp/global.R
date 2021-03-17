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
library(leaflet)
library(ggplot2)
library(shinyWidgets) 
library(shinycssloaders)
library(rhandsontable)
library(plotly)
library(sf)
library(tidyverse)
library(shinyalert)
library(rintrojs)

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

