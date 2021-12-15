# Dependencies ------------------------------------------------------------
library(shiny)
library(tidyverse)
library(leaflet)
library(legislatoR)
library(rgdal)
library(sp)
library(plyr)
library(stringr)

#  Load Scripts ----------------------------------------------------------

# Load SpatialPolygon data for map
source("shapefile_loader.R")

# Load Wahlkreis mapping for each session
source("wahlkreis_data_loader.R")


# Constants used throughout code  ----------------------------------------
char_to_replace_for_join <- "-|–|[:blank:]|–|[.]"
