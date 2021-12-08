library(leaflet)
library(rgdal)

## Load the shape file to a Spatial Polygon Data Frame (SPDF) using the readOGR() function
myspdf = readOGR(dsn=getwd(), layer="TM_WORLD_BORDERS_SIMPL-0.3")
