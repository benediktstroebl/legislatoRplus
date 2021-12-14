# Import shapefiles for Wahlkreise per session

# btw21
btw21_wahlkreise_spdf <-
  readOGR(dsn = "data/btw21/shapefiles/Geometrie_Wahlkreise_20DBT_geo.shp",
          encoding = "UTF-8",
          use_iconv = TRUE) %>%
  # Transform spatial polygons data frame to longlat data
  spTransform(CRS("+init=epsg:4326"))


# btw17
btw17_wahlkreise_spdf <-
  readOGR(dsn = "data/btw17/shapefiles/Geometrie_Wahlkreise_19DBT_geo.shp",
          encoding = "UTF-8",
          use_iconv = TRUE) %>%
  # Transform spatial polygons data frame to longlat data
  spTransform(CRS("+init=epsg:4326"))


# btw13
btw13_wahlkreise_spdf <-
  readOGR(dsn = "data/btw13/shapefiles/Geometrie_Wahlkreise_18DBT.shp",
          encoding = "UTF-8",
          use_iconv = TRUE) %>%
  # Transform spatial polygons data frame to longlat data
  spTransform(CRS("+init=epsg:4326"))


# btw09
btw09_wahlkreise_spdf <-
  readOGR(dsn = "data/btw09/shapefiles/Geometrie_Wahlkreise_17DBT.shp",
          encoding = "UTF-8",
          use_iconv = TRUE) %>%
  # Transform spatial polygons data frame to longlat data
  spTransform(CRS("+init=epsg:4326"))


# btw05
btw05_wahlkreise_spdf <-
  readOGR(dsn = "data/btw05/shapefiles/Geometrie_Wahlkreise_16DBT_geo.shp",
          encoding = "UTF-8",
          use_iconv = TRUE) %>%
  # Transform spatial polygons data frame to longlat data
  spTransform(CRS("+init=epsg:4326"))


# btw02
btw02_wahlkreise_spdf <-
  readOGR(dsn = "data/btw02/shapefiles/Geometrie_Wahlkreise_15DBT_geo.shp",
          encoding = "UTF-8",
          use_iconv = TRUE) %>%
  # Transform spatial polygons data frame to longlat data
  spTransform(CRS("+init=epsg:4326"))








#
# # Plot on leaflet map
# leaflet() %>%
#   addProviderTiles("CartoDB.Positron", options = providerTileOptions(opacity = 0.99)) %>%
#   addPolygons(
#     data = wahlkreise_ll,
#     stroke = TRUE,
#     weight = 1,
#     color = "#968C83",
#     fillColor = '#968C83',
#     fillOpacity = 0.5,
#     smoothFactor = 0.5,
#     popup = wahlkreise_ll$WKR_NAME,
#     highlightOptions = highlightOptions(
#       color = '#636363',
#       fillColor = '#636363',
#       opacity = 1,
#       weight = 2,
#       fillOpacity = 0.5,
#       bringToFront = TRUE,
#       sendToBack = TRUE
#     )
#   )
