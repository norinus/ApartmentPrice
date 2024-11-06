setwd(dirname(rstudioapi::getSourceEditorContext()$path))

load("./data/2021/geodataframe/apt_price.rdata")
load("./data/2021/map/kde_high.rdata")
load("./data/2021/map/kde_hot.rdata")

library(sf)

grid<-st_read("./data/sigun_grid/seoul.shp")
bnd<- st_read("./data/sigun_grid/seoul.shp")


pcnt_10 <- as.numeric(quantile(apt_price$py, probs =seq(.1,.9,by =.1))[1])

pcnt_90 <- as.numeric(quantile(apt_price$py, probs =seq(.1,.9,by =.1))[9])

load("./data/circle_marker/circle_marker.rdata")

circle.colors <- sample(x=c("red", "green", "blue"), size=1000, replace =TRUE)

library(purrr)


leaflet() %>%
  addTiles() %>%
  addPolygons(data =bnd, weight = 3, color = "red", fill = NA) %>%
  addRasterImage(raster_high, colors = colorNumeric(c("blue", "green", "yellow", "red"), values(raster_high), na.color = "transparent"), opacity = 0.4, group = "2021 최고가")%>%
  addRasterImage(raster_hot, colors = colorNumeric(c("blue", "green", "yellow", "red"), values(raster_hot), na.color = "transparent"), opacity = 0.4, group = "2021 급등지")%>%
  addLayersControl(baseGroups = c("2021 최고가", "2021 급등지"), options = layersControlOptions(collapsed = FALSE)) %>%
  addCircleMarkers(data = apt_price, lng = unlist(map(apt_price$geometry,1)),lat = unlist(map(apt_price$geometry,2)), radius = 10, stroke = FALSE,
                   fillOpacity = 0.6 , fillColor = circle.colors, weight = apt_price$py, clusterOptions = markerClusterOptions(iconCreateFunction =JS(avg.formula)))

rm(list = ls())



