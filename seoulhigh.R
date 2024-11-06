library(sf)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

load("./data/2021/geodataframe/apt_price.rdata")
load("./data/2021/map/kde_high.rdata")

grid <- st_read("./data/sigun_grid/seoul.shp")
library(tmap)
tmap_mode('view')

tm_shape(grid) +
  tm_borders(lwd = 0.5) +  # Set line width for thinner borders
  tm_text("ID", col = "red") +
  tm_shape(raster_high) +
  tm_raster(palette = c("blue", "green", "yellow", "red"), alpha = .4) +
  tm_basemap(server = 'OpenStreetMap')




library(dplyr)


apt_price <-st_join(apt_price, grid, join = st_intersects)

apt_price <- apt_price %>% st_drop_geometry()


all<-apt_price
sel <- apt_price %>% filter(ID ==81016)

dir.create("./data/2021/chart")

save(all, file = "./data/2021/chart/all.rdata")
save(sel, file = "./data/2021/chart/sel.rdata")

rm(list = ls())

