library(sp)
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
load("./data/2021/preprocess/preprocess.rdata")
load("./data/2021/geocoding/juso_geocoding.rdata")
library(dplyr)

apt_price <- left_join(apt_price, juso_geocoding, by = c("juso_jibun" = "apt_juso"))

apt_price <- na.omit(apt_price)


colnames(apt_price)
head(apt_price)

coordinates(apt_price) <- ~coord_x +coord_y

proj4string(apt_price) <- "+proj=longlat +datum=WGS84 +no_defs"
library(sf)

apt_price <-st_as_sf(apt_price)

plot(apt_price$geometry, axes = T, pch =1)

library(leaflet)

leaflet() %>%
  addTiles() %>%
  addCircleMarkers(data = apt_price[1:1000, ], label = ~juso_jibun)


dir.create("./data/2021/geodataframe")

save(apt_price, file="./data/2021/geodataframe/apt_price.rdata")
write.csv(apt_price, "./data/2021/geodataframe/apt_price.csv")