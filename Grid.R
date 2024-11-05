install.packages("rgdal", repos="http://R-Forge.R-project.org")

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
load("./data/2021/geodataframe/apt_price.rdata")
library(sf)

grid <- st_read("./data/sigun_grid/seoul.shp")

apt_price <- st_join(apt_price, grid, join = st_intersects)

head(apt_price,2)

kde_high <- aggregate(apt_price$py, by=list(apt_price$ID), mean)


colnames(kde_high) <- c("ID", "avg_price")

head(kde_high,2)

kde_high <- merge(grid, kde_high, by="ID")

library(ggplot2)
library(dplyr)


##격제 밀집도 평균 가격 높은 건 red, 낮은값 white
kde_high %>% ggplot(aes(fill= avg_price))+geom_sf()+scale_fill_gradient(low= "white", high = "red")

##---------------------------------------------------------------------------------시작
library(sp)
kde_high_sp <- as(st_geometry(kde_high), "Spatial")
x <- coordinates(kde_high_sp)[,1]
y <- coordinates(kde_high_sp)[,2]


l1  <- bbox(kde_high_sp)[1,1] - (bbox(kde_high_sp)[1,1]*0.0001)
l2 <- bbox(kde_high_sp)[1,2] + (bbox(kde_high_sp)[1,2]*0.0001)
l3 <- bbox(kde_high_sp)[2,1] - (bbox(kde_high_sp)[2,1]*0.0001)
l4 <- bbox(kde_high_sp)[2,2] + (bbox(kde_high_sp)[1,1]*0.0001)

library(spatstat)

win<-owin(xrange= c(l1 ,l2), yrange=c(l3,l4))

##지도 경계선
plot(win)

rm(list = c("kde_high_sp", "apt_price", "l1","l2","l3","l4"))

##밀도 그래프
p<- ppp(x,y,window = win)

d<- density.ppp(p, weights = kde_high$avg_price, sigma = bw.diggle(p), kernel = 'gaussian')

plot(d)

rm(list = c("x","y", "win", "p"))
##---------------------------------------------------------------------------------끝


##---------------------------------------------------------------------------------시작
##노이즈제거 와 래스터 이미지로 변화
d[d < quantile(d)[4] +(quantile(d)[4]*0.1)] <-NA

library(sf)
library(sp)
library(raster)

raster_high<-raster(d)

plot(raster_high)

##---------------------------------------------------------------------------------끝

# 서울의 경계 데이터를 읽어옵니다 (sf 객체)
bnd <- st_read("./data/sigun_grid/seoul.shp")
# raster_high 객체를 bnd_sp의 범위로 자릅니다 (crop)
raster_high <- crop(raster_high, extent(bnd))

# 좌표 참조 체계 (CRS)를 설정
crs(raster_high) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0")
# 결과를 시각화
plot(raster_high)
plot(bnd, col=NA, border="red", add=TRUE)



library(rgdal)
library(leaflet)

#leaflet() %>%
#  addProviderTiles(providers$CartoDB.Positron) %>%
#  addPolygons(data =bnd, weight = 3, color = "red", fill= NA) %>%
#  addRasterImage(raster_high, colors = colorNumeric(c("blue", "green", "yellow","red"), values(raster_high), na.color = "transparent"), opacity = 0.4)

leaflet() %>%
  addProviderTiles(providers$CartoDB.Positron) %>%
  addPolygons(data = bnd, weight = 1, color = "gray", fill = NA) %>%
  addRasterImage(raster_high,
                 colors = colorNumeric(c("blue", "green", "yellow", "red"),
                                       values(raster_high), na.color = "transparent"),
                 opacity = 0.4)


dir.create("./data/2021/map")
save(raster_high, file = "./data/2021/map/kde_high.rdata")
rm(list = ls())
