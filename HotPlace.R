setwd(dirname(rstudioapi::getSourceEditorContext()$path))

load("./data/2021/geodataframe/apt_price.rdata")
grid<-st_read("./data/sigun_grid/seoul.shp")

apt_price<-st_join(apt_price, grid,join= st_intersects)
head(apt_price,2)


kde_before <- subset(apt_price,dealDate < "2021-07-01")
kde_before<- aggregate(kde_before$py, by=list(kde_before$ID),mean)
colnames(kde_before)<- c("ID", "before")


kde_after <- subset(apt_price,dealDate > "2021-07-01")
kde_after<- aggregate(kde_after$py, by=list(kde_after$ID),mean)
colnames(kde_after)<- c("ID", "after")


kde_diff<- merge(kde_before,kde_after, by="ID")
kde_diff$diff<-round((((kde_diff$after-kde_diff$before)/kde_diff$before)*100),0)

head(kde_diff,2)



library(sf)

kde_diff <-kde_diff[kde_diff$diff >0 ,]

kde_hot <- merge(grid, kde_diff, by="ID")

library(ggplot2)
library(dplyr)

kde_hot %>% ggplot(aes(fill = diff)) + geom_sf()+ scale_fill_gradient(low= "white", high="red")



#---# [4단계: 지도경계선 그리기]

library(sp)   # install.packages("sp")
kde_hot_sp <- as(st_geometry(kde_hot), "Spatial") # sf형 => sp형 변환
x <- coordinates(kde_hot_sp)[,1]  # 그리드 x, y 좌표 추출
y <- coordinates(kde_hot_sp)[,2]

l1 <- bbox(kde_hot_sp)[1,1] - (bbox(kde_hot_sp)[1,1]*0.0001) # 그리드 기준 경계지점 설정
l2 <- bbox(kde_hot_sp)[1,2] + (bbox(kde_hot_sp)[1,2]*0.0001)
l3 <- bbox(kde_hot_sp)[2,1] - (bbox(kde_hot_sp)[2,1]*0.0001)
l4 <- bbox(kde_hot_sp)[2,2] + (bbox(kde_hot_sp)[1,1]*0.0001)

library(spatstat)  # install.packages("spatstat")
win <- owin(xrange=c(l1,l2), yrange=c(l3,l4))  # 경계지점 기준 외곽선 만들기(bounding-box)
plot(win)                                      # 확인
rm(list = c("kde_hot_sp", "apt_price", "l1", "l2", "l3", "l4")) # 메모리 정리

#---# [5단계: 밀도 그래프 변환하기]

p <- ppp(x, y, window=win, marks=kde_hot$diff) # 경계창 위에 좌표값 포인트 생성
d <- density.ppp(p, weights=kde_hot$diff,      # 포인트를 커널밀도 함수로 변환
                 sigma = bw.diggle(p),
                 kernel = 'gaussian')
plot(d)   # 확인
rm(list = c("x", "y", "win","p")) # 변수 정리

#---# [6단계: 픽셀 -> 레스터 변환]

d[d < quantile(d)[4] + (quantile(d)[4]*0.1)] <- NA  # 노이즈 제거
library(raster)         # install.packages("raster")
raster_hot <- raster(d) # 레스터 변환
plot(raster_hot) #  확인

#---# [7단계: 클리핑]

bnd <- st_read("./data/sigun_grid/seoul.shp") # 서울시 경계선 불러오기
raster_hot <- crop(raster_hot, extent(bnd))            # 외곽선 클리핑
crs(raster_hot) <- sp::CRS("+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84
                        +towgs84=0,0,0")  # 좌표계 정의
plot(raster_hot)   #  확인
plot(bnd, col=NA, border = "red", add=TRUE)

#---# [8단계: 지도 그리기]

library(leaflet)   # install.packages("leaflet")
leaflet() %>%
  #---# 베이스맵 불러오기
  addProviderTiles(providers$CartoDB.Positron) %>%
  #---# 서울시 경계선 불러오기
  addPolygons(data = bnd, weight = 3, color= "red", fill = NA) %>%
  #---# 레스터 이미지 불러오기
  addRasterImage(raster_hot,
                 colors = colorNumeric(c("blue", "green", "yellow","red"),
                                       values(raster_hot), na.color = "transparent"), opacity = 0.4)



path <- "./data/2021/map"
if (!dir.exists(path)) {
  dir.create(path)
}else {
  cat("경로가 이미 존재합니다.", "\n\n")
}


rdata_path <- "./data/2021/map/kde_hot.rdata"
if (!file.exists(rdata_path)) {
  save(raster_hot, file = rdata_path)
}else {
  cat("파일이 이미 존재합니다.", "\n\n")
}
rm(list = ls())


