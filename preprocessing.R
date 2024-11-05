setwd(dirname(rstudioapi::getSourceEditorContext()$path))
options(warn = -1)

load("./data/2021/integrated/apt_price.rdata")

head(apt_price, 2)
head(apt_price$dealAmount, 2)

##table(is.na(apt_price))
##apt_price<-na.omit(apt_price)
##table(is.na(apt_price))

library(lubridate)
library(dplyr)
library(withr)
library(stringr)


names(apt_price)[names(apt_price) == "sggCd"] <- "code"
##거래 년,월,일 -> 하나로 합치기
apt_price <- apt_price %>% mutate(dealDate = make_date(dealYear, dealMonth, dealDay))


apt_price$dealym <- floor_date(apt_price$dealDate, "month")
head(apt_price, 2)

##거래 가격 숫자로 변경
apt_price$dealAmount <- apt_price$dealAmount %>%
  sub(",", "", .) %>%
  as.numeric()

head(apt_price$dealAmount, 2)

apt_price$aptNm <- gsub("\\(.*", "", apt_price$aptNm)

head(apt_price$aptNm, 30)


loc <- read.csv("./sigun_code.csv", fileEncoding = "UTF-8")

apt_price <- merge(apt_price, loc, by = 'code')

apt_price$juso_jibun <- paste0(apt_price$addr_2, " ", apt_price$umdNm, " ", apt_price$jibun, " ", apt_price$aptNm)

head(apt_price, 2)


apt_price$buildYear <- apt_price$buildYear %>% as.numeric()
head(apt_price$buildYear, 2)
apt_price$excluUseAr <- apt_price$excluUseAr %>%
  as.numeric() %>%
  round(0)
head(apt_price$excluUseAr, 2)

apt_price$py <- round(((apt_price$dealAmount / apt_price$excluUseAr) * 3.3), 0)

head(apt_price$py, 2)

min(apt_price$floor)
apt_price$floor <- apt_price$floor %>% as.numeric() %>% abs()

min(apt_price$floor)

apt_price$cnt <- 1
head(apt_price, 2)


library(withr)

apt_price <- apt_price %>% select(dealDate, dealym, dealYear, dealMonth, dealDay, code, addr_1, aptNm, juso_jibun, dealAmount, buildYear, excluUseAr, floor, py, cnt)

head(apt_price, 2)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

path <- "./data/2021/preprocess"
if (!dir.exists(path)) {
  dir.create(path)
}else {
  cat("경로가 이미 존재합니다.", "\n\n")
}

rdata_path <- "./data/2021/preprocess/preprocess.rdata"

if (!file.exists(rdata_path)) {
  save(apt_price, file = rdata_path)
}else {
  cat("파일이 이미 존재합니다.", "\n\n")
}

csv_path <- "./data/2021/preprocess/preprocess.csv"

if (!file.exists(csv_path)) {
  write.csv(csv_path, "./data/2021/preprocess/preprocess.csv")
}else {
  cat("파일이 이미 존재합니다.", "\n\n")
}
