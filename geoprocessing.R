setwd(dirname(rstudioapi::getSourceEditorContext()$path))
load("./data/2021/preprocess/preprocess.rdata")

# apt_juso 데이터 프레임 생성 (중복 제거 후 컬럼명 설정)
apt_juso <- data.frame(apt_juso = unique(apt_price$juso_jibun))
head(apt_juso, 2)

add_list <- list()

cnt <- 0

kakao_key <- "0c924b4a9109b1b70fd86688a603dd67"

library(httr)
library(RJSONIO)
library(data.table)
library(dplyr)

max_retries <- 3  # Set a maximum number of retries

for (i in seq_len(nrow(apt_juso))) {
  tryCatch(
  {
    retries <- 0  # Track retry count

    repeat {
      lon_lat <- GET(
        url = 'https://dapi.kakao.com/v2/local/search/address.json',
        query = list(query = apt_juso[i,]),
        add_headers(Authorization = paste0("KakaoAK ", kakao_key))
      )

      # Check if the status is 200 OK
      if (status_code(lon_lat) == 200) {
        coordxy <- lon_lat %>% content(as = 'parsed')
        x_coord <- coordxy$documents[[1]]$address$x
        y_coord <- coordxy$documents[[1]]$address$y
        cnt <- cnt + 1
        add_list[[cnt]] <- data.table(
          apt_juso = apt_juso[i, 1],
          coord_x = x_coord,
          coord_y = y_coord
        )
        message <- paste0("[", i, "/", nrow(apt_juso), "]번째 (", round(i / nrow(apt_juso) * 100, 2), " %) [", apt_juso[i,], "] 지오 코딩 중입니다.: X= ", add_list[[cnt]]$coord_x, " / Y=", add_list[[cnt]]$coord_y)
        cat(message, "\n\n")
        break  # Exit the repeat loop on success
      } else if (status_code(lon_lat) == 429 && retries < max_retries) {
        cat("Rate limit exceeded, retrying...\n")
        Sys.sleep(3 * (2 ^ retries))  # Start with 3 seconds and increase exponentially
        retries <- retries + 1
      } else {
        cat("Status not OK for ", apt_juso[i,], ": ", status_code(lon_lat), "\n")
        break
      }
    }
  }, error = function(e) {
      cat("ERROR at index ", i, " for address ", apt_juso[i,], ": ", status_code(lon_lat), "\n")
    }
  )
}


# 리스트를 데이터 프레임으로 변환하여 결합
juso_geocoding <- rbindlist(add_list, fill = TRUE)  # fill = TRUE로 누락된 열을 NA로 채움


# 이후 처리 동일
juso_geocoding$coord_x <- as.numeric(juso_geocoding$coord_x)
juso_geocoding$coord_y <- as.numeric(juso_geocoding$coord_y)
juso_geocoding <- na.omit(juso_geocoding)

# 저장 절차
if (!dir.exists("./data/2021/geocoding")) {
  dir.create("./data/2021/geocoding", showWarnings = FALSE)
}else{
  cat("디렉토리 존재함...\n")  # 저장 중 메시지 출력
}

if (!file.exists("./data/2021/geocoding/juso_geocoding.rdata")) {
  save(juso_geocoding, file = "./data/2021/geocoding/juso_geocoding.rdata")
}else{
  cat("파일 존재함...\n")  # 저장 중 메시지 출력
}

# CSV 파일 저장
if (!file.exists("./data/2021/geocoding/juso_geocoding.csv")) {
  cat("저장 중...\n")  # 저장 중 메시지 출력
  tryCatch({
    write.csv(juso_geocoding, "./data/2021/geocoding/juso_geocoding.csv", row.names = FALSE)
    cat("저장 완료\n")  # 저장 완료 메시지 출력
  }, error = function(e) {
    cat("CSV 파일 저장 중 오류 발생:", e$message, "\n")
  })
} else {
  cat("파일이 이미 존재합니다. 저장 완료된 상태입니다.\n")
}