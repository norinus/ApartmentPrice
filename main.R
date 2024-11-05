install.packages("rstudioapi")   # rstudioapi 설치                         
setwd(dirname(rstudioapi::getSourceEditorContext()$path)) # 작업폴더 설정
getwd()   # 확인

# 데이터 로드 및 변환
loc <- read.csv("./sigun_code.csv", header = TRUE, fileEncoding = "UTF-8")
loc$code <- as.character(loc$code) # 행정구역명 문자 변환
head(loc, 2) # 확인

datelist <- seq(from = as.Date('2021-01-01'), # 시작
                to   = as.Date('2021-12-31'), # 종료
                by    = '1 month')            # 단위
datelist <- format(datelist, format = '%Y%m') # 형식변환(YYYY-MM-DD => YYYYMM)
datelist[1:3]          # 확인


# 인증키 및 URL 생성
service_key <- "kRabKsGy7tAGj4QAUALeERMUL51wHlWECAkNpryGNmFTKPJtEecz83pKP1LVwHz%2BzSYPv40izA5a5%2BGcXmPLaQ%3D%3D"

#---# [1단계: 요청목록 만들기]
url_list <- list() # 빈 리스트 만들기
cnt <-0	           # 반복문의 제어 변수 초깃값 설정

#---# [2단계: 요청목록 채우기]
for(i in seq_len(nrow(loc))){           # 외부반복: 25개 자치구
  for(j in seq_along(datelist)){  # 내부반복: 12개월
    cnt <- cnt + 1               # 반복누적 카운팅
    #---# 요청 목록 채우기 (25 X 12= 300)
    url_list[cnt] <- paste0("https://apis.data.go.kr/1613000/RTMSDataSvcAptTrade/getRTMSDataSvcAptTrade?",
                            "serviceKey=", service_key,  # 인증키
                            "&LAWD_CD=", loc[i,1],       # 지역코드
                            "&DEAL_YMD=", datelist[j],   # 수집월
                            "&numOfRows=", 1000)         # 한번에 가져올 최대 자료 수 
  }                                                      # (한달에 한 지역에 1,000건 이상 거래는 거의 없을테니 최대값 설정 / 미설정시 10건만 가져옴) 
  Sys.sleep(0.1)                                          # 0.1초간 멈춤
  msg <- paste0("[", i,"/",nrow(loc), "]  ", loc[i,3], " 의 크롤링 목록이 생성됨 => 총 [", cnt,"] 건") # 알림 메시지
  cat(msg, "\n\n") 
}

#---# [3단계: 요청 목록 동작 확인]

length(url_list)                # 요청목록 갯수 확인
browseURL(paste0(url_list[1]))  # 정상작동 확인(웹브라우저 실행)


#----------------
# 3-3 크롤링 실행
#----------------
#---# [1단계: 임시 저장 리스트 생성]

# 필요한 패키지 로드
library(XML)        # XML 데이터를 처리하기 위한 패키지
library(data.table) # 대용량 데이터 처리를 위한 패키지
library(stringr)    # 문자열 처리를 위한 패키지
library(xml2)       # XML 및 HTML 문서를 파싱하기 위한 패키지
library(dplyr)      # 데이터 조작을 위한 패키지


##raw_data <- list()        # xml 임시 저장소
##root_Node <- list()       # 거래내역 추출 임시 저장소
##total <- list()           # 거래내역 정리 임시 저장소

dir.create("data") # 새로운 폴더 만들기

#---# [2단계: URL 요청 - XML 응답]
# 데이터를 누적할 빈 리스트 생성
##xmlData_list <- list()

year_path <- NULL

for(i in seq_along(url_list)){

  # XML 파일 읽기
  doc <- read_xml(as.character(url_list[i]))

  # 모든 <item> 노드 선택
  items <- xml_find_all(doc, '//item')

  # <item> 노드가 없는 경우 메시지를 출력하고 다음 URL로 이동
  if (length(items) == 0) {
    cat("[", i,  "] 데이터가 없습니다. URL:", "\n\n")
    next  # 다음 반복으로 넘어감
  }

  # 각 <item> 노드의 데이터를 리스트로 추출
  item_list <- lapply(items, function(item){
    # <item>의 자식 노드들 추출
    children <- xml_children(item)
    # 자식 노드들의 이름과 텍스트 추출하여 리스트로 반환
    data <- setNames(xml_text(children), xml_name(children))
    return(data)
  })

  # 리스트를 데이터프레임으로 변환
  xmlData <- bind_rows(lapply(item_list, function(x) as.data.frame(t(x), stringsAsFactors=FALSE)))

  # xmlData 데이터 프레임의 컬럼명을 한글로 변경 및 추출 대상 처리 코드
  #xmlData <- xmlData %>%
  #  rename(
  #    아파트동명 = aptDong,
  #    아파트단지명 = aptNm,
  #    건축년도 = buildYear,
  #    거래주체정보_매수자(개인/법인/공공기관/기타) = buyerGbn,
  #    취소거래일 = cdealDay,
  #    취소거래유형 = cdealType,
  #    거래금액(만원) = dealAmount,
  #    계약일 = dealDay,
  #    계약월 = dealMonth,
  #    계약년도 = dealYear,
  #    거래구분(중개및직거래여부) = dealingGbn,
  #    중개사소재지 = estateAgentSggNm,
  #    전용면적 = excluUseAr,
  #    층 = floor,
  #    지번 = jibun,
  #    토지임대부구분 = landLeaseholdGbn,
  #    등기일자 = rgstDate,
  #    지역코드 = sggCd,
  #    거래주체정보(개인/법인/공공기관/기타) = slerGbn,
  #    법정동명 = umdNm
  #  )
  # 필요한 컬럼만 선택
  # 필요한 컬럼만 선택
  ##xmlData <- xmlData %>%select(아파트단지명,건축년도,거래금액,계약년도, 계약월, 계약일, 전용면적, 지역코드,지번, 법정동명)

  # 지역명 추출
  region_code <- regmatches(url_list[i], regexpr("(?<=LAWD_CD=)[^&]*", url_list[i], perl=TRUE))
  region_nm <- subset(loc, code == region_code)[, 4]

  # Ensure region_nm is a single value
  region_nm <- as.character(region_nm)
  if (length(region_nm) > 1) {
    region_nm <- region_nm[1]  # Or handle appropriately
  }

  # 연월(YYYYMM) 추출
  date <- gsub(".*DEAL_YMD=(\\d{6}).*", "\\1", url_list[i])

  # 연도 추출
  year <- substr(date, 1, 4)

  # 연도 폴더 생성 (없을 경우)
  year_dir <- paste0("./data/", year)
  year_path <-year_dir

  if (!dir.exists(year_dir)) {
    dir.create(year_dir)
  }

  # 저장위치 설정
  path <- paste0(year_dir, "/", region_nm, "_", date, ".csv")
  # 파일 존재 여부 확인 및 저장 결정
  if (!file.exists(path)) {
    # csv 저장
    write.csv(xmlData, path, row.names = FALSE)
    # 알림 메시지
    msg <- paste0("[", i, "/", length(url_list), "] 수집한 데이터를 [", path, "]에 저장 합니다.")
  } else {
    # 파일이 이미 존재할 경우 저장하지 않음
    msg <- paste0("[", i, "/", length(url_list), "] 파일 [", path, "] 이 이미 존재합니다. 저장을 건너뜁니다.")
  }

  cat(msg, "\n\n")
}

# 파일 목록을 가져옴
files <- dir(file.path(year_path))

# 파일 경로를 안전하게 생성
file_paths <- file.path(year_dir, files)

# 각 파일을 읽어서 데이터프레임으로 통합
apt_price <- ldply(as.list(file_paths), read.csv)

# 데이터 확인
tail(apt_price)

# 통합 디렉토리 생성
integrated_dir <- file.path(year_dir, "integrated/")
dir.create(integrated_dir, showWarnings = FALSE)

# 데이터 저장
save(apt_price, file = file.path(integrated_dir, "apt_price.rdata"))
write.csv(apt_price, file.path(integrated_dir, "apt_price.csv"))