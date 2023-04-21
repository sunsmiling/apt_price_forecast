rm(list=ls())
setwd("C:/Users/sunny/Dropbox/PC (2)/Desktop/ty/apt_price/apt_price_forecast/preprocessing")

##2023-04-21 Wrtie

library(dplyr)
library(stringr)
apt_detail <- read.csv("../data/apt_detail.csv")
colnames(apt_detail)
head(apt_detail)

###########################
# convenientFacility 처리 #
###########################
# Hospital ----------------------------------------------------------------
apt_detail$num_Hospital =  str_extract(apt_detail$convenientFacility, "병원\\((.*?)\\)") %>%
  str_remove("차량 10분") %>%
  str_extract("\\((.*)\\)") %>%
  str_extract_all("[가-힣]+") %>%
  sapply(length)

# number of park (is park) ------------------------------------------------
has_korean <- function(x) {
  str_detect(str_extract(x, "\\((.*?)\\)"), "\\p{Hangul}")
}

apt_detail$numPark = str_extract(apt_detail$convenientFacility, "공원\\((.*?)\\)") %>%
  str_extract("\\((.*)\\)")
apt_detail$isPark <- ifelse(has_korean(apt_detail$numPark),1,0)
apt_detail$isPark <- ifelse(is.na(apt_detail$isPark),0,1)

apt_detail = apt_detail %>% select(-convenientFacility)


############################################
# educationFacility 처리 (Elm/Mid/Hi_sch)  #
###########################################
apt_detail$num_Elm_sch = str_extract(apt_detail$educationFacility, "초등학교\\((.*?)\\)") %>%
  str_extract("\\((.*)\\)") %>%
  str_extract_all("[가-힣]+") %>%
  sapply(length)

apt_detail$num_Mid_sch = str_extract(apt_detail$educationFacility, "중학교\\((.*?)\\)") %>%
  str_extract("\\((.*)\\)") %>%
  str_extract_all("[가-힣]+") %>%
  sapply(length)

apt_detail$num_Hi_sch = str_extract(apt_detail$educationFacility, "고등학교\\((.*?)\\)") %>%
  str_extract("\\((.*)\\)") %>%
  str_extract_all("[가-힣]+") %>%
  sapply(length)
apt_detail = apt_detail %>% select(-educationFacility)
apt_detail %>% head()


############################################
# kaptdWtimesub처리
###########################################
apt_detail$kaptdWtimesub %>% table()
# 10~15분이내  15~20분이내    20분초과  5~10분이내     5분이내
# 30               7            2          67            60

apt_detail = apt_detail %>%
  mutate(Subway_new = ifelse(kaptdWtimesub  == "20분초과",
                             "15분초과",
                             ifelse(kaptdWtimesub == "15~20분이내",
                                    "15분초과",
                                    kaptdWtimesub ))) %>%
  mutate(Subway_new = factor(Subway_new,
                             levels=c("5분이내","5~10분이내","10~15분이내","15분초과")
                             )
         )
apt_detail = apt_detail %>% select(-kaptdWtimesub)

apt_detail$Subway_new %>% table()
# 5분이내  5~10분이내 10~15분이내    15분초과
# 60          67          30           9


############################################
# kaptdWtimebus처리
###########################################

apt_detail$kaptdWtimebus %>% table()
# 10~15분이내 15~20분이내  5~10분이내     5분이내
#     9           4          62          97

apt_detail = apt_detail %>%
  mutate(Bus_new = ifelse(kaptdWtimebus == "20분초과",
                          "10분초과",
                          ifelse(kaptdWtimebus== "15~20분이내",
                                 "10분초과",
                                 kaptdWtimebus))) %>%
  mutate(Bus_new = factor(Bus_new,
                          levels=c("5분이내","5~10분이내","10분초과")
                          )
         )
apt_detail = apt_detail %>% select(-kaptdWtimebus)
apt_detail$Bus_new %>% table()
# 5분이내 5~10분이내   10분초과
# 97         62          4


# BASE정보와 merge -----------------------------------------------------------
# apt_info
### key =  kaptCode
apt_base <- read.csv("../data/apt_base.csv")
colnames(apt_base)
dim(apt_base) #172  17
dim(apt_detail) #172  15

apt_info = merge(apt_base,apt_detail)
dim(apt_info) #172  30
head(apt_info)


####################
##### NA 처리 ######
####################
## Row 별 na
apt_info[rowSums(is.na(apt_info)) >= 5,]
# A10023348 개포자이프레지던스 사용일 20230228, 너무 최신이라 na가 많은 듯. 분석에서 제외!
apt_info = apt_info %>% filter(kaptCode != "A10023348")

apt_info[rowSums(is.na(apt_info)) >= 3,] #welfareFacility subwayStation numPark 등이 na

## column별 na
colSums(is.na(apt_info))[colSums(is.na(apt_info))>=1]
# codeAptNm        doroJuso welfareFacility   subwayStation      subwayLine         numPark      Subway_new
# 2               3               1              46               2              58               5
# Bus_new
# 9

# codeAptNm ---------------------------------------------------------------
apt_info$codeAptNm %>% table()
# 아파트 연립주택 주상복합
# 147        2       20
apt_info[which(is.na(apt_info$codeAptNm)),"codeAptNm"] <- rep("아파트",2)
# 개포상록스타힐스디 에이치포레센트아파트 : 모두 아파트
apt_info$codeAptNm %>% table()
# 아파트 연립주택 주상복합
# 149        2       20

# doroJuso ----------------------------------------------------------------
apt_info[which(is.na(apt_info$doroJuso)),"kaptAddr"]
# 서울특별시 강남구 도곡동 543-7 도곡1차아이파크 = 서울특별시 강남구 도곡로28길 8
# 서울특별시 강남구 도곡동 467-7 아카데미스위트 = 서울특별시 강남구 언주로30길 21 아카데미스위트
# 서울특별시 강남구 도곡동 895-8 도곡한신 = 서울특별시 강남구 논현로 205 도곡한신아파트
apt_info[which(is.na(apt_info$doroJuso)),"doroJuso"] <- c("서울특별시 강남구 도곡로28길 8",
                                                          "서울특별시 강남구 언주로30길 21 아카데미스위트",
                                                          "서울특별시 강남구 논현로 205 도곡한신아파트")

colSums(is.na(apt_info))[colSums(is.na(apt_info))>=1]
# 주차대수(지상) Na이면 0

# welfareFacility ---------------------------------------------------------
apt_info$welfareFacility %>% head() #분석에 필요없는 변수로 판단되어 데이터에서 제외
apt_info <- apt_info %>% select(-welfareFacility)

# subwayStation  ---------------------------------------------------------
apt_info[which(is.na(apt_info$subwayStation)),c("kaptName","subwayStation","Subway_new")]
#역 이름 없더라도 지하철까지 걸리는 시간에 대한 정보가 있으므로
# 분석에 필요없는 변수로 판단되어 데이터에서 제외
apt_info <- apt_info %>% select(-subwayStation)

# subwayLine  ---------------------------------------------------------
apt_info$subwayLine %>% table()
apt_info[which(is.na(apt_info$subwayLine)),c("kaptName","Subway_new","subwayLine")]
#           kaptName      Subway_new    subwayLine
#      강남센트럴아이파크 5~10분이내       <NA>  --> "2호선, 분당선"
# 강남신동아파밀리에1단지   15분초과       <NA> --> 아주 멀리 수서역 존재 "3호선"
apt_info[which(is.na(apt_info$subwayLine)),c("subwayLine")] <- c("2호선, 분당선","3호선")

# numPark  ---------------------------------------------------------
# 갯수보다는 존재 유무 isPark를 넣을 것이므로 변수 제외
apt_info <- apt_info %>% select(-numPark)
# Subway_new ---------------------------------------------------------
apt_info$Subway_new %>% table()
apt_info[which(is.na(apt_info$Subway_new)),c("kaptName","Subway_new","subwayLine")]
# kaptName Subway_new    subwayLine
# 9  개포래미안포레스트       <NA>         3호선 ==>"매봉역 14분"  ==> "10~15분이내"
# 49 청담대우유로카운티       <NA>         7호선 ==>"청담역 7분"  ==> "5~10분이내"
# 69       세곡푸르지오       <NA> 3호선, 분당선 ==> 수서역 걸어서 40분 ==> "15분초과"
# 72         래미안포레       <NA>         3호선 ==> 수서역 걸어서 20분 ==> "15분초과"
# 73     강남한양수자인       <NA>  3호선, 8호선 ==> 수서역 걸어서 20분 ==> "15분초과"
apt_info[which(is.na(apt_info$Subway_new)),"Subway_new"] <-c("10~15분이내", "5~10분이내",rep("15분초과",3))
# Bus_new ---------------------------------------------------------
apt_info$Bus_new %>% table()
apt_info[which(is.na(apt_info$Bus_new)),"kaptName"]
#[1] "동양파라곤"         "논현신동아"         "청담대우유로카운티" "삼성청담공원"       "도곡현대그린"
#[6] "도곡경남"           "도곡한신"           "논현동현"           "도곡현대"

### 네이버 지도앱 결과 모두 버스 5분이내 존재
apt_info[which(is.na(apt_info$Bus_new)),"Bus_new"] <- rep("5분이내",9)

# NA 전처리 끝 ---------------------------------------------------------
colSums(is.na(apt_info))
write.csv(apt_info,"../data/apt_info.csv", row.names = FALSE)
