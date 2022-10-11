# 옵션 & 패키지 세팅----

options(scipen = 100, digits = 5)
graphics.off()          # clear all graphs
rm(list = ls())         # remove all files from your workspace
gc(reset = TRUE)

library(tidyverse)
library(lubridate)   # 날짜데이터 다루기
library(readxl)      # 엑셀파일 불러오기
library(openxlsx)    # 엑셀파일로 내보내기
library(showtext)    # 한글폰트
library(dlookr)


font_add_google("Nanum Gothic", "nanumgothic")
theme_set(theme_bw(base_family = "nanumgothic"))  # 그림 테마설정


# 1) 파일 불러오기----

#경로설정
data_path <- "C:/Users/user/Documents/intern/벤치마크"
data_nm <- "벤치마크data.xlsx"
#파일불러오기
data1<- read_excel(file.path(data_path, data_nm))
data1

#데이터확인
glimpse(data1)
diagnose(data1)


data2_nm <- "자조합별운용현황data.xlsx"
data2 <- read_excel(file.path(data_path, data2_nm))
data2


glimpse(data2)
diagnose(data2)


data3_nm <- "가치평가data.xlsx"
data3 <- read_excel(file.path(data_path, data3_nm))
data3

glimpse(data3)
diagnose(data3)

#엑셀파일명설정
workb <- createWorkbook("벤치마크_최종.xlsx")
workb

# data1)벤치마크 데이터 ----

# - 구분 열에서 첫 8개 행 가치평가 잔여자산(평가금액-배분총액) > 기초(벤치마크)

data1[1:8,]<-data1[1:8,] %>% mutate(구분="기초(벤치마크)")


# 설립출자 > 기초(벤치마크)
# 가치평가 잔여자산(평가금액-배분총액)  > 기말(벤치마크)
# 중간배분 > 배분(벤치마크)

#변경 전 데이터 확인
data1 %>% group_by(구분) %>% summarize(n())

data1<- data1 %>% mutate(구분=ifelse(구분=="설립출자","기초(벤치마크)",
                              ifelse(구분=="가치평가 잔여자산(평가금액-배분총액)","기말(벤치마크)",
                              ifelse(구분=="중간배분","배분(벤치마크)",구분))))
#변경 후 데이터 확인
data1 %>% group_by(구분) %>% summarize(n())


# - 같은 이름의 자펀드를 기초합, 중간배분합, 기말합 해서 다음과 같은 피벗테이블 작성
# 
# 자펀드 // 기초(벤치마크) // 배분(벤치마크) // 기말(벤치마크).

df<- data1 %>% group_by(자펀드) %>% summarize(구분, 거래금액)



data11 <-data1 %>% group_by(자펀드) %>% summarize(기초_벤치마크=sum(ifelse(구분=="기초(벤치마크)",거래금액,0)),
                                             배분_벤치마크=sum(ifelse(구분=="배분(벤치마크)",거래금액,0)),
                                             기말_벤치마크=sum(ifelse(구분=="기말(벤치마크)",거래금액,0)))



# 
# data2) 자조합별운용현황 데이터 ----
# select 조합, 조합ID
# rename(자펀드=조합, 조합코드 = 조합ID)


data2 <-data2 %>% rename ("자펀드"="조합",
                          "조합코드"="조합ID")

# 이후 data1과 left_join

join <-left_join(
  data11,
  data2,
  by = "자펀드"
)


# data3) 가치평가 데이터 ----
# 조합코드 기준으로 data1과 left_join 

join <-left_join(
  join,
  data3,
  by = "조합코드"
)

#컬럼명 괄호 오류로 열 이름 변경

join <- join %>% rename ("기초_가치평가"="기초(가치평가)",
                         "배분_가치평가"="배분(가치평가)",
                         "기말_가치평가"="기말(가치평가)")

#배분(가치평가) (na값,-값 처리) 처리
join <- join %>% mutate(배분_가치평가= ifelse((is.na(배분_가치평가)||배분_가치평가=="-"),0,배분_가치평가))

# mutate
# 기초 차이 열 = 기초(벤치마크)+기초(가치평가)
# 배분 차이 열 = 배분(벤치마크)-배분(가치평가)
# 기말 차이 열 = 기말(벤치마크)-기말(가치평가)


join <-join %>% mutate(기초차이=as.numeric(기초_벤치마크)+as.numeric(기초_가치평가),
                       배분차이=as.numeric(배분_벤치마크)- as.numeric(배분_가치평가),
                       기말차이=as.numeric(기말_벤치마크)- as.numeric(기말_가치평가))

#데이터 확인 결과 한 행 전체가 na값으로 존재 -> 삭제처리
glimpse(join)
diagnose(join)

join[is.na(join$자펀드),]

join <- join[!is.na(join$자펀드),]

glimpse(join)
diagnose(join)



addWorksheet(workb, "전체data")
writeDataTable(workb,"전체data",join)


#자펀드, 조합코드, 벤치마크 3열 , 가치평가 3열, 차이 3열 총 11개 컬럼 선택


join <- join %>% select(자펀드,조합코드,기초_벤치마크,배분_벤치마크,기말_벤치마크,기초_가치평가,배분_가치평가,기말_가치평가,기초차이,배분차이,기말차이)



addWorksheet(workb, "최종data")
writeDataTable(workb,"최종data",join)

saveWorkbook(workb, file="./벤치마크_최종.xlsx")
