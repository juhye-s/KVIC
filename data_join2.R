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
data_path <- "C:/Users/user/Documents/intern"

data_nm <- "자조합별운용현황data.xlsx"
data1.raw<- read_excel(file.path(data_path, data_nm))
data1.raw

glimpse(data1.raw)
diagnose(data1.raw)


data2_nm <- "통계data.xlsx"
data2.raw <- read_excel(file.path(data_path, data2_nm))
data2.raw


glimpse(data2.raw)
diagnose(data2.raw)

# 
# data1)자조합별운용현황에서
# 
# rename 조합코드, 조합ID

data1.raw <-data1.raw %>% rename ("조합코드"="조합ID",
                                  "결성금액"="결성금액(약정)")


# select 조합코드, 조합구분, 조합, 계정, VC, 결성금액(약정), 총투자금액, 등록일

data1<- data1.raw %>% select(조합코드, 조합구분, 조합, 계정, VC, 결성금액, 총투자금액, 등록일, 투자기간, 조합상태)
data1 <-data1 %>% rename ("투자기간2"="투자기간")

# 소진율 column 만들어서
# 총투자금액/결성금액(약정) *100

data1<- data1 %>% mutate(총투자금액= ifelse(is.na(총투자금액),0,총투자금액))
# 
data1 <-data1 %>% mutate(소진율 =(총투자금액/결성금액)*100)


glimpse(data1)
diagnose(data1)
# 
# data2)통계data에서
# 
# 결성여부 열에서
# `결성실패`인 행 삭제

data2<-data2.raw %>% filter(!(결성여부=="결성실패"))

# data2<-data2.raw[!(결성여부=="결성실패"),]

# 
# 
# select 조합코드, 청산여부, 조합유형-세, 투자기간
# 
# 



data2 <-data2 %>% rename ("조합유형"="조합유형-세")

unique(data2$조합유형)
glimpse(data2)
diagnose(data2)

data2 <-data2 %>% select(조합코드, 청산여부,조합유형, 투자기간)

# data3) 조합코드 기준으로 left join(자조합별운용현황, 통계data)
# 


data3 <-left_join(
  data1,
  data2,
  by = "조합코드"
)


glimpse(data3)
diagnose(data3)

# data3중 청산여부 na값이 있는 경우 -> 자조합별운용현황 중 조합상태 값으로 대체
# data3중 투자기간 na값이 있는 경우 -> 자조합별운용현황 중 투자기간 값으로 대체
# 
# 
# df <- left_join(
#   data3 %>% filter(is.na(data3$청산여부)),
#   data1.raw %>% select(조합코드, 조합상태, 투자기간),
#   by = "조합코드")

data3 %>% filter(is.na(data3$청산여부))
data3 %>% filter(is.na(data3$투자기간))


data3<- data3 %>% mutate(청산여부= ifelse(is.na(청산여부),조합상태,청산여부),
                 투자기간= ifelse(is.na(투자기간),투자기간2,투자기간))


glimpse(data3)
diagnose(data3)


df<-data3 %>% filter(is.na(조합유형))
write.xlsx(df, sheetName="sheet1", file="조합유형Na.xlsx")
  
# 투자진행여부 column 만들어서
# 등록일+투자기간*365 <= 2022/06/30 이면 완료, 아니면 진행

data3$투자기간 <-as.numeric(data3$투자기간)

data3<- data3 %>% mutate(투자진행=투자기간*365)

data3$투자진행 <-as.Date(data3$투자진행,origin = data3$등록일)
 
data3 <- data3 %>% mutate(투자진행여부= ifelse(as.Date(투자진행,origin =등록일) <="2022-06-30","완료","진행"))
# 
# 최종포함여부 column 만들어서 다음과 같은 우선순위로 진행
# 
# (1)조합유형_세 = 자조합출자 인 경우
# 모펀드 제외(삭제)

# (2)청산여부 = 해산완료 인 경우
# 해산완료 제외(삭제)
# 
# (3)투자진행여부 = 완료 인 경우
# 투자기간만료(삭제)
# 
# (4)소진율 >= 70인 경우
# 소진율 70% 이상(삭제)


data3 <- data3 %>% mutate(최종포함여부= ifelse(조합유형=="자조합출자","모펀드 제외(삭제)", 
                                        ifelse(청산여부=="해산완료","해산완료 제외(삭제)",
                                        ifelse(투자진행여부=="완료","투자기간만료(삭제)",
                                        ifelse(소진율>=70,"소진율 70% 이상(삭제)","포함")))))

data3$투자기간2<-NULL
data3$조합상태<-NULL
                 

glimpse(data3)
diagnose(data3)


#확인하기

data3 %>% filter(조합유형=="자조합출자")
data3 %>% filter(청산여부=="해산완료")
data3 %>% filter(투자진행여부=="완료")
data3 %>% filter(is.na(최종포함여부))


#엑셀로 저장
write.xlsx(data3, sheetName="sheet1", file="최종data.xlsx")


##최종여부가 na값이 생기는이유 -> 조합유형이 na값인 경우 최종여부가 na값이 됨, 소진율이 na값인경우 (1개) -> 결성금액이 na값인 경우로 인해 발생


