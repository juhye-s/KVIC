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
library(tibble)
library(stringr)
library(dlookr)


font_add_google("Nanum Gothic", "nanumgothic")
theme_set(theme_bw(base_family = "nanumgothic"))  # 그림 테마설정


# 1) 파일 불러오기----
data_path <- "C:/Users/user/Documents/intern"

data_nm <- "기업가치평가data.xlsx"
data1.raw <- read_excel(file.path(data_path, data_nm))
data1.raw

data2_nm <- "상장data.xlsx"
data2.raw <- read_excel(file.path(data_path, data2_nm))
data2.raw

data3_nm <- "포트폴리오조회data.xlsx"
data3.raw <- read_excel(file.path(data_path, data3_nm))
data3.raw

# workb <- createWorkbook("sample_data_시장수익률")
# workb

## 데이터 확인

#행의 개수와 컬럼 개수 확인
glimpse(data1.raw)
#중복을 제외한 사업자등록번호 개수 파악
data1.raw %>% summarize(n_distinct(사업자등록번호))
#사업자등록번호와 기업가치 컬럼별 결측치 개수 파악
sum(is.na(data1.raw$사업자등록번호))
sum(is.na(data1.raw$기업가치))


glimpse(data2.raw)
data2.raw %>% summarize(n_distinct(사업자등록번호))
sum(is.na(data2.raw$사업자등록번호))
sum(is.na(data2.raw$등록일자))

glimpse(data3.raw)
data3.raw %>% summarize(n_distinct(사업자등록번호))
sum(is.na(data3.raw$사업자등록번호))

# 2) 데이터 중복값 처리 ----


## 기업가치평가 데이터 정리

#기업가치가 여러개인 사업자등록번호는 중위값으로 추출
data1 <- data1.raw %>% group_by(사업자등록번호) %>% summarise(기업가치=median(기업가치))


#추출 데이터 개수 확인
glimpse(data1)


## 상장 데이터 정리(사업자등록번호가 비어있지않고 시장구분이 1 또는 2인 데이터만 추출)

data2 <- data2.raw %>% filter(!is.na(사업자등록번호) & 시장구분 %in% c(1,2)) 

glimpse(data2)
sum(is.na(data2$사업자등록번호))
sum(is.na(data2$등록일자))
data2 %>% summarize(n_distinct(사업자등록번호))

# data2.raw %>% filter(!is.na(사업자등록번호) & 시장구분 %in% c(3,4)) 


##포트폴리오조회 데이터 정리

### 최빈값 도출함수
getmode <- function(v) {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# 컬럼명에 괄호표시 오류로 컬럼명 변환
data3.raw <-data3.raw %>% rename ("업종대분류"="업종대분류(VC기준)",
                    "업종분류"="업종분류(VC기준)")

#같은 사업자등록번호에 데이터가 여러개인경우 거래금액을 제외한 나머지컬럼은 최빈값으로 대체, 거래금액은 sum으로 사업자등록번호 별 거래금액총합을 계산

data3 <- data3.raw %>% group_by(사업자등록번호) %>% summarise(설립일자=getmode(설립일자), 국가=getmode(국가), 
                                                       표준산업분류코드명 =getmode(표준산업분류코드명),
                                                       업종대분류=getmode(업종대분류),
                                                       업종분류=getmode(업종분류), 
                                                       거래금액=sum(거래금액) )

data3



# 3) 데이터 합치기----

## 사업자 등록번호 기준 합치기 


df <-left_join(
  data1,
  data2,
  by = "사업자등록번호"
  )


glimpse(df)

df %>% summarize(n_distinct(사업자등록번호))
sum(is.na(df$사업자등록번호))

df <-left_join(
  df,
  data3,
  by = "사업자등록번호"
)


glimpse(df)
diagnose(df)

##등록일자 2022년 이전 데이터 삭제 && 국가 한국 필터링

df <- df %>% filter((!(등록일자<=20220101) | is.na(등록일자)) & 국가=="한국")

#엑셀로 저장
write.xlsx(df, sheetName="기업가치", file="최종data.xlsx")

