# Setting Options & Loading Packages --------------

options(scipen = 100, digits = 5)

#패키지 불러오기
library(tidyverse)
library(lubridate) 
library(readxl)    
library(openxlsx)
library(showtext)
library(dlookr)


font_add_google("Nanum Gothic", "nanumgothic")
theme_set(theme_bw(base_family = "nanumgothic"))

#데이터 불러오기
data_path <- "C:/Users/user/Documents/intern"
data_nm <- "220913_sample_data.xlsx"
data.raw <- read_excel(file.path(data_path, data_nm))


#데이터확인 ----

## 데이터 진단 ----
diagnose(data.raw)

### 형 변환 예시
# data$User_Score <- as.integer(data$User_Score)
# data.raw[sapply(data.raw, is.character)] <- lapply(data.raw[sapply(data.raw, is.character)], as.factor)

###수치형 데이터 진단
diagnose_numeric(data.raw)
###범주형 데이터 진단
diagnose_category(data.raw)
###이상치 데이터 진단
diagnose_outier(data.raw)


#데이터 탐색----

##기술통계량 확인하기
describe(data.raw)

###특정 변수를 group 하여 기술통계량 확인하기
data.raw %>%
  group_by(계정) %>% 
  describe(GP, Income) 

##정규성 테스트----
normality(data.raw)

##정규성 시각화----
plot_normality(data.raw, 선정연도,결성연도)

plot_normality(data.raw, 선정연도,존속기간,투자기간)



##상관계수 계산----
correlate(data.raw)
data.raw %>% 
  correlate() %>% 
  plot()


## 대상변수 기반 EDA----

### 예측변수 설정 (계정 변수: 범주형)
categ <- target_by(data.raw, 계정)

diagnose_report(Carseats, output_format = "html")

data.raw %>% distinct()


