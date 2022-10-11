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


font_add_google("Nanum Gothic", "nanumgothic")
theme_set(theme_bw(base_family = "nanumgothic"))  # 그림 테마설정


# 1) 파일 불러오기----
data_path <- "C:/Users/user/Documents/intern"

data_nm <- "220913_sample_data.xlsx"

data1.raw <- read_excel(file.path(data_path, data_nm))

data1.raw

data2_nm <- "220404_증권시장지수.xlsx"

data2.raw <- read_excel(file.path(data_path, data2_nm))

data2.raw

workb <- createWorkbook("sample_data_시장수익률")
workb

# 2) 데이터 합치기----

## 조합결성일- 날짜

### merge 활용
## 조합결성일- 날짜

df <- merge(data1.raw,data2.raw, by.x="조합결성일", by.y="날짜")

df<- df %>% rename ("결성_KOSPI"="KOSPI",
                    "결성_KOSDAQ"="KOSDAQ")

df
## 조합청산일 - 날짜


df2 <- merge(df,data2.raw, by.x="조합청산일", by.y="날짜",all.x = TRUE)

df2<- df2 %>% rename ("청산_KOSPI"="KOSPI",
                      "청산_KOSDAQ"="KOSDAQ")

df2


# ## inner_join 활용
# data <-data1.raw %>%  inner_join(data2.raw,
#                            by = c('조합결성일' = '날짜')) %>% print(n = 100)
# data
# 
# glimpse(data)
# summary(data)


# 3) 기간별 수익률 구하기----

## 결성-청산 수익률 (청산이 NA값인 경우는 제외)

fund1<- df2 %>% mutate(조합별_KOSPI수익률=(청산_KOSPI/결성_KOSPI-1)*100,
                      조합별_KOSDAQ수익률=(청산_KOSDAQ/결성_KOSDAQ-1)*100)

fund1
addWorksheet(workb, "조합별 수익률")
writeDataTable(workb,"조합별 수익률",fund1)

fund<-fund1

## 결성일로부터 n개월(3,6,12) 전부터 결성일까지의 수익률

as.Date(fund$조합결성일,format = "%Y-%m-%d")

is.Date(fund$조합결성일)
fund$조합결성일

threeM <-fund$조합결성일 + months(-3)
sixM   <-fund$조합결성일 + months(-6)
oneY   <-fund$조합결성일 + years(-1)


fund$bf3=threeM
fund$bf6=sixM
fund$bf12=oneY


fund<- merge(fund,data2.raw, by.x="bf3", by.y="날짜", all.x = TRUE)
fund<-fund %>% rename ("bf3_KOSPI"="KOSPI",
                 "bf3_KOSDAQ"="KOSDAQ")

fund <- merge(fund,data2.raw, by.x="bf6", by.y="날짜",all.x = TRUE)
fund<-fund %>% rename ("bf6_KOSPI"="KOSPI",
                 "bf6_KOSDAQ"="KOSDAQ")
fund <- merge(fund,data2.raw, by.x="bf12", by.y="날짜",all.x = TRUE)
fund<-fund %>% rename ("bf12_KOSPI"="KOSPI",
                 "bf12_KOSDAQ"="KOSDAQ")

fund<- fund %>% mutate(bf3_KOSPI수익률= (결성_KOSPI/bf3_KOSPI-1)*100,
                       bf3_KOSDAQ수익률=(결성_KOSDAQ/bf3_KOSDAQ-1)*100,
                       bf6_KOSPI수익률= (결성_KOSPI/bf6_KOSPI-1)*100,
                       bf6_KOSDAQ수익률=(결성_KOSDAQ/bf6_KOSDAQ-1)*100,
                       bf12_KOSPI수익률= (결성_KOSPI/bf12_KOSPI-1)*100,
                       bf12_KOSDAQ수익률=(결성_KOSDAQ/bf12_KOSDAQ-1)*100)


addWorksheet(workb, "특정기간별 시장 수익률")
writeDataTable(workb,"특정기간별 시장 수익률",fund)

## 결성연도별 코스피, 코스닥 수익률

data2.raw <-data2.raw %>% mutate(결성연도=year(날짜))
max_time <- data2.raw %>% group_by(결성연도) %>% slice(which.max(날짜))
min_time <- data2.raw %>% group_by(결성연도) %>% slice(which.min(날짜))

max_time<- max_time %>% rename ("최대_KOSPI"="KOSPI",
                                "최대_KOSDAQ"="KOSDAQ",
                                "최대날짜"="날짜")
min_time<- min_time %>% rename ("최소_KOSPI"="KOSPI",
                                "최소_KOSDAQ"="KOSDAQ",
                                "최소날짜"="날짜")

minmax <- merge(max_time,min_time)
minmax <- minmax %>% mutate(연도별_KOSPI수익률=(최대_KOSPI/최소_KOSPI-1)*100,
                            연도별_KOSDAQ수익률=(최대_KOSDAQ/최소_KOSDAQ-1)*100)

addWorksheet(workb, "연도별 시장 수익률")
writeDataTable(workb,"연도별 시장 수익률",minmax)


bd <- merge(fund,minmax)



saveWorkbook(workb, file="./sample_data_시장수익률.xlsx")

# 4) 시각화 ----

##연도별 코스피, 코스닥 수익률

ggplot(minmax,
       aes(x=결성연도,y=연도별_KOSPI수익률,fill=결성연도))+
  geom_bar(stat='identity')


ggplot(minmax,
       aes(x=결성연도,y=연도별_KOSDAQ수익률,fill=결성연도))+
  geom_bar(stat='identity')

ggplot(data=minmax, aes(side=1, x=결성연도, y=연도별_KOSDAQ수익률, group=1)) +
  geom_line()+
  geom_point() +
  geom_text(aes(label =round(연도별_KOSDAQ수익률,3) ), vjust = -0.8, size = 5)



ggplot(minmax, aes(x = 결성연도)) + 
  geom_line(aes(y = 연도별_KOSPI수익률, color = '연도별_KOSPI수익률')) + 
  geom_line(aes(y = 연도별_KOSDAQ수익률, color = '연도별_KOSDAQ수익률'))

##계정별 평균 코스피, 코스닥 수익률


fund<-fund %>% mutate(조합별_KOSPI수익률= ifelse(is.na(조합별_KOSPI수익률), (bf3_KOSPI수익률+bf6_KOSPI수익률+bf12_KOSPI수익률)/3, 조합별_KOSPI수익률))
fund<-fund %>% mutate(조합별_KOSDAQ수익률= ifelse(is.na(조합별_KOSDAQ수익률), (bf3_KOSDAQ수익률+bf6_KOSDAQ수익률+bf12_KOSDAQ수익률)/3, 조합별_KOSDAQ수익률))


fund<- fund %>% mutate(평균KOSPI수익률=
                         (조합별_KOSPI수익률+bf3_KOSPI수익률+bf6_KOSPI수익률+bf12_KOSPI수익률)/4,
                       평균KOSDAQ수익률=
                         (조합별_KOSDAQ수익률+bf3_KOSDAQ수익률+bf6_KOSDAQ수익률+bf12_KOSDAQ수익률)/4)


fund %>% group_by(계정) %>% summarise(평균_KOSPI수익률=mean(조합별_KOSPI수익률,na.rm = TRUE),평균_KOSDAQ수익률=mean(조합별_KOSDAQ수익률,na.rm = TRUE)) %>% print(n = 100)

dtt<- fund %>% group_by(계정) %>% summarise(평균_KOSPI수익률=mean(평균KOSPI수익률,na.rm = TRUE) ,평균_KOSDAQ수익률=mean(평균KOSDAQ수익률,na.rm = TRUE)) %>% print(n = 100)

ggplot(dtt,
       aes(x=계정,y=평균_KOSPI수익률,fill=계정))+
  geom_bar(stat='identity')

```jsx

##계정별 코스피, 코스닥 수익률

fund<-fund %>% mutate(조합별_KOSPI수익률= ifelse(is.na(조합별_KOSPI수익률), (bf3_KOSPI수익률+bf6_KOSPI수익률+bf12_KOSPI수익률)/3, 조합별_KOSPI수익률))
fund<-fund %>% mutate(조합별_KOSDAQ수익률= ifelse(is.na(조합별_KOSDAQ수익률), (bf3_KOSDAQ수익률+bf6_KOSDAQ수익률+bf12_KOSDAQ수익률)/3, 조합별_KOSDAQ수익률))

fund<- fund %>% mutate(평균KOSPI수익률=
                         (조합별_KOSPI수익률+bf3_KOSPI수익률+bf6_KOSPI수익률+bf12_KOSPI수익률)/4,
                       평균KOSDAQ수익률=
                         (조합별_KOSDAQ수익률+bf3_KOSDAQ수익률+bf6_KOSDAQ수익률+bf12_KOSDAQ수익률)/4)

fund %>% group_by(계정) %>% summarise(평균_KOSPI수익률=mean(조합별_KOSPI수익률,na.rm = TRUE),평균_KOSDAQ수익률=mean(조합별_KOSDAQ수익률,na.rm = TRUE)) %>% print(n = 100)

dtt<- fund %>% group_by(계정) %>% summarise(평균_KOSPI수익률=mean(평균KOSPI수익률,na.rm = TRUE) ,평균_KOSDAQ수익률=mean(평균KOSDAQ수익률,na.rm = TRUE)) %>% print(n = 100)

ggplot(dtt,
       aes(x=계정,y=평균_KOSPI수익률,fill=계정))+
  geom_bar(stat='identity')


## GP별 코스피, 코스닥 수익률

dt<-fund %>% group_by(GP구분) %>%  summarise(평균_KOSPI수익률=mean(조합별_KOSPI수익률,na.rm = TRUE),평균_KOSDAQ수익률=mean(조합별_KOSDAQ수익률,na.rm = TRUE)) %>% print(n = 100)

ggplot(dt,
       aes(x=GP구분,y=평균_KOSPI수익률,fill=GP구분))+
  geom_bar(stat='identity')

ggplot(dt,
       aes(x=GP구분,y=평균_KOSDAQ수익률,fill=GP구분))+
  geom_bar(stat='identity')



##수요일에 할것




