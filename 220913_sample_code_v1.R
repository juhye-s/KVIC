#===============================================================================================#
#  Setting Options & Loading Packages -----------------------------------------------------------
#===============================================================================================#

# Ref. Changing from exponential to numeric notation -------------------------------------------#
# Global: options(scipen = 100)   vs  -100                                                      #
# Local: format(df$var,scientific = F) vs T                                                     #
#===============================================================================================#
options(scipen = 100, digits = 5)
graphics.off()          # clear all graphs
rm(list = ls())         # remove all files from your workspace


# for Manipulating Data 
library(tidyverse)

library(lubridate)   # 날짜데이터 다루기
library(readxl)      # 엑셀파일 불러오기
library(openxlsx)    # 엑셀파일로 내보내기
# library(writexl)     # 엑셀파일로 내보내기

library(showtext)    # 한글폰트

# Ref. 그림 테마 추천 --------------------------------------------------------------------------#
# 1) theme_linedraw()                                                                           #
# 2) theme_classic()                                                                            #
#-----------------------------------------------------------------------------------------------#
font_add_google("Nanum Gothic", "nanumgothic")
theme_set(theme_bw(base_family = "nanumgothic"))  # 그림 테마설정



# Tips_1. to set up the settings  --------------------------------------------------------------#
# rm(list = ls())        # remove objects                                                       #
#gc(reset = TRUE)       # reset the memory                                                     #
#-----------------------------------------------------------------------------------------------#

# Tips_2. Copy & Paste -------------------------------------------------------------------------#
# tmp <- read.table("clipboard", header = TRUE, sep = "\t")      # 클립보드 자료 복사 붙여넣기  #
# write.table(tmp, "clipboard", row.names = FALSE, sep = "\t")   # R데이터 클립보드에 복사하기  #
# clipr::write_clip(tmp)                                                                        #
#-----------------------------------------------------------------------------------------------#



#===============================================================================================#
# I. Data Import  -------------------------------------------------------------------------------
#===============================================================================================#

## 1. Path & Names ==============================================================================
# Ref. 작업경로 및 경로안 파일 확인 ------------------------------------------------------------#
# getwd()      # 현재 작업경로 확인                                                             #
# list.files() # 현재 작업경로 안의 파일                                                        #
#-----------------------------------------------------------------------------------------------#

## 1) File Paths
data_path <- "C:/Users/user/Documents/intern"

# Ref. 작업경로 set up -------------------------------------------------------------------------#
# setwd(dir = work_path)                                                                        #
#-----------------------------------------------------------------------------------------------#

## 2) File Names
data_nm <- "220913_sample_data.xlsx"

## 3) Column Name & Type
# col_nm1 <- c("조합ID", "납입방식", "조합구분", "청산여부", "GPID", "GP구분",	
#              "선정연도", "결성연도", "만기연도", "차수", "계정",
#              "조합결성일", "투자완료일", "투자완료여부", "만기예정일", "조합청산일", "존속기간", "투자기간")
# 
# col_ty1 <- c("text", "text", "text", "text", "text", "text",
#              "numeric", "numeric", "numeric", "numeric", "text",
#              "date", "date", "text", "date", "date", "numeric", "numeric")


## 2. Importing Data ============================================================================
# # check the encoding type of data
# guess_encoding(file = file.path(raw_path, mas_fee.nm))

# importing the data
# data.raw <- read_excel(file.path(data_path, data_nm), skip = 1, col_names = col_nm1, col_types = col_ty1)
data.raw <- read_excel(file.path(data_path, data_nm))

gc(reset = TRUE)

workb <- createWorkbook("sample_data_EDA")
workb




#===============================================================================================#
# II. Data Transformation  ----------------------------------------------------------------------
#===============================================================================================#

# 9/13 할거 ----
# 1. na값이 있는 행들의 특징 찾기
# 2. GP별 투자 특징(평균투자기간, 계정별 횟수,차수(최빈값), 납입방식(최빈값) 등)
# 3. 계정별 특징(존속기간(평균), 차수(최빈값), 납입방식(최빈값) 등)
# 4. 연도-월-일 데이터를 월 또는 일만 추출해서 새로운 열 생성하기 -> 년도별 월별 투자횟수 그래프 시각화

##1.NA값 탐색

data.raw %>% names()

# column=list("조합ID","납입방식","조합구분","청산여부","GPID","GP구분","선정연도","결성연도","만기연도","차수","계정" ,"조합결성일" ,"투자완료일","투자완료여부", "만기예정일","조합청산일","존속기간" ,"투자기간")
# 
# for( i in column){
#   print(sum(is.na(data.raw$i)))
# }


#컬럼별 결측치값 확인
colSums(is.na(data.raw))

#결측치 행 추출
na.data <- data.raw[is.na(data.raw$조합청산일),]
na.data


# write.xlsx(na.data,"./sample_data_EDA.xlsx",sheetName="NA값 특징", 
#            col.names = TRUE, row.names = TRUE, append = TRUE)

addWorksheet(workb, "NA값 특징")
writeDataTable(workb,"NA값 특징",na.data)

##GP별 투자 특징

data.raw %>% distinct(GP구분)

# data.raw %>% group_by(GP구분) %>% summarise(n= n(),평균_존속기간= mean(존속기간)) -> gpgroup
# Mode(gpgroup$납입방식)

#최빈값구하기

getmode <- function(v) {
  uniqv = unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

# library(DescTools)
# 
# Mode(data.raw$GP구분)


ggplot2


data.raw %>% group_by(GP구분,계정) %>% summarise(n= n(), 평균_존속기간= mean(존속기간),평균_투자기간= mean(투자기간),납입방식=getmode(납입방식),차수=getmode(차수),청산여부=getmode(청산여부))%>% print(n = 100) -> GPgroup

GPgroup
# LLC<-filter(data.raw,GP구분=="LLC")
# Mode(LLC$납입방식)
# Mode(LLC$차수)
# 
# 창투사<-filter(data.raw,GP구분=="창투사")
# Mode(창투사$납입방식)
# Mode(창투사$차수)
# 
# 기타운용사<-filter(data.raw,GP구분=="기타운용사")
# Mode(기타운용사$납입방식)
# Mode(기타운용사$차수)
# 
# 신기술사<-filter(data.raw,GP구분=="신기술사")
# Mode(신기술사$납입방식)
# Mode(신기술사$차수)
# 


# write.xlsx(GPgroup,"./sample_data_EDA.xlsx", sheetName="GP별 투자 특징", append=TRUE)

addWorksheet(workb, "GP별 투자 특징")
writeDataTable(workb,"GP별 투자 특징",GPgroup)


##계정별 특징

data.raw %>% group_by(계정) %>% summarise(n= n(),평균_존속기간= mean(존속기간),평균_투자기간= mean(투자기간), 
                                        납입방식=getmode(납입방식),차수=getmode(차수),청산여부=getmode (청산여부)) %>% print(n = 100)->account

addWorksheet(workb, "계정별 특징")
writeDataTable(workb,"계정별 특징",account)

##연도별 특징 

dt<- data.raw
dt

dt$조합결성일_년도 <- as.character(year(dt$조합결성일))
dt$조합결성일_월   <- as.character(month(dt$조합결성일))
dt$조합결성일_일   <- as.character(day(dt$조합결성일))
dt$조합결성일_요일 <- as.character(wday(dt$조합결성일, label=T))

dt$투자완료일_년도 <-as.character(year(dt$투자완료일))
dt$투자완료일_월   <-as.character(month(dt$투자완료일))
dt$투자완료일_일   <-as.character(day(dt$투자완료일))
dt$투자완료일_요일 <-as.character(wday(dt$투자완료일, label=T))


dt.data <- dt %>% 
  group_by(조합결성일_년도) %>% 
  summarise(n = n(),
            주GP = mode(GP구분),
            계정 = mode(계정),
            조합결성_월 =  mode(조합결성일_월),
            조합결성_요일 = mode(조합결성일_요일),
            평균_존속기간 = mean(존속기간),
            평균_투자기간 = mean(투자기간),
            납입방식=mode(납입방식),
            차수=mode(차수),
            청산여부=mode(청산여부)) %>% 
  ungroup() 

dt.data

#데이터 클립보드에 복사하기
clipr::write_clip(dt.data) 

#클립보드에 복사한 데이터 삽입하기
tmp <- read.table("clipboard", header = TRUE, sep = "\t")

dt.data %>%
  ggplot(aes(x = 조합결성일_년도, y = 평균_존속기간)) +
  geom_point()

addWorksheet(workb, "기간별 특징")
writeDataTable(workb,"기간별 특징", dt.data)


saveWorkbook(workb, file="sample_data_EDA.xlsx")

dt%>% group_by(조합결성일_년도,계정) %>% summarise(도수= n(),  상대도수=round(n()/12,3))%>% print(n = 100) ->df2
df2


pivot_longer()




# 시각화(년도별 계정 비율, 계정별 납입방식 비율,년도별 GP 비율, 계정별 투자기간)

##계정별 투자기간 시각화(막대그래프)

dt %>% group_by(계정) %>% summarise(n=n(),평균_투자기간 = mean(투자기간))%>% print(n = 100) ->dt2

dt2

ggplot(dt2,
       aes(x=계정,y=평균_투자기간))+
  geom_bar(stat='identity')

##계정별 납입방식 비율 시각화(막대그래프)

dt %>% group_by(계정) %>% summarise(납입방식) %>% print(n = 100) ->dt3
dt3

ggplot(dt3,
       aes(x=계정,fill=납입방식))+
  geom_bar()

##년도별 계정 비율, GP비율

dt %>% group_by(조합결성일_년도) %>% summarise(계정, GP구분) %>% print(n = 100) ->dt4
dt4

dt  %>% select(조합결성일_년도,계정, GP구분)%>% summarise(조합결성일_년도,계정, GP구분,n=n()) %>%  print(n = 100)->dt4
dt4

ggplot(dt4,
       aes(x=조합결성일_년도,fill=계정))+
  geom_bar()

ggplot(dt4,
       aes(x=조합결성일_년도,fill=GP구분))+
  geom_bar()

##GP별 투자 계정 비율

dt %>% group_by(GP구분) %>%  summarise(계정) %>% print(n = 100) ->dt5

ggplot(dt5,
       aes(x=GP구분,fill=계정))+
  geom_bar()

##월별 조합결성횟수

dt %>% group_by(조합결성일_월) %>%  summarise(n=n()) %>% print(n = 100) ->dt6

xat=seq(1,12,by=1)

ggplot(data=dt6, aes(side=1,at=xat,x=조합결성일_월, y=n, group=1)) +
  geom_line()+
  geom_point() +
  geom_text(aes(label = n), vjust = -0.8, size = 5)


##월별 계정 투자 회수

dt %>% group_by(조합결성일_년도,계정) %>%  summarise(n=n()) %>% print(n = 100)->dt7

dt7 %>% ggplot(aes(x= 조합결성일_년도, y = n)) +
    geom_line(aes(group = 계정, color = 계정))



##SmartEDA 패키지 사용해보기

library(SmartEDA)

##데이터 요약
### type 1-> 전체 샘플사이즈, 열의 개수, 자료형별 변수 개수 , 결측치를 포함하지않는 변수 , 결측치 범위에 따른 변수 개수
ExpData(data.raw, type = 1)
### type 2-> 각각의 변수에 따른 변수타입, 샘플 개수, 결측치수, 결측치의 비율, 유일값의 개수
ExpData(data.raw, type = 2)
###type 2의 결과에 합계, 평균, 중앙값, 분산 등을 같이 나타내는 코드
ExpData(data.raw, type = 2, fun = c('sum', 'mean', 'sd'))
###수치형 변수 요약
ExpNumStat(data.raw)

###변수의 그룹에 따라 EDA을 보여줌 (by=> A:전체,G:그룹,GA:전체와 그룹화)
ExpNumStat(data.raw, by = 'GA', gp = '계정')

##수치형 변수에 대한 밀도분포 함수를 볼수있다.(target 설정 가능: 수치형=> 산점도, 범주형=> 박스 플롯)

data.raw %>% ExpNumViz()

##범주형 변수 요약: 범주형 변수를 자동으로 선택하여 변수에 대한 빈도나 테이블을 제공
ExpCTable(data.raw)

### 타겟 설정(팩터형 변수라면 해당 펙터 값에 따른 빈도와 분포율 제공,수치형이라면 해당 수치형 변수를 구간으로 그룹화하여 해당 구간에 나타난 빈도와 분포율을 출력)
ExpCTable(df_dropout, Target = '학교급', per = T)




write_csv(file.path(data_path, "xxx.csv"))
write_rds()


