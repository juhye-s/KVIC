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

data_path <- "C:/Users/user/Documents/intern/심층분석"

data_nm <- "221031_회수추정.xlsx"
data.raw<- read_excel(file.path(data_path, data_nm), sheet='Sheet3')
data.raw

data2.raw<- read_excel(file.path(data_path, data_nm), sheet='납입비율')
data2.raw

data3.raw<- read_excel(file.path(data_path, data_nm), sheet='회수율')
data3.raw

workb <- createWorkbook("심층분석.xlsx")

##결성연도 기준 left 조인하기 

#납입비율
data <- left_join(
  data.raw,
  data2.raw,
  by = "결성연도")

#회수율
data2 <- left_join(
  data.raw,
  data3.raw,
  by = "결성연도")

# 
# data3 <- left_join(
#   data2,
#   data)


##납입비율 계산
data <-data %>% mutate(예측년도= 결성연도 + 납입시점,
                       납입금액예측= 납입비율 * 모태약정액.x,
                       납입금액예측_누적= 납입비율_누적 * 모태약정액.x)

data <-data %>% mutate(예측년도= ifelse((예측년도 <= year(조합청산일))|is.na(조합청산일) ,예측년도,NA))



##회수비율 계산

data2 <-data2 %>% mutate(예측년도= 결성연도 + 배분시점,
                       배분금액예측= 회수율 * 모태약정액.x,
                       배분금액예측_누적= 회수율_누적 * 모태약정액.x)

data2 <-data2 %>% mutate(예측년도= ifelse((예측년도 <= year(조합청산일))|is.na(조합청산일) ,예측년도,NA))

data2$청산연도<- NULL



df <- data[!is.na(data$예측년도),]
df <- df %>% mutate(청산여부= ifelse((2019 <= year(조합결성일)),NA,청산여부))
df <- df[!is.na(df$청산여부),]

data1 <-df %>% group_by(예측년도) %>% summarize(연도별납입금액=sum(납입금액예측),연도별납입금액_누적=sum(납입금액예측_누적),연도별납입금액_평균=mean(납입금액예측),연도별납입비율_평균=mean(납입비율),개수=n(),
                                            모태약정액=mean(모태약정액.y))

df2 <- data2[!is.na(data2$예측년도),]
df2 <- df2 %>% mutate(청산여부= ifelse((2019 <= year(조합결성일)),NA,청산여부))
df2 <- df2[!is.na(df2$청산여부),]

data22 <-df2 %>% group_by(예측년도) %>% summarize(연도별배분금액=sum(배분금액예측),연도별배분금액_누적=sum(배분금액예측_누적),연도별배분금액_평균=mean(배분금액예측),개수=n(),
                                              모태약정액=mean(모태약정액.y))


df33 <-df %>% group_by(결성연도,납입시점) %>% summarize(예상납입비율=mean(납입비율),예상납입비율_누적_누적=mean(납입비율_누적),개수=n())

df44 <-df2 %>% group_by(배분시점) %>% summarize(예상회수율=mean(회수율),예상회수율_누적_누적=mean(회수율_누적),개수=n())

addWorksheet(workb, "납입")
writeDataTable(workb,"납입",df)

addWorksheet(workb, "배분")
writeDataTable(workb,"배분",df2)

dt <- left_join(
  data1,
  data22,
  by = "예측년도")



# 회수율
data3 <- data3.raw |> 
  select(-`회수율_누적`) |>
  filter(`결성연도` < 2020) |>
  left_join(
    data3.raw |>
      filter(`결성연도` < 2020) |>
      group_by(`배분시점`) |>
      summarise(`회수율_평균` = mean(`회수율`, na.rm = TRUE),
                `회수율_누적` = mean(`회수율_누적`, na.rm = TRUE)) |>
      ungroup() |>
      mutate(`회수율_증분` = `회수율_누적` - lag(`회수율_누적`)),
    by = "배분시점"
  ) |>
  mutate(`회수율_증분` = ifelse(is.na(`회수율_증분`), `회수율_누적`, `회수율_증분`))

addWorksheet(workb, "회수율")
writeDataTable(workb,"회수율",data3)

# 납입비율
data2 <- data2.raw |> 
  select(-`납입비율_누적`) |>
  filter(`결성연도` < 2020) |>
  left_join(
    data2.raw |>
      filter(`결성연도` < 2020) |>
      group_by(`납입시점`) |>
      summarise(`납입비율_평균` = mean(`납입비율`, na.rm = TRUE),
                `납입비율_누적` = mean(`납입비율_누적`, na.rm = TRUE)) |>
      ungroup() |>
      mutate(`납입비율_증분` = `납입비율_누적` - lag(`납입비율_누적`)),
    by = "납입시점"
  ) |>
  mutate(`납입비율_증분` = ifelse(is.na(`납입비율_증분`), `납입비율_누적`, `납입비율_증분`))


addWorksheet(workb, "납입비율")
writeDataTable(workb,"납입비율",data2)



# 기준연도별 회수율
for (i in 2013:2019){
  
  if (i == 2013){
    
    data3 <- data3.raw |>
      filter(`결성연도` <= i) |>
      group_by(`배분시점`) |>
      summarise(`회수율_평균` = mean(`회수율`, na.rm = TRUE),
                `회수율_누적` = mean(`회수율_누적`, na.rm = TRUE)) |>
      ungroup() |>
      mutate(`회수율_증분` = `회수율_누적` - lag(`회수율_누적`)) |>
      mutate(`회수율_증분` = ifelse(is.na(`회수율_증분`), `회수율_누적`, `회수율_증분`),
             `기준연도` = i)
    
  }
  else{
    
    data3 <- bind_rows(
      data3,
      data3.raw |>
        filter(`결성연도` <= i) |>
        group_by(`배분시점`) |>
        summarise(`회수율_평균` = mean(`회수율`, na.rm = TRUE),
                  `회수율_누적` = mean(`회수율_누적`, na.rm = TRUE)) |>
        ungroup() |>
        mutate(`회수율_증분` = `회수율_누적` - lag(`회수율_누적`)) |>
        mutate(`회수율_증분` = ifelse(is.na(`회수율_증분`), `회수율_누적`, `회수율_증분`),
               `기준연도` = i)
    )
    
  }
  
}

# 기준연도별 납입비율
for (i in 2013:2019){
  
  if (i == 2013){
    
    data2 <- data2.raw |>
      filter(`결성연도` <= i) |>
      group_by(`납입시점`) |>
      summarise(`납입비율_평균` = mean(`납입비율`, na.rm = TRUE),
                `납입비율_누적` = mean(`납입비율_누적`, na.rm = TRUE)) |>
      ungroup() |>
      mutate(`납입비율_증분` = `납입비율_누적` - lag(`납입비율_누적`)) |>
      mutate(`납입비율_증분` = ifelse(is.na(`납입비율_증분`), `납입비율_누적`, `납입비율_증분`),
             `기준연도` = i)
    
  }
  else{
    
    data2 <- bind_rows(
      data2,
      data2.raw |>
        filter(`결성연도` <= i) |>
        group_by(`납입시점`) |>
        summarise(`납입비율_평균` = mean(`납입비율`, na.rm = TRUE),
                  `납입비율_누적` = mean(`납입비율_누적`, na.rm = TRUE)) |>
        ungroup() |>
        mutate(`납입비율_증분` = `납입비율_누적` - lag(`납입비율_누적`)) |>
        mutate(`납입비율_증분` = ifelse(is.na(`납입비율_증분`), `납입비율_누적`, `납입비율_증분`),
               `기준연도` = i)
    )
    
  }
  
}



data <- data.raw %>%
  left_join(
    ratio,
    by = "결성연도"
  ) %>%
  mutate(`예측기간` = `결성연도` + `배분시점`,
         `배분금액_추정` = 배분비율*모태약정액) %>%
  group_by(`예측기간`) %>%
  summarise(`배분금액` = sum(`배분금액_추정`)) %>%
  ungroup()

#예측값

work <- createWorkbook("심층분석0.xlsx")

addWorksheet(work, "dt")
writeDataTable(work,"dt",dt)


addWorksheet(work, "납입")
writeDataTable(work,"납입",data2)

addWorksheet(work, "회수")
writeDataTable(work,"회수",data3)



saveWorkbook(work, file="./심층분석!.xlsx")
