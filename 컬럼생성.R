##2011년부터 21년까지 청산여부 파악하는 컬럼 생성하기

#패키지 불러오기
library(tidyverse)
library(lubridate)
library(readxl)
library(openxlsx)
library(showtext)
library(dlookr)

#데이터 불러오기
data_path <- "C:/Users/user/Documents/intern/심층분석"
data_nm <- "221019_모태출자펀드_중기부_데이터인턴.xlsx"
data.raw <- read_excel(file.path(data_path, data_nm))

data.raw


df <-data.raw %>% mutate(운용여부2011년=ifelse(is.na(조합청산일),TRUE,ifelse((format(조합결성일,"%y")<= 11)&(format(조합청산일,"%y")>= 11),TRUE ,FALSE)),
                         운용여부2012년=ifelse(is.na(조합청산일),TRUE,ifelse((format(조합결성일,"%y")<= 12)&(format(조합청산일,"%y")>=12),TRUE ,FALSE)),
                         운용여부2013년=ifelse(is.na(조합청산일),TRUE,ifelse((format(조합결성일,"%y")<= 13)&(format(조합청산일,"%y")>= 13),TRUE ,FALSE)),
                         운용여부2014년=ifelse(is.na(조합청산일),TRUE,ifelse((format(조합결성일,"%y")<= 14)&(format(조합청산일,"%y")>= 14),TRUE ,FALSE)),
                         운용여부2015년=ifelse(is.na(조합청산일),TRUE,ifelse((format(조합결성일,"%y")<= 15)&(format(조합청산일,"%y")>= 15),TRUE ,FALSE)),
                         운용여부2016년=ifelse(is.na(조합청산일),TRUE,ifelse((format(조합결성일,"%y")<= 16)&(format(조합청산일,"%y")>= 16),TRUE ,FALSE)),
                         운용여부2017년=ifelse(is.na(조합청산일),TRUE,ifelse((format(조합결성일,"%y")<= 17)&(format(조합청산일,"%y")>= 17),TRUE ,FALSE)),
                         운용여부2018년=ifelse(is.na(조합청산일),TRUE,ifelse((format(조합결성일,"%y")<= 18)&(format(조합청산일,"%y")>= 18),TRUE ,FALSE)),
                         운용여부2019년=ifelse(is.na(조합청산일),TRUE,ifelse((format(조합결성일,"%y")<= 19)&(format(조합청산일,"%y")>= 19),TRUE ,FALSE)),
                         운용여부2020년=ifelse(is.na(조합청산일),TRUE,ifelse((format(조합결성일,"%y")<= 20)&(format(조합청산일,"%y")>= 20),TRUE ,FALSE)),
                         운용여부2021년=ifelse(is.na(조합청산일),TRUE,ifelse((format(조합결성일,"%y")<= 21)&(format(조합청산일,"%y")>= 21),TRUE ,FALSE)))
                    
# data[, !!str_c(i,"년도 운용여부") := list(ifelse((format(조합결성일,"%Y")<= i)&(format(조합청산일,"%Y")> i),TRUE ,FALSE)]
# 
# data[ , !!str_c(2011,"년도 운용여부") := list(ifelse((format(조합결성일,"%Y")<= 2011)&(format(조합청산일,"%Y")> i),TRUE ,FALSE)]



# !!->컬럼명 앞에 붙여줌 / :=->(컬럼생성)/ as.Date / str_c/ 조합청산일이 NA값인 경우?

# 
# for(i in 2011:2021){
#   data <-data.raw %>% mutate(!!str_c(i,"운용여부"):=ifelse(((year(조합결성일)<= i)&((year(조합청산일)> i)))||is.na(조합청산일) , TRUE , FALSE))
# }
# 
# 
# 
# for(i in 2011:2021){
#   data<-data.raw %>% mutate(!!str_c(i,"운용여부"):=ifelse(((year(조합결성일,'%Y')<= i)&((year(조합청산일,'%Y')>i)))||is.na(조합청산일) , TRUE , FALSE))
# }
# 


names(df)

df2<-df %>% pivot_longer(cols = c("운용여부2011년", "운용여부2012년","운용여부2013년", "운용여부2014년" ,"운용여부2015년", "운용여부2016년", "운용여부2017년", "운용여부2018년","운용여부2019년" ,"운용여부2020년", "운용여부2021년"))

df2<-df2 %>% mutate(name=substr(name, 5, 8))

df2<-df2[df2$value==TRUE,]

write.xlsx(df2, sheetName="sheet1", file="심층분석2.xlsx")
