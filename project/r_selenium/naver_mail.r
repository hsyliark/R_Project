# cd \Selenium -> dir (C드라이브 내에서 작업, cmd)


setwd("D:/Workplace/R_Project/project/r_selenium")

install.packages("RSelenium")
install.packages("rvest")
library(RSelenium)
library(rvest)
library(stringr)
library(dplyr)
trim <- function(x) gsub("^\\s+|\\s+$", "", x)



remDr<-remoteDriver(remoteServerAddr="localhost", port=4445L, browserName="chrome")
remDr$open()

remDr$navigate("http://nid.naver.com/nidlogin.login") #접속할 사이트 입력한다.
#DOM-->문서 전체의 element간의 계층모델로 가짐
#findElement() 함수를 이용하여 특정 위치를 지정할 수 있습니다. 
txt_id<-remDr$findElement(using="css selector","#id") #id="~~~~"의 형식이 나타나는데, 차례대로 입력하면 된다.
txt_pw<-remDr$findElement(using="id",value="pw")
login_btn<-remDr$findElement(using="class",value="btn_global") #로그인 버튼도 id/pw와 비슷하게 class='~~~~"입력

txt_id$setElementAttribute("value","hsyliark") # 아이디 입력
txt_pw$setElementAttribute("value","*********") # 비밀번호 입력
login_btn$clickElement()

remDr$navigate("https://mail.naver.com/")
mail_texts <- remDr$findElement(using="id",value="list_for_view")
                            # (using='css navigator', "subject")
mail_texts
mail_texts <- mail_texts$getElementText()
tmp <- str_split(mail_texts,'\n') %>% .[[1]]
tmp

sender <- c()
subject <- c()
time <- c()
for (i in 1:30) {
  sender <- c(sender,tmp[4*i-3])
  subject <- c(subject,tmp[4*i-2])
  time <- c(time,tmp[4*i-1])
}
df_mail <- data.frame(sender=sender,subject=subject,time=time)
df_mail
remDr$close()



# 출처 : https://sancj.tistory.com/62