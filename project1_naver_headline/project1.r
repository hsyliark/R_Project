setwd("D:/Workplace/R_Project/project1_naver_headline")

library(rvest)
library(dplyr)
library(stringr)
library(openxlsx)
library(abind)

trim <- function(x) gsub("^\\s+|\\s+$", "", x)


### Naver news 속보 top 20


categories <- c("정치","경제","사회","생활_문화","세계","IT_과학","오피니언")
base_url <- 'https://news.naver.com/main/list.nhn?mode=LSD&mid=sec&sid1='
number <- c(100,101,102,103,104,105,110)


# wb <- createWorkbook()

category <- c() 
for (i in 1:length(categories)) {
  category <- c(category,rep(categories[i],20))
}

naver_headline <- data.frame(title=c(), writer=c(), article=c())

for (i in 1:length(number)) {
  
  url <- paste0(base_url,number[i])
  html <- read_html(url)
  
  # 제목
  lis1 <- html %>%
    html_node('#main_content') %>%
    html_node('.type06_headline') %>%
    html_nodes('dt') %>%
    html_text() %>%
    trim()
  lis1 <- ifelse(lis1 %in% c("","동영상기사"),"",lis1)
  lis2 <- html %>%
    html_node('#main_content') %>%
    html_node('.type06') %>%
    html_nodes('dt') %>%
    html_text() %>%
    trim()
  lis2 <- ifelse(lis2 %in% c("","동영상기사"),"",lis2)
  lis.title <- c(lis1,lis2)
  title <- c()
  for (i in 1:length(lis.title)) {
    if (lis.title[i] %in% c("","동영상기사")) {
      title <- title }
    else {
      a <- gsub('\\"',"",lis.title[i])
      title <- c(title,a)
    }
  }
  
  # 제공회사
  lis3 <- html %>%
    html_node('#main_content') %>%
    html_node('.type06_headline') %>%
    html_nodes('.writing') %>%
    html_text()
  lis4 <- html %>%
    html_node('#main_content') %>%
    html_node('.type06') %>%
    html_nodes('.writing') %>%
    html_text() 
  writer <- c(lis3,lis4)
  
  # 기사
  lis5 <- html %>%
    html_node('#main_content') %>%
    html_node('.type06_headline') %>%
    html_nodes('.lede') %>%
    html_text() %>%
    trim()
  lis5 <- ifelse(lis5 %in% c("","동영상기사"),"",lis5)
  lis6 <- html %>%
    html_node('#main_content') %>%
    html_node('.type06') %>%
    html_nodes('.lede') %>%
    html_text() %>%
    trim()
  lis6 <- ifelse(lis6 %in% c("","동영상기사"),"",lis6)
  lis.article <- c(lis5,lis6)
  article <- c()
  for (i in 1:length(lis.article)) {
    if (lis.article[i] %in% c("","동영상기사")) {
      article <- article }
    else {
      a <- gsub('\\"',"",lis.article[i])
      article <- c(article,a)
    }
  }
  
  headline <- data.frame(title=title, writer=writer, article=article)
  naver_headline <- rbind.data.frame(naver_headline, headline)
  
#  addWorksheet(wb, categories[i])
#  writeDataTable(wb, categories[i], naver_headline)
  
}

# saveWorkbook(wb, file="D:/Workplace/R_Project/project1_naver_headline/naver_headline_top20.xlsx")

naver_headline <- cbind.data.frame(category,naver_headline)

View(naver_headline)

write.csv(naver_headline,"naver_headline.csv")





