setwd("D:/Workplace/R_Project/project/project3_daum_ranking/crawling")

library(rvest)
library(dplyr)
library(stringr)
library(openxlsx)
library(abind)


### Daum ranking news

trim <- function(x) gsub("^\\s+|\\s+$", "", x)

base_url <- "https://media.daum.net/ranking/"
url_cat <- c("popular",
             "popular?include=society,politics,culture,economic,foreign,digital",
             "popular?include=entertain",
             "popular?include=sports",
             "kkomkkom?include=news",
             "kkomkkom?include=entertain",
             "kkomkkom?include=sports",
             "bestreply",
             "bestreply?include=society,politics,culture,economic,foreign,digital",
             "bestreply?include=entertain",
             "bestreply?include=sports",
             "empathy/")
categories <- c("많이 본(종합)","많이 본(뉴스)","많이 본(연예)","많이 본(스포츠)",
                "열독률 높은(뉴스)","열독률 높은(연예)","열독률 높은(스포츠)",
                "댓글 많은(종합)","댓글 많은(뉴스)","댓글 많은(연예)","댓글 많은(스포츠)",
                "실시간 공감")


daum_ranking <- data.frame(category=c(),rank=c(),title=c(),writer=c(),article=c())

for (i in 1:length(url_cat)) {
  
  url <- paste0(base_url,url_cat[i])
  html <- read_html(url)
  
  lis <- html %>% html_node(".list_news2") %>% html_nodes("li")
  
  category <- c()
  rank <- c()
  title <- c()
  writer <- c()
  article <- c()
  
  for (li in lis) {
    
    # 순위
    rank <- c(rank ,li %>% html_node(".screen_out") %>% html_text())
    # 제목
    tmp <- li %>% html_node("strong.tit_thumb") %>% html_text() %>% trim()
    idx <- str_locate(tmp,"\n")
    title <- c(title,str_sub(tmp,1,idx[1]-1))
    # 회사
    tmp <- trim(str_sub(tmp,idx[1],-1))
    writer <- c(writer,tmp)
    # 범주
    category <- rep(categories[i],nrow(idx))
    # 기사
    article <- c(article,li %>% html_node(".desc_thumb") %>% html_text() %>% trim())  
    
  }
  
  ranking <- data.frame(category=category,rank=rank,title=title,writer=writer,article=article)
  daum_ranking <- rbind.data.frame(daum_ranking,ranking)
  
}

View(daum_ranking)

write.csv(daum_ranking,"daum_ranking.csv")
