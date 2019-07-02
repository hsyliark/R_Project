setwd("D:/Workplace/R_Project/7.2")

## Crawling 방법
# html_nodes() -> tag, 구별자 (class,id)
# html_attr() -> src, 속성이름 검색
# html_text() -> h1, 텍스트 가져올 때

# &ndsp; -> 빈칸 

## CSS 적용 방법
# 1. tag에서 attribute 적용
# 2. <head> style </head>
# 3. 별도 파일

### list 만들기

# 순서 있는 리스트 - <ol> </ol>
# 순서 없는 리스트 - <ul> </ul>
# 정의 리스트 - <dl> </dl>

## list item
# <li> ... </li>
# <li> 생략 가능

### table
# <table> : 표 전체를 담는 컨테이너

###
# id : # --> javascript
# class : . 
# <caption> : 표 제목
# <thead> : 헤딩 셀 그룹
# <tfoot> : 바닥 셀 그룹
# <tbody> : 데이터 셀 그룹
# <tr> : 행, 여러 셀로 구성 <th>, <td>
# <th> : 제목 셀
# <td> : 데이터 셀

### anchor <a>





# 한빛아카데미 도서 crawling

install.packages("rvest")
library(rvest)
library(dplyr)
library(stringr)
trim <- function(x) gsub("^\\s+|\\s+$", "", x)



base_url <- 'http://www.hanbit.co.kr/store/books/category_list.html?'
page <- 'page='
category <- c('&cate_cd=004007&srt=p_pub_date','&cate_cd=004008&srt=p_pub_date',
              '&cate_cd=004003&srt=p_pub_date','&cate_cd=004004&srt=p_pub_date',
              '&cate_cd=004006&srt=p_pub_date','&cate_cd=005005&srt=p_pub_date')
              

hanbit_books <- data.frame(title=c(),writer=c(),price=c()) 

for (n in 1:length(category)) {

for (i in 1:2) {
  
url <- paste0(base_url,page,i,category[n])

html <- read_html(url)
# book_list <- html_node(html,'.sub_book_list_area')
# lis <- html_nodes(book_list,"li")

lis <- html %>%
  html_node('.sub_book_list_area') %>%
  html_nodes("li") 
  
price <- c()
title <- c()
writer <- c()

for (li in lis) {
  pr <- html_node(li,'.price') %>% html_text()
  pr <- gsub("\\\\","",pr)
  price <- c(price,pr)
  title <- c(title,html_node(li,'.book_tit') %>% html_text())
  writer <- c(writer,html_node(li,'.book_writer') %>% html_text()) 
  # cat(title,writer,price,'\n')
}

books <- data.frame(title=title,writer=writer,price=price)
hanbit_books <- rbind.data.frame(hanbit_books,books)

}
  
}

hanbit_books








