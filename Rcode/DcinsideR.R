
install.packages("RSelenium")
install.packages("XML")
install.packages('wordcloud2') 
install.packages("KoNLP")
##################################################
library(RSelenium) 
library(XML)
library(wordcloud2)
library("KoNLP")  
library(rvest)
library(dplyr)


### 
remDr <- remoteDriver(remoteServerAddr = 'localhost', 
                      port = 4445L, #
                      browserName = "chrome") 
remDr$open() 

### 
setwd("c:/R_data/Dcinside")
all.title<- c()
all.rank <- c()
all.list.dc = list()

####

  url = 'https://www.dcinside.com/'
  
  remDr$navigate(url)
  
  page_source = remDr$getPageSource()[[1]]
  
  rank.name <-read_html(page_source) %>%
    html_nodes(xpath='//*[@id="rank_gall"]/ol[6]/li[*]/a/span[2]')%>%
    html_text()
  rank.url<-read_html(page_source) %>%
    html_nodes(xpath='//*[@id="rank_gall"]/ol[6]/li[*]/a')%>%
    html_attr('href')
  
   
  
  rank.df = data.frame(rank.name,rank.url)
####
  max=1;
for(rnk in 1:10){
  dc.title<- c()
  for(page in 1:max){
    url <- paste(rank.df[[2]][rnk],'&list_num=100&sort_type=N','&page=',page,sep='')
    
    
    remDr$navigate(url)
    
    page_source = remDr$getPageSource()[[1]]
    
    title <-read_html(page_source) %>%
      html_nodes(xpath='//*[@id="container"]/section[1]/article[2]/div[2]/table/tbody/tr[*]/td[2]/a[1]/text()')%>%
      html_text()
    
  
    
    dc.title <- c(dc.title, title)
  }
  dc.title = dc.title[1:(max*100)]
    list.dc = list(dc.title)
    all.list.dc = c(all.list.dc,list.dc)
  
}
  dc.title
  #
  names(all.list.dc) = rank.df[[1]]
  #########################
  df.dcinside = data.frame(all.list.dc)
  ###################################
  filestr = paste(Sys.time(),".csv")
  filestr = gsub(pattern = ":", 
      replacement = "",
      x = filestr)
  write.csv(df.dcinside, file = filestr)


