library(httr)
library(rvest)
library(stringr)
library(dplyr)
library(purrr)

yearpages<-data.frame(year=c(1995:2003),limit=c(3,25,43,46,53,65,64,43,39))

getPages<-function(year,limit){
  return(data.frame(year=year,page=1:limit))
}

getContent<-function(year,page){
  webpage<-httr::GET(paste0("https://www.cnet.com/sitemaps/articles/",year,"/",page,"/"))
  html_nodes(content(webpage), "a") -> nodes
  nodes %>% html_attr('href') -> links
  nodes %>% html_text(titles[[1]]) -> titles
  return(data.frame(titles=titles,links=links))
}

#maybe someway to use purrrlyr::invoke_rows for this but it seems to not cooperate
yearpages %>% 
  plyr::mdply(getPages) %>% 
  select(-limit) %>% 
  plyr::mdply(getContent) %>%
  filter(str_detect(links,'^/news/\\S+')) %>% 
  filter(str_detect(titles,'Commentary')) -> commentary