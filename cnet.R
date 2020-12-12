#library(tidyverse)
library(httr)
library(rvest)
library(stringr)
library(dplyr)
library(plyr)
#library(purrr)

yearpages<-data.frame(year=c(1995:2003),limit=c(3,25,43,46,53,65,64,43,39))

getPages<-function(year,limit){
  return(data.frame(year=year,page=1:limit))
}

getContent<-function(year,page){
  html_nodes(content(httr::GET(paste0("https://www.cnet.com/sitemaps/articles/",year,"/",page,"/"))), "a") -> nodes
  nodes %>% html_attr('href') -> links
  nodes %>% html_text() -> titles
  return(data.frame(titles=titles,links=links))
}

#maybe someway to use purrrlyr::invoke_rows or pmap_df for this but they seem to be casting
yearpages %>% 
  plyr::mdply(getPages) %>% 
  select(-limit) %>% 
  plyr::mdply(getContent) %>%
  filter(str_detect(links,'^/news/\\S+')) %>% 
  filter(str_detect(titles,'Commentary')) %>%
  mutate(titles=str_replace_all(string=titles,pattern="\\s+?\n?",replacement="")) %>%
  mutate(hyperlink=paste0('<a href="https://www.cnet.com/news',links,'">',titles,'</a>')) -> commentary