library(rvest)
library(tidyverse)
library(clipr)
library(lubridate)
users<- read.csv("slackUsers.csv")
email <- read_clip()
email<- as.data.frame(strsplit(email,", "))

colnames(email ) <- "email"

missing<- email%>% left_join(users) %>% select(email, fullname) %>% filter(is.na(fullname))

gsub(x=paste(shQuote(missing$email), collapse = ", "),"'","" )%>% write_clip()


users<- read.csv("slackUsers.csv")
roster <- read.csv("h5.csv")
roster_names<- roster %>% separate(Name,into = c('last','first'),sep = ',') %>% mutate(across(everything(),tolower))

slack_names<- users %>% select("fullname") %>% separate(fullname,into = c('first_s','last_s'),sep = ' ') %>% mutate(across(everything(),tolower))

roster_names %>% left_join(slack_names,by = c("last"="last_s"))

slack_names%>% left_join(roster_names,by = c("last_s"="last")) %>% arrange(first_s)

week1 <- read_clip()
week1

posted<- grep("[0-9]{1,2}:|joined",week1,value = T)  %>%
  as.data.frame(row.names = NULL) %>%
  separate (col=".",into=c("name","time"),sep = "  ") %>%
  mutate(check=lead(name)) %>%
  mutate(check=ifelse(grepl("joined",check,ignore.case=T),1,0)) %>%
  filter(!grepl('joined',name,ignore.case=T) ) %>%
  filter (check!=1) %>%
  select(name) %>%  unique(.)



users %>% anti_join(posted, by = c("displayname"="name")) %>% filter(status =="Member") %>% select(displayname,email,fullname) %>%
  mutate(message= paste0("Hi ",displayname,", Week 2 is getting close to being half over, and I'm not seeing a post to week 1. I might be wrong because I'm using program to filter for people who haven't posted yet, but I don't think you've posted to week 1 yet.  Do you need any help?  The #logistics channel is available."))
