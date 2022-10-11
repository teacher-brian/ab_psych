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


grep("[a]|\\s",week1,value = T)  %>%
  grep("[a-z]$|\\s[0-9]{1,2}:.*[AP]M$|joined",.,value = T,ignore.case = T)  %>%
  grep("Google Doc|G Suite Document|repl|days|iew|http|rian|Only|one doc",
       .,invert = T,value = T)  %>%  as.data.frame() -> week_post

colnames(week_post) <- "data"

head(week_post)


posted <- week_post %>%
  mutate(time=dplyr::lead(data,1)) %>%
  mutate(time= gsub("^[a-zA-Z]",NA,time)) %>%
  filter(complete.cases(.)) %>%
  mutate(data= gsub("^[ 0-9]",NA,data)) %>%
  filter(complete.cases(.)) %>%
  select(data) %>%  unique(.)


users %>% anti_join(posted, by = c("displayname"="data")) %>% filter(status =="Member") %>% select(displayname,email,fullname) %>%
  mutate(message= paste0("Hi ",displayname,", Week 2 is getting close to being half over, and I'm not seeing a post to week 1. I might be wrong because I'm using program to filter for people who haven't posted yet, but I don't think you've posted to week 1 yet.  Do you need any help?  The #logistics channel is available."))
