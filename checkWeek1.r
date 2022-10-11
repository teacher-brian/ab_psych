library(rvest)
library(tidyverse)
library(clipr)
library(lubridate)
email<- read_clip()  #from notify everyone email before sending
email1<- read_clip()  #from notify everyone email before sending
users<- read.csv("slackUsers.csv")
roster.d1 <- read.csv("d1.csv")
roster.h2 <- read.csv("h2.csv")
roster <- rbind(roster.d1,roster.h2)
roster %>% separate(Name,into = c('last','first'),sep = ',')

email<- as.data.frame(strsplit(email,", "))
email1<- as.data.frame(strsplit(email1,", "))
colnames(email1) <- "email"
colnames(email ) <- "email"
email<- rbind(email,email1)
missing<- email%>% left_join(users) %>% select(email, fullname) %>% filter(is.na(fullname))

gsub(x=paste(shQuote(missing$email), collapse = ", "),"'","" )%>% write_clip()

week1 <- read_clip()

week1.bak <- week1

    grep("[a]|\\s|Romy",week1,value = T)  %>% #romy gets filtered out
    grep("[a-z\\)]$|\\s[0-9]{1,2}:.*[AP]M$|joined",.,value = T,ignore.case = T)  %>%
    grep("Google Doc|G Suite Document|repl|days|iew|http|rian|Only|one doc",
         .,invert = T,value = T)  %>%  as.data.frame() -> week_post

    colnames(week_post) <- "data"

head(week_post)



grep("[a]|\\s",week1,value = T)  %>%
  grep("[a-z]$|\\s[0-9]{1,2}:.*[AP]M$|joined",.,value = T,ignore.case = T)  %>%
  grep("Google Doc|G Suite Document|repl|days|iew|http|rian|Only|one doc",
       .,invert = T,value = T)  %>%  as.data.frame() -> week_post

posted <- week_post %>%
          mutate(time=dplyr::lead(data,1),joined=dplyr::lead(data,2)) %>%
      mutate(time= gsub("^[a-zA-Z]",NA,time)) %>%
  mutate(data= gsub("^[ 0-9]",NA,data)) %>%
  filter(grepl("joined",joined,ignore.case=T,)==F) %>%
  filter(grepl("joined",data,ignore.case=T,)==F) %>%
  filter(complete.cases(.)) %>%
  select(data) %>%  unique(.)



colnames(week_post) <- "data"


head(week_post)

users  %>% mutate_all(as.character) %>% mutate(displayname=ifelse(nchar(displayname)<1,fullname,displayname)) %>% anti_join(posted, by = c("displayname"="data")) %>% filter(status =="Member") %>% select(displayname,email,fullname) %>%
  mutate(message= paste0("Hi @",displayname,", Week 3 is beginning, and I'm not seeing a post from you to week 1. I might be wrong because I'm using a program to filter for people who haven't posted yet, but I don't think you've posted to week 1 yet.  Do you need any help?  I know a few students are trying to get caught up but at this point, week 1 should be posted.  The #logistics channel is available."))  %>% write_clip()

