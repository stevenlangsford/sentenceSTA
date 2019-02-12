library(tidyverse)
library(patchwork)
theme_set(theme_light())
rm(list=ls()) #to remove

demographics.df <- read.csv("raw/demographicsdata.csv")

responses.df <- read.csv("raw/sentenceresponsedata.csv")%>%
    filter(response!="continue")%>%
    mutate(response=as.numeric(as.character(response)),
           questiontext=ifelse(questiontext=="Is this an <em>acceptable</em> English sentence?","Acceptable?","Grammatical?")
           )

##exclusions
badids <- data.frame()
##incomplete:
responsecount <- responses.df%>%group_by(ppntID)%>%summarize(count=n())
badids <- responsecount%>%filter(count!=max(count))%>%select(ppntID) #Safe to assume max count is the expected number of trials... right? Better like this, better code in a magic number (currently 62), or best to do either but protect with asserts?
##attention checks
attncheck_goodsentence <- responses.df%>%filter(text=="Sarah expected to get a good grade.")%>%select(ppntID,response)
attncheck_badsentence <- responses.df%>%filter(text=="Him would have been fired.")%>%select(ppntID,response)

badids <- badids%>%rbind(attncheck_goodsentence%>%filter(response<4)%>%select(ppntID)%>%rbind(attncheck_badsentence%>%filter(response>1)%>%select(ppntID))%>%distinct)
#Apply exclusions:
responses.df <- responses.df%>%filter(!(ppntID%in%badids$ppntID))

##remove attn checks from responses: these are not analysed further.
responses.df <- responses.df%>%filter(text!="Sarah expected to get a good grade.",text!="Him would have been fired.")

##add canonical status:
apriori.df <- read.csv("stimcollection1.csv")
responses.df$canon_status <- unlist(sapply(responses.df$text,function(x){return(apriori.df[as.character(apriori.df$sentence)==as.character(x),"grammatical"])}))

#ggplot(responses.df,aes(x=response))+geom_histogram()+facet_wrap(~questiontext)+
ggsave(ggplot(responses.df,aes(x=response,fill=canon_status))+geom_bar(position="dodge")+facet_wrap(~questiontext),file="pilot1.png")

repeats <- responses.df%>%group_by(text)%>%summarize(count=n())%>%filter(count>=2)#unneccessary with full coverage n! Everything should be repeated a bunch of times.
avsg.df <- responses.df%>%filter(text%in%repeats$text)%>%group_by(questiontext,text)%>%summarize(mean_response=mean(response))
#GATHER by sentence, a row should be sentence: gramscore, accscore, canonical_status. Question: norm responses by ppnt first? hmmm. maybe not, this time?
