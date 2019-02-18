library(tidyverse)
library(patchwork)
theme_set(theme_light())

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
responses.df$stim_type <- unlist(sapply(responses.df$text,function(x){return(apriori.df[as.character(apriori.df$sentence)==as.character(x),"sentenceType"])}))

byitem.df <- responses.df%>%
    group_by(questiontext,text)%>%
    summarize(mean_response=mean(response),count=n()) #just taking the mean like this possibly not ideal! You're in raw likert land, consider standardizing by ppnt (iff that doesn't muddy the waters too much re state trace / comparisons between acc and gram patterns? Also, some of these are probably error-detected/error-missed mixtures! Might be important.


ratingtypes.df <- byitem.df%>%select(-count)%>%spread(questiontext,mean_response)%>%left_join(responses.df%>%select(text,canon_status),by="text")
names(ratingtypes.df) <- c("text","acceptable","grammatical","canonical")#just getting rid of the ? marks after Acceptable and Grammatical

write.csv(ratingtypes.df,file="threeratingtypes.csv",row.names=FALSE)
