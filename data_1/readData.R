library(tidyverse)
library(patchwork)
theme_set(theme_light())

demographics.df <- read.csv("raw/demographicsdata.csv")

responses.df <- read.csv("raw/sentenceresponsedata.csv")%>%
    group_by(ppntID)%>%arrange(responseTime)%>% #setup to recover reading times
    mutate(deliberationTime=responseTime-lag(responseTime,default=responseTime[1]),
           normTime=(deliberationTime-mean(deliberationTime))/sd(deliberationTime) #standardize reading times. Note there are spacer stim fronting question blocks, the continue button on those 'starts the clock' for the first actual stim: first spacer screen gets a deliberation time of 0.
           )%>%
    ungroup()%>%
    filter(response!="continue")%>% #removes those spacer screen response rows.
    mutate(response=as.numeric(as.character(response)),
           textnchars=nchar(as.character(text)), #in characters, so includes spaces/punctuation. Is wordcount better/different? Probably?
           wordcount=sapply(strsplit(as.character(text), " "), length)+1, #hacky but good enough? There are no double spaces or one word sentences here, right?
           questiontext=ifelse(questiontext=="Is this an <em>acceptable</em> English sentence?","is_acceptable","is_grammatical") #more code-friendly level names.
           )

##exclusions
badids <- data.frame()
## Demographics exclusions
badids <- demographics.df%>%filter(age<=0)%>%select(ppntID) #clearly not cooperating, probably deliberately :-)
badids <- badids%>%rbind(demographics.df%>%filter(!str_detect(language,"en"))%>%select(ppntID)) #Reported non english speaker. Admits misspellings though.
##Super-duper fast:
timetaken.df <- responses.df%>%group_by(ppntID)%>%summarize(beginTime=min(responseTime), endTime= max(responseTime), totalTime=(endTime-beginTime)/1000/60)
badids <- badids%>%rbind(timetaken.df%>%filter(totalTime<4)%>%select(ppntID)) #Always intended to cut superfast responses, but the actual cut-point here is informed by the histogram of response times, ggplot(timetaken.df,aes(x=totalTime))+geom_histogram(binwidth=1)+geom_vline(aes(xintercept=4)). Hope this is not evil, makes sense to me?

##incomplete:
responsecount <- responses.df%>%group_by(ppntID)%>%summarize(count=n())
badids <- badids%>%rbind(responsecount%>%filter(count!=max(count))%>%select(ppntID)) #Safe to assume max count is the expected number of trials... right? Better like this, better code in a magic number (currently 62), or best to do either but protect with asserts?
##attention checks
attncheck_goodsentence <- responses.df%>%filter(text=="Sarah expected to get a good grade.")%>%select(ppntID,response)
attncheck_badsentence <- responses.df%>%filter(text=="Him would have been fired.")%>%select(ppntID,response)

badids <- badids%>%rbind(attncheck_goodsentence%>%filter(response<4)%>%select(ppntID)%>%rbind(attncheck_badsentence%>%filter(response>1)%>%select(ppntID))%>%distinct)

#Apply exclusions:
responses.df <- responses.df%>%filter(!(ppntID%in%badids$ppntID))

##remove attn checks from responses: these are not analysed further.
responses.df <- responses.df%>%filter(text!="Sarah expected to get a good grade.",text!="Him would have been fired.",text!="We were sure that the teaching assistant. liked to meet before lecture.") #First two are attn checks, last one a flawed stim. RS pointed this out, appears only in the pilot data (which is included for now.)

##add canonical status:
apriori.df <- read.csv("stimcollection1.csv")
responses.df$canon_status <- unlist(sapply(responses.df$text,function(x){return(apriori.df[as.character(apriori.df$sentence)==as.character(x),"grammatical"])}))
responses.df$stim_type <- unlist(sapply(responses.df$text,function(x){return(apriori.df[as.character(apriori.df$sentence)==as.character(x),"sentenceType"])}))
responses.df$item_type <- unlist(sapply(responses.df$text,function(x){return(apriori.df[as.character(apriori.df$sentence)==as.character(x),"itemType"])}))


byitem.df <- responses.df%>%
    group_by(questiontext,text,stim_type,item_type,canon_status)%>% #stim/item type does not vary after grouping by text, but handy to have around?
    summarize(mean_response=mean(response),
              mean_time=mean(normTime),
              canonstatus=canon_status[1],
              count=n())%>%
    ungroup()#just taking the mean like this possibly not ideal! You're in raw likert land, consider standardizing by ppnt (iff that doesn't muddy the waters too much re state trace / comparisons between acc and gram patterns? Also, some of these are probably error-detected/error-missed mixtures! Might be important.


ratingtypes.df <- byitem.df%>%select(-count)%>%spread(questiontext,mean_response)%>%left_join(responses.df%>%select(text,canon_status),by="text")%>%distinct

item_responsecount.df <- responses.df%>%group_by(text,item_type,stim_type,canon_status)%>%summarize(count=n())
itemtype_responsecount.df <- responses.df%>%group_by(item_type,stim_type,canon_status)%>%summarize(count=n())

write.csv(ratingtypes.df,file="threeratingtypes.csv",row.names=FALSE)
