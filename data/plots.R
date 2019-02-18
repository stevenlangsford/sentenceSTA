#rm(list=ls())#To remove...
source("readData.R")
##Convenience flags: make sure all plot var names end with .plot, then save all or print all to screen at the end of the file.
saveplots <- FALSE
viewplots <- FALSE

responsehists.plot <- ggplot(responses.df,aes(x=response,fill=canon_status))+geom_bar(position="dodge")+facet_wrap(~questiontext)

ratingcomparison_byitem.plot <- ggplot(ratingtypes.df,aes(x=grammatical,y=acceptable,color=canonical))+geom_point(size=3)

bycondition.df <- responses.df%>%
    mutate(condition=paste(stim_type,canon_status,sep="_"))%>%
    group_by(condition,questiontext)%>%
    summarize(meanresponse=mean(response),canon_status=canon_status[1])%>%
    ungroup()%>%
    spread(questiontext,meanresponse)%>%
    mutate(sentencetype=sapply(condition,function(x){strsplit(x,"_")[[1]][[1]]}))#afterthought...

 names(bycondition.df)=c("condition","canon_status","acceptability_rating","grammaticality_rating","sentencetype")

ratingcomparison_bycondition.plot <- ggplot(bycondition.df,aes(x=acceptability_rating,y=grammaticality_rating,color=canon_status))+
    geom_line(aes(x=acceptability_rating,y=acceptability_rating))+
    geom_point(size=3)+
    geom_text(aes(label=sentencetype),hjust=0, vjust=0)



ratingdifference.plot <- ggplot(ratingtypes.df,aes(x=text,y=(is_acceptable-is_grammatical),color=stim_type,shape=canon_status))+
    geom_hline(aes(yintercept=0))+
    geom_point(aes())+
    facet_grid(.~stim_type,scales="free")

##For everything that obeys the .plot naming convention:
for(aplot in grep("\\.plot",ls(),value=TRUE)){
    if(viewplots){ x11(); print(eval(parse(text=aplot)))}
    if(saveplots){ggsave(eval(parse(text=aplot)),file=paste0(aplot,".png"))}
}
