rm(list=ls())#To remove...
source("readData.R")
##Convenience flags: make sure all plot var names end with .plot, then save all or print all to screen at the end of the file.
saveplots <- TRUE
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


byitemtype.df <- responses.df%>%
    group_by(item_type,questiontext,stim_type)%>%
    summarize(meanresponse=mean(response),canon_status=canon_status[1])%>%
    ungroup()%>%
    spread(questiontext,meanresponse)

ratingcomparison_byitem.plot <- ggplot(byitemtype.df,aes(x=is_acceptable,y=is_grammatical,color=canon_status))+
    geom_line(aes(x=is_acceptable,y=is_acceptable),color="black")+
    geom_point(size=3)+
    geom_text(aes(label=paste0(stim_type,":",item_type)),hjust=1, vjust=1)


ratingdifference.plot <- ggplot(ratingtypes.df,aes(x=text,y=(is_acceptable-is_grammatical),color=stim_type,shape=canon_status))+
    geom_hline(aes(yintercept=0))+
    geom_point(aes())+
    facet_grid(.~stim_type,scales="free")


ratingbyitemhist.plot <- (ggplot(ratingtypes.df,aes(x=is_acceptable))+geom_histogram()+ggtitle("Acceptability rating"))+
    (ggplot(ratingtypes.df,aes(x=is_grammatical))+geom_histogram()+ggtitle("Grammaticality rating"))


for(astimtype in unique(ratingtypes.df$stim_type)){
    #crazy eval thing is just to keep the ".plot names for plots" convention, which makes plot saving DRY. Not sure if this is good, seems worth trying? If it's bad, sh*t like this is where it goes bad...
       eval(parse(text=paste0(astimtype,"_AGhistograms.plot<-",
"(ggplot(ratingtypes.df,aes(x=is_acceptable))+geom_histogram()+ggtitle(paste0(astimtype,\": acceptability rating task\"))+
    geom_vline(data=ratingtypes.df%>%filter(stim_type==astimtype),aes(xintercept=is_acceptable,color=item_type),alpha=.3)+
    geom_vline(data=ratingtypes.df%>%filter(stim_type==astimtype)%>%group_by(item_type)%>%summarize(mean_acceptability=mean(is_acceptable))%>%ungroup(),
               aes(xintercept=mean_acceptability,color=item_type),size=2)+guides(color=FALSE))+
(ggplot(ratingtypes.df,aes(x=is_grammatical))+geom_histogram()+ggtitle(paste0(astimtype,\": grammaticality rating task\"))+
    geom_vline(data=ratingtypes.df%>%filter(stim_type==astimtype),aes(xintercept=is_grammatical,color=item_type),alpha=.3)+
    geom_vline(data=ratingtypes.df%>%filter(stim_type==astimtype)%>%group_by(item_type)%>%summarize(mean_acceptability=mean(is_grammatical))%>%ungroup(),
               aes(xintercept=mean_acceptability,color=item_type),size=2))
    ")))
}



##WRAPUP: Save or view loop
##For everything that obeys the .plot naming convention:
for(aplot in grep("\\.plot",ls(),value=TRUE)){
    if(viewplots){ x11(); print(eval(parse(text=aplot)))}
    if(saveplots){ggsave(eval(parse(text=aplot)),file=paste0("plots/",aplot,".png"),width=15)}
}
