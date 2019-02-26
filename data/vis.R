rm(list=ls())#To remove...
source("readData.R")
##Convenience flags: make sure all plot var names end with .plot, then save all or print all to screen at the end of the file.
saveplots <- FALSE
viewplots <- FALSE

#Ok this response count thing is good to know, looks a little creepy. Imbalance in items is created by balancing responses over stim_type & canon_status evenly, but there are different numbers of items in each of these conditions, so the distribution over items is rough. Just keep your eyes open I guess?
item_responsecount.plot <-
    ggplot(item_responsecount.df,aes(x=text,y=count,color=canon_status))+
    geom_point()+
    facet_grid(.~stim_type,scales="free")+ggtitle("Number of responses per item")

responsehists.plot <- ggplot(responses.df,aes(x=response,fill=canon_status))+geom_bar(position="dodge")+facet_wrap(~questiontext)

ratingcomparison_byitem.plot <-
    ggplot(ratingtypes.df,aes(x=is_grammatical,y=is_acceptable,color=canon_status))+geom_point(size=3)

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

##original:
## byitemtype.df <- responses.df%>%
##     group_by(item_type,questiontext,stim_type)%>%
##     summarize(meanresponse=mean(response),canon_status=canon_status[1])%>%
##     ungroup()%>%
##     spread(questiontext,meanresponse)

byitemtype.df <- responses.df%>%
    group_by(item_type,questiontext,stim_type)%>%
    summarize(meanresponse=mean(response),ci.low=mean(response)+qnorm(.025)*sd(response)/sqrt(n()),ci.high=mean(response)+qnorm(.975)*sd(response)/sqrt(n()),canon_status=canon_status[1])%>%
    ungroup()

bit_A <- byitemtype.df%>%filter(questiontext=="is_acceptable")%>%select(-questiontext)
names(bit_A) <- paste0("acc_",names(bit_A))
bit_G <- byitemtype.df%>%filter(questiontext=="is_grammatical")%>%select(-questiontext)
names(bit_G) <- paste0("gram_",names(bit_G))
byitemtype_ci.df<- cbind(bit_A,bit_G)

ratingcomparison_byitemtype.plot <-
    ggplot(byitemtype_ci.df,aes(x=acc_meanresponse,y=gram_meanresponse,color=gram_canon_status))+
    geom_line(aes(x=acc_meanresponse,y=acc_meanresponse),color="black",alpha=.3)+
#    geom_smooth(aes(group=1),linetype="dashed",alpha=.1)+ #1d != lm
    geom_point(size=3)+
    geom_errorbarh(aes(xmin=acc_ci.low,xmax=acc_ci.high))+
    geom_errorbar(aes(ymin=gram_ci.low,ymax=gram_ci.high))+
    geom_text(aes(label=paste0(acc_stim_type,":",acc_item_type)),hjust=1, vjust=1)+
    xlab("Mean acceptability rating")+ylab("Mean grammaticality rating")+scale_color_discrete(name="Canonical status")


itemtypediffs.df <- ratingtypes.df%>%group_by(stim_type,item_type)%>%summarize(meandiff=mean(is_acceptable-is_grammatical, na.rm=TRUE))%>%ungroup()

ratingdifference.plot <-
    ggplot(ratingtypes.df,aes(x=text,y=(is_acceptable-is_grammatical),color=canon_status,shape=canon_status))+
    geom_hline(aes(yintercept=0))+
    geom_point(aes())+
    facet_grid(.~stim_type,scales="free")#+
#    geom_hline(data=itemtypediffs.df,aes(yintercept=meandiff,color=item_type)) #mildly informative but ugly AF.


ratingbyitemhist.plot <-
    (ggplot(ratingtypes.df,aes(x=is_acceptable))+geom_histogram(binwidth=.1)+ggtitle("Acceptability rating"))+
    (ggplot(ratingtypes.df,aes(x=is_grammatical))+geom_histogram(binwidth=.1)+ggtitle("Grammaticality rating"))


for(astimtype in unique(ratingtypes.df$stim_type)){
    #crazy eval thing is just to keep the ".plot names for plots" convention, which makes plot saving DRY. Not sure if this is good, seems worth trying? If it's bad, sh*t like this is where it goes bad...
       eval(parse(text=paste0(astimtype,"_AGhistograms.plot<-",
"(ggplot(ratingtypes.df,aes(x=is_acceptable))+geom_histogram(binwidth=.1)+ggtitle(paste0(astimtype,\": acceptability rating task\"))+
    geom_vline(data=ratingtypes.df%>%filter(stim_type==astimtype),aes(xintercept=is_acceptable,color=item_type),alpha=.3)+
    geom_vline(data=ratingtypes.df%>%filter(stim_type==astimtype)%>%group_by(item_type)%>%summarize(mean_acceptability=mean(is_acceptable))%>%ungroup(),
               aes(xintercept=mean_acceptability,color=item_type),size=2)+guides(color=FALSE))+
(ggplot(ratingtypes.df,aes(x=is_grammatical))+geom_histogram(binwidth=.1)+ggtitle(paste0(astimtype,\": grammaticality rating task\"))+
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
