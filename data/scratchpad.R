rm(list=ls())
source("readData.R")


astimtype="embedding"     

(ggplot(ratingtypes.df,aes(x=is_acceptable))+geom_histogram(binwidth=.1)+
 geom_vline(data=ratingtypes.df%>%filter(stim_type==astimtype),aes(xintercept=is_acceptable,color=item_type),alpha=.3)+
 geom_vline(data=ratingtypes.df%>%filter(stim_type==astimtype)%>%group_by(item_type)%>%summarize(mean_acceptability=mean(is_acceptable,na.rm=TRUE))%>%ungroup(),
            aes(xintercept=mean_acceptability,color=item_type),size=2)+guides(color=FALSE))+
    (ggplot(ratingtypes.df,aes(x=is_grammatical))+geom_histogram(binwidth=.1)+
     geom_vline(data=ratingtypes.df%>%filter(stim_type==astimtype),aes(xintercept=is_grammatical,color=item_type),alpha=.3)+
     geom_vline(data=ratingtypes.df%>%filter(stim_type==astimtype)%>%group_by(item_type)%>%summarize(mean_acceptability=mean(is_grammatical,na.rm=TRUE))%>%ungroup(),
                aes(xintercept=mean_acceptability,color=item_type),size=2)+guides(color=FALSE))
