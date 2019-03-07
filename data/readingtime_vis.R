rm(list=ls()) #Stop doing this?
source("readData.R")

alltimes_hist.plot <- ggplot(responses.df,aes(x=normTime))+geom_histogram()


timehist_bytype.plot <- ggplot() #stack in patchwork rather than facet just because it re-sets the colors on the item types under each stim type: 15 colors is annoying & not useful. Also, patchworking gives you a legend for each 'facet', which is nice here?
timehist_typeandquestion.plot <- ggplot()#this is the vis you want, but comes out pretty small/hard to read. save big and scroll around, or paginate?

for(stimtype in unique(responses.df$stim_type)){
    timehist_bytype.plot <- timehist_bytype.plot+ggplot(responses.df%>%filter(stim_type==stimtype),aes(x=normTime,fill=item_type))+geom_histogram(alpha=.5,position="identity")
    timehist_typeandquestion.plot <-
        timehist_typeandquestion.plot+ggplot(responses.df%>%filter(stim_type==stimtype),aes(x=normTime,fill=item_type))+geom_histogram(alpha=.5,position="identity")+facet_wrap(.~questiontext)
    
}

#histograms vs density plots? Both say the same thing...
##ggplot(responses.df,aes(x=normTime,fill=questiontext))+geom_histogram(alpha=.5,position="identity",binwidth=.2)+ggtitle("Reponse time by condition")
timebyquestion.plot <-
    ggplot(responses.df,aes(x=normTime,color=questiontext))+geom_density()+ggtitle("Reponse time by condition")

timegrid.plot <- #hmm. Beware eyeballing tiny facets?
    ggplot(byitem.df,aes(y=mean_time,x=mean_response,color=as.factor(item_type),shape=canonstatus))+geom_point()+facet_grid(questiontext~stim_type)
