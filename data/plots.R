
source("readData.R")
##ggplot(responses.df,aes(x=response))+geom_histogram()+facet_wrap(~questiontext)+
ggsave(ggplot(responses.df,aes(x=response,fill=canon_status))+geom_bar(position="dodge")+facet_wrap(~questiontext),file="responsehists.png")

ggsave(ggplot(ratingtypes.df,aes(x=grammatical,y=acceptable,color=canonical))+geom_point(size=3),file="ratingtype_comparison.png")
