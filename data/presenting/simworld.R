library(tidyverse)
library(patchwork)
theme_set(theme_light())

rm(list=ls())

set.seed(4)

##setup params

n_stim = 500
trials_per_stim = 300
get_goodness <- function(n_stim){
#    return(runif(n_stim,0,1))
    return(rbeta(n_stim,.2,.2))#More realistic? Argument doesn't depend strongly on this.
}

##single well-formedness dimension
stimset <- data.frame(stim_id=1:n_stim,goodness=get_goodness(n_stim))
one_d_stim.df <- data.frame();
for(i in 1:(trials_per_stim)){
    one_d_stim.df <- rbind(one_d_stim.df,stimset)
}

#changing endorsement/noise
one_d_stim.df$acceptability <- sapply(one_d_stim.df$goodness,function(x){rnorm(1,x,.5)>.5}) 
one_d_stim.df$grammaticality <- sapply(one_d_stim.df$goodness,function(x){rnorm(1,x,.1)>.5})


one_d_stim.df <- one_d_stim.df%>%
    group_by(stim_id)%>%
    summarize(acceptability=mean(acceptability),grammaticality=mean(grammaticality),goodness=mean(goodness)) #Open to analytic shortcuts, but it's easier/fully general to walk through a bunch of simulated decisions?

S_cartoon_1d <-
    ggplot(one_d_stim.df,aes(x=acceptability,y=grammaticality,color=rank(acceptability)>n_stim/2,shape=rank(grammaticality)>n_stim/2))+
    geom_point(size=3)+
    scale_color_discrete(name="rank(acceptability)",labels=c("unacceptable","acceptable"))+
    scale_shape_discrete(name="rank(grammatical)",labels=c("ungrammatical","grammatical"))+
    ggtitle("Endorsement proportion")+xlab("Is this acceptable?")+ylab("Is this grammatical?")
#ggsave(S_cartoon_1d,file="scartoon_1d.png",width=15)

##two-component well-formedness
stimset <- data.frame(stim_id=1:n_stim,goodness_x=get_goodness(n_stim))%>%mutate(goodness_y=goodness_x*.9+rnorm(n_stim,0,.15)) #the two types of goodness are correlated, but not super tightly.
two_d_stim.df <- data.frame();
for(i in 1:(trials_per_stim)){
    two_d_stim.df <- rbind(two_d_stim.df,stimset)
}

##acc and gram have simple threshold decsion rules, but independently respond to x and y flavor goodness.
two_d_stim.df$acceptability <- sapply(two_d_stim.df$goodness_x,function(x){rnorm(1,x,.3)>.5}) 
two_d_stim.df$grammaticality <- sapply(two_d_stim.df$goodness_y,function(x){rnorm(1,x,.3)>.5})

two_d_stim.df <- two_d_stim.df%>%
    group_by(stim_id)%>%
    summarize(acceptability=mean(acceptability),grammaticality=mean(grammaticality),goodness_x=mean(goodness_x),goodness_y=mean(goodness_y))

S_cartoon_2d <-
    ggplot(two_d_stim.df,aes(x=acceptability,y=grammaticality,color=rank(acceptability)>n_stim/2,shape=rank(grammaticality)>n_stim/2))+
    geom_point(size=3)+
    scale_color_discrete(name="rank(acceptability)",labels=c("unacceptable","acceptable"))+
    scale_shape_discrete(name="rank(grammatical)",labels=c("ungrammatical","grammatical"))+
    ggtitle("Endorsement proportion")+xlab("Is this acceptable?")+ylab("Is this grammatical?")

#ggsave(S_cartoon_2d,file="scartoon_2d.png",width=15)

#ggsave(S_cartoon_1d+ggtitle("1d state trace")+guides(color=FALSE,shape=FALSE)+S_cartoon_2d+ggtitle("2d state trace"),file="onevstwoD.png",width=15)

#ggplot(one_d_stim.df,aes(y=grammaticality,x=acceptability))+geom_point()

one_d_stim.df <- one_d_stim.df%>%mutate(
                                     top_acc = rank(acceptability)>n_stim/2,
                                     top_gram = rank(grammaticality)>n_stim/2,
                                     status = ifelse(top_acc==top_gram,"boring",ifelse(top_acc,"AnotG","GnotA"))
                                 )

mywidth=10
ggsave(
ggplot(one_d_stim.df,aes(x=goodness,y=acceptability))+geom_point()+ylim(c(0,1))+ylab("")+ggtitle("Acceptability")+
ggplot(one_d_stim.df,aes(x=goodness,y=grammaticality))+geom_point()+ylim(c(0,1))+ylab("")+ggtitle("Grammaticality"),
file="01_Acc_Gram.png",width=10)

ggsave(
ggplot(one_d_stim.df,aes(x=goodness,y=acceptability))+geom_point(aes(color=top_acc))+guides(color=FALSE)+
    ylim(c(0,1))+ylab("")+ggtitle("Acceptability")+
ggplot(one_d_stim.df,aes(x=goodness,y=grammaticality))+geom_point(aes(color=top_gram))+guides(color=FALSE)+ylim(c(0,1))+ylab("")+ggtitle("Grammaticality"),
file="02_AG_colbyrank.png",width=10)

ggsave(
(ggplot(one_d_stim.df,aes(x=goodness,y=acceptability))+geom_point(aes(color=status,alpha=(status!="boring")))+guides(color=FALSE,alpha=FALSE)+ggtitle("Acceptability")+
 ggplot(one_d_stim.df,aes(x=goodness,y=grammaticality))+geom_point(aes(color=status,alpha=(status!="boring")))+guides(color=FALSE,alpha=FALSE)+ggtitle("Grammaticality")),
file="03_highlight_dissoc.png",width=10)



ggsave(
ggplot(one_d_stim.df,aes(x=acceptability,y=grammaticality))+geom_point()+guides(color=FALSE)+ggplot(two_d_stim.df,aes(x=acceptability,y=grammaticality))+geom_point(),
file="04_statetrace1dvs2d.png",width=10)

#with(two_d_stim.df,cor(goodness_x,goodness_y)) #0.9204558
