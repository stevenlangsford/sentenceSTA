library(tidyverse)
library(gganimate)
library(patchwork)
theme_set(theme_light())

rm(list=ls())

set.seed(1)

##setup params

n_stim = 200
trials_per_stim = 100
get_goodness <- function(n_stim){
    #runiform(n,0,1)
    return(rbeta(n_stim,.5,.5))#More realistic? Argument doesn't depend strongly on this.
}

##single well-formedness dimension
stimset <- data.frame(stim_id=1:n_stim,goodness=get_goodness(n_stim))
one_d_stim.df <- data.frame();
for(i in 1:(trials_per_stim)){
    one_d_stim.df <- rbind(one_d_stim.df,stimset)
}

one_d_stim.df$acceptability <- sapply(one_d_stim.df$goodness,function(x){rnorm(1,x,.3)>.5}) #simple noisy impression of goodness.
one_d_stim.df$grammaticality <- sapply(one_d_stim.df$goodness,function(x){rnorm(1,x,.1)>.5})#Considering any monotonic relation plausible: this noise reduction thing has an ok hand-wavey signal detection story behind it, but more importantly is fair game in a 1d world and produces an interesting shape.

one_d_stim.df <- one_d_stim.df%>%
    group_by(stim_id)%>%
    summarize(acceptability=mean(acceptability),grammaticality=mean(grammaticality)) #Open to analytic shortcuts, but it's easier/fully general to walk through a bunch of simulated decisions?


one_d_stim.df <- one_d_stim.df%>%mutate(frame=0,mycolor=rank(grammaticality)>n_stim/2,myshape=rank(acceptability)>n_stim/2)
destination.df <- one_d_stim.df%>%mutate(grammaticality=0,frame=1)
twoframes.df <- rbind(one_d_stim.df,destination.df)
                                                      
S_cartoon_1d <-
    ggplot(twoframes.df,aes(x=acceptability,y=grammaticality,color=mycolor,shape=myshape))+
    geom_point(size=3)+
    scale_color_discrete(name="rank(grammaticality)",labels=c("ungrammatical","grammatical"))+
    scale_shape_discrete(name="rank(acceptability",labels=c("unacceptable","acceptable"))+
    ggtitle("Endorsement proportion")+xlab("Is this acceptable?")+ylab("Is this grammatical?")+
    transition_states(frame,
                      transition_length=2,
                      state_length=1)


anim_save(S_cartoon_1d,file="bouncingpoints.gif")


## ##two-component well-formedness
## stimset <- data.frame(stim_id=1:n_stim,goodness_x=get_goodness(n_stim))%>%mutate(goodness_y=goodness_x*.9+rnorm(n_stim,0,.3)) #the two types of goodness are correlated, but not super tightly.
## two_d_stim.df <- data.frame();
## for(i in 1:(trials_per_stim)){
##     two_d_stim.df <- rbind(two_d_stim.df,stimset)
## }

## ##acc and gram have simple threshold decsion rules, but independently respond to x and y flavor goodness.
## two_d_stim.df$acceptability <- sapply(two_d_stim.df$goodness_x,function(x){rnorm(1,x,.3)>.5}) 
## two_d_stim.df$grammaticality <- sapply(two_d_stim.df$goodness_y,function(x){rnorm(1,x,.3)>.5})

## two_d_stim.df <- two_d_stim.df%>%
##     group_by(stim_id)%>%
##     summarize(acceptability=mean(acceptability),grammaticality=mean(grammaticality))

## S_cartoon_2d <-
##     ggplot(two_d_stim.df,aes(x=acceptability,y=grammaticality,color=rank(acceptability)>n_stim/2,shape=rank(grammaticality)>n_stim/2))+
##     geom_point(size=3)+
##     scale_color_discrete(name="rank(acceptability)",labels=c("unacceptable","acceptable"))+
##     scale_shape_discrete(name="rank(grammatical)",labels=c("ungrammatical","grammatical"))+
##     ggtitle("Endorsement proportion")+xlab("Is this acceptable?")+ylab("Is this grammatical?")

## ggsave(S_cartoon_2d,file="scartoon_2d.png",width=15)

## ggsave(S_cartoon_1d+ggtitle("1d state trace")+guides(color=FALSE,shape=FALSE)+S_cartoon_2d+ggtitle("2d state trace"),
##        file="onevstwoD.png",width=15)
