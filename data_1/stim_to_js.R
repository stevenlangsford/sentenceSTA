library(tidyverse)

stimsource.df <- read.csv("stimcollection1.csv", stringsAsFactors=FALSE)

stimsource.df$sentence <- sapply(stimsource.df$sentence,function(x){return(gsub("\\'","\\\\'",x))})#Ok for now but you might also want to check for " marks too.

stimsource.df <- stimsource.df%>%select(sentence,sentenceType,grammatical)%>%mutate(cellID=as.factor(paste0(sentenceType,ifelse(grammatical,"_canon","_notcanon"))))

##old one-big-vector version
 ## cat(paste0("var stim = shuffle(['",paste(stimsource.df$sentence,collapse="','"),"'])"),
 ##     file="stimarr.js")

sink(file="bycell.js")
cat("var allcells = {")
for(acell in unique(stimsource.df$cellID)){
    cat(paste0(acell,":","shuffle(['",paste((stimsource.df%>%filter(cellID==acell))$sentence,collapse="','"),"']),")) #Ugh note you have to delete the last comma.
}
cat("}")
sink()
