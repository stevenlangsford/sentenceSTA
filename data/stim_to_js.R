library(tidyverse)

stimsource.df <- read.csv("stimcollection1.csv", stringsAsFactors=FALSE)

stimsource.df$sentence <- sapply(stimsource.df$sentence,function(x){return(gsub("\\'","\\\\'",x))})#Ok for now but you might also want to check for " marks too.

 cat(paste0("var stim = shuffle(['",paste(stimsource.df$sentence,collapse="','"),"'])"),
     file="stimarr.js")
