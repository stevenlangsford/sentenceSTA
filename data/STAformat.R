rm(list=ls())#To remove!
preexisting = ls() #Cleanup rm later should respect anything that already existed.
source("readData.R")
toclear <- c(setdiff(ls(),preexisting),"toclear") #rm(list=toclear) erases everything built by readData, keeps pre-existing workspace & any products of this script.

##here stim_id reflects the sentence structure, eg 'valid embedding', 'embedding mising the first verb', aggregating over all items of that type.
##I think this is the main target. 'Sentence structure' is the level of abstraction linguists using judgement data typically work at, right?

stim_sentencetype_general <- ratingtypes.df%>% mutate(
                                                   stimID=as.integer(item_type),#converting to the recommended integer-coding format for matrix representation
                                                   canon_status=ifelse(canon_status,1,2)
                                               )%>%
    select(stimID,canon_status,is_acceptable,is_grammatical)%>%
    gather(key="ratingtype",value="rating",is_acceptable:is_grammatical)%>%
    mutate(ratingtype=ifelse(ratingtype=="is_acceptable",1,2))%>%
    as.matrix
##SAVE STIM_sentencetype_general somewhere.
##Follow the same template but for items.
##Supply the one for ppnts with all the NA values, probably not going to use this?
##EMAIL THIS THING OFF WITH A BUNCH OF CONTEXT-PLOTS.

#rm(list=toclear)
