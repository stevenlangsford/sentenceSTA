source("readData.R")

sentencetype_general <- ratingtypes.df%>% mutate(
                                              stimID=as.integer(item_type),#converting to the recommended integer-coding format for matrix representation
                                              canon_status=ifelse(canon_status,1,2)
                                          )%>%
    select(stimID,canon_status,is_acceptable,is_grammatical)%>%
    gather(key="ratingtype",value="rating",is_acceptable:is_grammatical)%>%
    mutate(ratingtype=ifelse(ratingtype=="is_acceptable",1,2))%>%
    as.matrix

dput(sentencetype_general,file="sentencetype_general.RData") #see codebook.txt for details.


###Other possible targets if I'm wrong about the level of aggregation here:

##Same to-general template, but aggregating by item, NOT by item-type.
byitem_general <- ratingtypes.df%>% mutate(
                                        stimID=as.integer(text),#converting to the recommended integer-coding format for matrix representation
                                        canon_status=ifelse(canon_status,1,2)
                                    )%>%
    select(stimID,canon_status,is_acceptable,is_grammatical)%>%
    gather(key="ratingtype",value="rating",is_acceptable:is_grammatical)%>%
    mutate(ratingtype=ifelse(ratingtype=="is_acceptable",1,2))%>%
    as.matrix

##ppnt_general
## I don't think this is actually what we want? But I might be wrong!
##This is the most pedantic/literal response, so I feel like it should be available here somewhere
##Participant ID; Between-participant condition (set to 1 for everyone); DV (1=acc vs. 2=gram ratings); then 10 columns for sentence type x grammaticality status
ppnt_general_stimtype <- responses.df%>%
    mutate(betweenppntcond = 1,conditioncol=paste0(stim_type,ifelse(canon_status,"_grammatical","_ungrammatical")))%>%
    spread(key=conditioncol,value=response)%>%
    select(ppntID,betweenppntcond,questiontext,agreementattraction_grammatical:NPI_grammatical)%>%
    mutate(ppntID=as.numeric(as.factor(ppntID)), questiontext=ifelse(questiontext=="is_acceptable",1,2))%>%
    group_by(ppntID,questiontext)%>%summarize_all(funs(mean),na.rm=TRUE)%>%
    as.matrix

##Although randomization over stim_type x canon_staus, I'm claiming above that the most interesting level-of-abstraction is actually item_type.
##The current randomization scheme isn't terrible, especially if you consider sentences within a 'stim_type' to be similar, which they mostly are.
##On the other hand, under this scheme if you create something like ppnt_general but for item_type you get a sprinkling of NaN's for missed cells.
ppnt_general_itemtype <- responses.df%>%
    mutate(betweenppntcond = 1,conditioncol=paste0(item_type,ifelse(canon_status,"_grammatical","_ungrammatical")))%>%
    spread(key=conditioncol,value=response)%>%
    select(ppntID,betweenppntcond,questiontext,control_grammatical:valid_singular_grammatical)%>%
    mutate(ppntID=as.numeric(as.factor(ppntID)), questiontext=ifelse(questiontext=="is_acceptable",1,2))%>%
    group_by(ppntID,questiontext)%>%summarize_all(funs(mean),na.rm=TRUE)%>%
    as.matrix

##clean up workspace
rm(list=toclear)
