library(tidyverse)
library(patchwork)
library(janitor)

rm(list = ls()) #not recommended, remove before sharing. :-P
theme_set(theme_light())

demographics.df <- read.csv("raw/demographicsdata.csv",
                            stringsAsFactors = FALSE) %>%
    clean_names #may need to add a final newline to last line of raw download

responses.df <- read.csv("raw/sentenceresponsedata.csv",
                         stringsAsFactors = FALSE) %>%
    clean_names %>%
    group_by(ppnt_id) %>% arrange(response_time) %>% #responses from different ppnts may be interleaved in raw. Group and arrage allows calculation of deliberation time from adjacent response times. Note there are spacer/prompt stim fronting question blocks, the continue button on those 'starts the clock' for the first actual stim: first spacer screen gets a deliberation time of 0.
    mutate(deliberation_time =
               response_time - lag(response_time, default = response_time[1]),
           norm_time = (deliberation_time - mean(deliberation_time)) /
               sd(deliberation_time) #standardize reading times. 
           ) %>%
    ungroup() %>%
    filter(response != "continue") %>% #removes those spacer screen response rows.
    mutate(response = as.numeric(as.character(response)),
           textnchars = nchar(as.character(text)), #in characters, so includes spaces/punctuation. Is wordcount better/different? Probably?
           wordcount = sapply(strsplit(as.character(text), " "), length), #hack assuming no double spaces or one word sentences.
           questiontext = sapply(questiontext, function(x){
               mywords <- str_split(x, " ")[[1]]
               return(mywords[length(mywords)])
           })#more script/plot friendly than the verbose full version
           )

##Exclusion criteria:
badids <- data.frame()
## Demographics exclusions
badids <- demographics.df %>%
    filter(age <= 0) %>%
    select(ppnt_id) %>%
    mutate(reason = "bad_age") #clearly not cooperating.

badids <- badids %>%
    rbind(demographics.df %>% filter(!str_detect(language, "en")) %>%
          select(ppnt_id) %>% mutate(reason = "language")) #Reported non english speaker. Admits misspellings though.
##Super fast responses are bots or clickthru.
timetaken.df <- responses.df %>% group_by(ppnt_id) %>%
    summarize(begin_time = min(response_time),
              end_time = max(response_time),
              total_time = (end_time - begin_time) / 1000 / 60)
##response exclusions
badids <- badids %>%
    rbind(timetaken.df %>% filter(total_time < quantile(total_time, .1)) %>%
          select(ppnt_id) %>% mutate(reason = "veryfast")) #Check the histogram of response times, ggplot(timetaken.df,aes(x=totalTime))+geom_histogram(binwidth=1)+geom_vline(aes(xintercept=4)).Using a quantile involves betting on the % spamminess of MTurk, as opposed to betting on guessing people's reading times. The particular quantile used here is based on the responses to exp1.

##incomplete:
responsecount <- responses.df %>% group_by(ppnt_id) %>%
    summarize(count = n())
badids <- badids %>%
    rbind(responsecount %>%
          filter(count < 76) %>% ##Magic number. Some id's were NA, localstorage failed? Be careful of anything that uses ppnt_id (norm time is one)
          select(ppnt_id) %>%
          mutate(reason = "incomplete")
          ) #Assumes max count == completion

##attention checks
attncheck_goodsentence <- responses.df %>%
    filter(text == "Sarah expected to get a good grade.") %>%
    select(ppnt_id, response)

attncheck_badsentence <- responses.df %>%
    filter(text == "Him would have been fired.") %>%
    select(ppnt_id, response)

badids <- badids %>%
    rbind(attncheck_goodsentence %>%
          filter(response < 4) %>%
          select(ppnt_id) %>%
          rbind(attncheck_badsentence %>%
                filter(response > 1) %>%
                select(ppnt_id)) %>%
          distinct %>%
          mutate(reason = "attncheck")
          )

##Apply exclusions:
pre_exclusion.df <- responses.df
    
responses.df <- responses.df %>%
    filter(!(ppnt_id %in% badids$ppnt_id))

##remove attn check items
responses.df <- responses.df %>%
    filter(!(text %in% c("Sarah expected to get a good grade.",
           "Him would have been fired.")))


stiminfo.df <- read.csv("exp2_stim/adger_cgi_listversion.csv",
                        stringsAsFactors = FALSE)
responses.df <- left_join(responses.df, stiminfo.df, by = "text") %>%
    rename(stimsource = "source") #don't use a keyword as a var name, chump.

##handy summary df's
item_responsecount.df <- responses.df %>%
    group_by(text) %>% summarize(count = n())
