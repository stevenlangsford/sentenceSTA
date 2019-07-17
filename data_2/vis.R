library(tidyverse)
library(patchwork)
source("readData.R")

cgiresponses.df <- responses.df %>%
    filter(!(stimsource %in% c("agreementattraction", "embedding")))

##fun with cgi stuff: replication stim are a distraction for this.
itemmeans.df <- cgiresponses.df %>%
    group_by(text, questiontext, stimsource, structure_group) %>%
    summarize(meanrating = mean(response)) %>%
    ungroup()
 

acc_gram_meaning_density.plot <-
ggplot(itemmeans.df,
       aes(x = meanrating,
           color = stimsource)) +
    geom_density() +
    facet_grid(.~questiontext) +
ggplot(itemmeans.df,
       aes( x = meanrating,
           fill = stimsource)) +
    geom_histogram(position = "identity", alpha = .4) +
    facet_grid(stimsource~questiontext) +
    guides(fill = FALSE)

comparison.df <- itemmeans.df %>% spread(questiontext, meanrating) %>%
    rename(acceptable = "acceptable?",
           grammatical = "grammatical?",
           meaningful = "meaningful?")

acc_gram_hist <-
    ggplot(comparison.df, aes(x = acceptable - grammatical,
                          fill = stimsource)) +
    geom_histogram(position = "identity", alpha = .5) +
    ggplot(comparison.df, aes(x = acceptable - grammatical,
                              fill = stimsource == "cgi")) +
    geom_histogram(position = "identity", alpha = .5)

st_plots <-
    ggplot(comparison.df, aes(x = grammatical, y = acceptable)) +
    geom_point() + ggtitle("acceptable vs grammatical") +
    ggplot(comparison.df, aes(x = grammatical, y = meaningful)) +
    geom_point() + ggtitle("grammatical vs meaningful") +
    ggplot(comparison.df, aes(x = acceptable, y = meaningful)) +
    geom_point() +  ggtitle("acceptable vs meaningful")
