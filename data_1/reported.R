library(tidyverse)
library("BayesFactor")
source("readData.R")

##Setup and prettification:

saveplots <- FALSE
##prettify theme
theme_set(theme_light() +
          theme(strip.text.x = element_text(size = 20)))

##used to relabel factor levels to more reporting-friendly versions
from_items <- c("control",
                "full",
                "illusion",
                "linguistsNO_ppntsYES",
                "linguistsYES_ppntsNO",
                "missing1",
                "missing2",
                "missing3",
                "natural_error",
                "Partial match NPI",
                "Unlicensed NPI",
                "unnatural_error",
                "Valid NPI",
                "valid_plural",
                "valid_singular")
to_items <- c("control",
              "full embedding",
              "comparison illusion",
              "high rated nonconforming",
              "low rated conforming",
              "missing VP-1",
              "missing VP-2",
              "missing VP-3",
              "singular-plural error",
              "Partial match NPI",
              "Unlicensed NPI",
              "plural-singular error",
              "Valid NPI",
              "plural agreement",
              "singular agreement")

from_stim <- c("agreementattraction",
              "comparison",
              "embedding",
              "expert_vs_ppnt_clash",
              "NPI")
to_stim <- c("agreement attraction",
           "comparison",
           "embedding",
           "Linguistic Inquiry",
           "NPI")


responses.df$item_type <- plyr::mapvalues(responses.df$item_type,
                                        from = from_items,
                                        to = to_items
                                        )

responses.df$stim_type <- plyr::mapvalues(responses.df$stim_type,
                                        from = from_stim,
                                        to = to_stim
                                        )
##REPORTED RESULTS

##Timing was slower for gram.
ttestBF(formula = normTime ~ questiontext,
        data = as.data.frame(responses.df))

t.test(formula = normTime ~ questiontext, data = as.data.frame(responses.df))

responses.df %>%
    filter(questiontext == "is_acceptable") %>%
    select(normTime) %>%
    summary

responses.df %>%
    filter(questiontext == "is_grammatical") %>%
    select(normTime) %>%
    summary

##Responses were more extreme.
responsehists.plot <-
    ggplot(responses.df, aes(x = response)) +
    geom_bar(position = "dodge") +
    facet_wrap(~questiontext) #+


if (saveplots){
    ggsave(responsehists.plot,
           file = "plots/responsehists_mono.png")
}

##Response difference under different instruction conditions.
acc.df <- responses.df %>%
    filter(questiontext == "is_acceptable") %>%
    group_by(text, item_type, stim_type) %>%
    summarize(meanrating = mean(response))


gram.df <- responses.df %>%
    filter(questiontext == "is_grammatical") %>%
    group_by(text, item_type, stim_type) %>%
    summarize(meanrating = mean(response))


compare.df <- left_join(acc.df, gram.df, by = "text") %>%
    rename(meanrating_acc = meanrating.x,
           meanrating_gram = meanrating.y,
           item_type = item_type.x,
           stim_type = stim_type.x) %>%
    select(-item_type.y, -stim_type.y) %>%
    mutate(diff = meanrating_acc - meanrating_gram)

## compare.df$item_type <- plyr::mapvalues(compare.df$item_type,
##                                         from = from_items,
##                                         to = to_items
##                                         )

## compare.df$stim_type <- plyr::mapvalues(compare.df$stim_type,
##                                         from = from_stim,
##                                         to = to_stim
##                                         )

diffviolins.plot <-
    ggplot(compare.df, aes(x = item_type, y = diff)) +
    geom_violin(aes(color = stim_type)) +
    geom_point( data = compare.df %>%
                    group_by(item_type, stim_type) %>%
                    summarize(meandiff = mean(diff, na.rm = TRUE)),
               aes(x = item_type, y = meandiff)) +
    geom_hline(aes(yintercept = 0)) +
    facet_wrap(stim_type~., scales = "free") +
    guides(color = FALSE) +
    ylab("") + xlab("") +
    coord_flip()

if (saveplots){
    ggsave(diffviolins.plot,
           file = "plots/diffviolins.png",
           width = 13)
    }

item_means.df <- responses.df %>%
    group_by(item_type, questiontext, stim_type) %>%
    summarize(meanrating = mean(response),
              ci_low = meanrating + qnorm(.025) * sd(response) / sqrt(n()),
              ci_high = meanrating + qnorm(.975) * sd(response) / sqrt(n())
              )


## item_means.df$item_type <- plyr::mapvalues(item_means.df$item_type,
##                                         from = from_items,
##                                         to = to_items
##                                         )

## item_means.df$stim_type <- plyr::mapvalues(item_means.df$stim_type,
##                                         from = from_stim,
##                                         to = to_stim
##                                         )


instructiondiff_pointbar.plot <-
    ggplot(item_means.df, aes(x = item_type,
                         y = meanrating,
                         color = questiontext,
                         group = item_type)) +
        geom_line(size = 3, color = "black") +
    geom_point(size = 5, aes(shape = questiontext)) +
    geom_errorbar(aes(ymin = ci_low, ymax = ci_high), width = .5) +
    ##    facet_grid(stim_type~., scales = "free") +
    facet_wrap(stim_type~., scale = "free") +
    xlab("") + ylab("Mean rating") +
    coord_flip()

##Omnibus t.test loop. Hailstom of parallel tests is bad?
ttesttable <-
    "\\begin{center}
\\begin{tabular}{ l l c c r}
 Phenomenon & item type & Mean acceptability rating & Mean grammaticality rating & t-test \\\\
\\hline"

for (astim in unique(responses.df$stim_type)){
    for (anitem in unique(filter(responses.df, stim_type == astim)$item_type)){
        print(paste(astim, anitem, sep = ":"))
        my_responses <- responses.df %>%
            filter(stim_type == astim,
                   item_type == anitem)

        mytest <- t.test(formula = response ~ questiontext, data = my_responses)
        sigfigs <- 2
        t_description <- paste(
            c("$t_{df=",signif(mytest$parameter, sigfigs),"}=",
                           signif(mytest$statistic, sigfigs),
                           ",p = ",signif(mytest$p.value, 2),"$"
              ), collapse = "")
        
        myrow <- paste(
            c(astim, "&",
                   anitem, "&",
                   signif(mytest$estimate[1], sigfigs), "&",
                   signif(mytest$estimate[2], sigfigs), "&",
                   ifelse(mytest$p.value < .05, t_description, "No difference")
              ), collapse = " ")
        
        ttesttable <- paste(ttesttable, paste(myrow, "\\\\"))
    }
}

ttesttable <- paste(ttesttable,
"\\end{tabular}
\\end{center}")


#


if (saveplots){
    ggsave(instructiondiff_pointbar.plot,
           file = "plots/instructiondiff_pointbar.png",
    width = 13)
}


##State trace analysis
ratingtypes.df <- responses.df %>%
    group_by(item_type, questiontext, stim_type) %>%
    summarize(meanrating = mean(response)#,
#              ci_low = meanrating + qnorm(.025) * sd(response) / sqrt(n()),
#              ci_high = meanrating + qnorm(.975) * sd(response) / sqrt(n())
              ) %>%
    ungroup() %>%
    spread(questiontext, meanrating)

## ratingtypes.df$item_type <- plyr::mapvalues(ratingtypes.df$item_type,
##                                         from = from_items,
##                                         to = to_items
##                                         )

## ratingtypes.df$stim_type <- plyr::mapvalues(ratingtypes.df$stim_type,
##                                         from = from_stim,
##                                         to = to_stim
##                                         )


sta.plot <-
    ggplot(ratingtypes.df,
           aes(x = is_grammatical,
               y = is_acceptable,
               shape = stim_type
               )) +
    geom_point(size = 5) +
    xlab("Rating when asked `is this grammatical'") +
    ylab("Rating when asked `is this acceptable'") +
    theme(axis.title.x = element_text(size = 20),
          axis.title.y = element_text(size = 20),
          legend.title = element_text(size = 20)
          ) +
    scale_shape_discrete(name = "Phenomenon")

if (saveplots){
    ggsave(sta.plot,
           file = "plots/staplot.png",
    width = 10)
    }
