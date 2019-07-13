library(tidyverse)
library("BayesFactor")
source("readData.R")

saveplots <- TRUE
##prettify theme
theme_set(theme_light() +
          theme(strip.text.x = element_text(size = 20)))

##Timing was slower for gram.
ttestBF(formula = normTime ~ questiontext,
        data = as.data.frame(responses.df))

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

compare.df$item_type <- plyr::mapvalues(compare.df$item_type,
                                        from =
                                            c("control",
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
                                              "valid_singular"),
                                        to = c("control",
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
                                        )


compare.df$stim_type <- plyr::mapvalues(compare.df$stim_type,
                                        from = c("agreementattraction",
                                                 "comparison",
                                                "embedding",
                                                "expert_vs_ppnt_clash",
                                                "NPI"),
                                        to = c("agreement attraction",
                                               "comparison",
                                               "embedding",
                                                "Linguistic Inquiry",
                                               "NPI")
                                        )

diffviolins.plot <-
    ggplot(compare.df, aes(x = item_type, y = diff)) +
    geom_violin(aes(color = stim_type)) +
    geom_point( data = compare.df %>%
                    group_by(item_type, stim_type) %>%
                    summarize(meandiff = mean(diff, na.rm = TRUE)),
               aes(x = item_type, y = meandiff)) +
    geom_hline(aes(yintercept = 0)) +
    facet_wrap(stim_type~., scales = "free") +
    coord_flip() +
    guides(color = FALSE) +
    xlab("") + ylab("")

if (saveplots){
    ggsave(diffviolins.plot,
           file = "plots/diffvioins.png",
           width = 13)
    }
