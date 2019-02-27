library(tidyverse)

n_items <- 100

get_items <- function(n){
    return(rbeta(n,0.5, 0.5))
}

