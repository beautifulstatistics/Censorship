setwd("~/Censorship/Uncensorship/")
source('blclean.R')

library(tidyverse)
library(xtable)

isemp <- function(x) replace(x, x == "", "E")
isE <- function(x) replace(x, x == "E", "")
pas <- function(x) paste0(x,collapse= "")

counts <- bl %>%
    select(-zh,-en,-link,-remarks) %>%
    select(starts_with('r')) %>%
    mutate(r2 = ifelse(r2 == '' & r3 != '','Y',r2)) %>%
    mutate_all(isemp) %>%
    rowwise %>%
    mutate(rows = pas(across(everything())))

counts$zh = bl$zh
counts %>%
    filter(rows == "YYYNYN")

counts2 <- counts %>%
    group_by(rows) %>%
    summarise(counts = n()) %>%
    arrange(desc(counts))

counts2 %>%
    select(rows) %>%
    rowwise %>%
    pull %>%
    strsplit("") %>%
    do.call(rbind,.) %>%
    data.frame %>%
    tibble %>%
    mutate(across(everything(),isE)) %>%
    mutate(counts = counts2$counts) %>%
    data.frame %>%
    xtable %>%
    print(include.rownames=FALSE)


models <- readRDS('blmodel.RDS')

bld <- models$bld
bln <- models$bln


