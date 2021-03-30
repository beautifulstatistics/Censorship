library(tidyverse)
library(brms)

setwd("~/Thesis")

models <- readRDS('blmodel.RDS')

pred <- posterior_predict(models$brln,nsamples=10) %>%
  sort

prob <- models$bln %>%
  select(d1) %>%
  mutate(d1 = as.integer(Sys.Date() - as.Date(d1))) %>%
  mutate(d1 = findInterval(d1,pred)) %>%
  mutate(d1 = d1/length(pred)) %>%
  pull


###################################################

# test. Replace 'out' with the actual binary outcome.
# optional add weights.

df <- data.frame(out = rbernoulli(length(prob),prob),
                 prob)

logit <- function(x) log(x/(1-x))

br_model <- brm(out~0 + logit(datedt),df,
          family=bernoulli(link='logit'),
          prior=prior('std_normal()',coef='logitdatedt'),
          sample_prior='yes',
          iter=60000,
          save_pars=save_pars(all=TRUE))

br_null <- brm(out~0 + Intercept,df,
          family=bernoulli(link='logit'),
          prior=prior('std_normal()',class=b),
          sample_prior='yes',
          iter=60000,
          save_pars=save_pars(all=TRUE))

post_prob(br_model,br_null)

# as a secondary test:

