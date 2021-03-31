setwd("~/Censorship/Uncensorship/")
library(tidyverse)
library(brms)
options(mc.cores=4)


power_models <- readRDS('bltest_power.RDS')

models <- readRDS('blmodels.RDS')

###################################################

# test. Replace 'bin' with the actual binary outcome.
# optional add weights.

df <- data.frame(days = models$bln$d1,
                 bin = as.numeric(rbernoulli(nrow(models$bln),.86)),
                 weights=1)

#######################################################

power <- function(p){
  print(paste('Computing power: ',p))
  df$bin <- as.numeric(rbernoulli(nrow(df),p))
  
  bernlognorm_power <- update(bernlognorm,newdata=df)
  bernlognorm_null_power <- update(bernlognorm_null,newdata=df)
  bern_null_power <- update(bern_null,newdata=df)
  c(p,post_prob(bernlognorm_power,bernlognorm_null_power,bern_null_power))
}

power_seq <- map(seq(.05,.95,.05),power)

saveRDS(power_seq,file='bltest_power.RDS')
#############################################################
