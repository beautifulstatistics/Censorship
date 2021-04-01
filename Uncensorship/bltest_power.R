setwd("~/Censorship/Uncensorship/")
library(tidyverse)
library(brms)
options(mc.cores=4)


power_models <- readRDS('bltest_models.RDS')

models <- readRDS('blmodel.RDS')

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
  
  bernlognorm_power <- update(power_models$bernlognorm,newdata=df)
  bernlognorm_null_power <- update(power_models$bernlognorm_null,newdata=df)
  bern_null_power <- update(power_models$bern_null,newdata=df)
  c(p,post_prob(bernlognorm_power,bernlognorm_null_power,bern_null_power))
}

# power_seq <- map(seq(.80,.95,.01),power)
power_seq <- readRDS('bltest_power.RDS')
power_seq[[7]] = power(.86)

saveRDS(power_seq,file='bltest_power.RDS')
#############################################################
