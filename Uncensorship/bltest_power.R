setwd("~/Censorship/Uncensorship/")
library(tidyverse)
library(brms)
options(mc.cores=4)

power_models <- readRDS('./models/bltest_models.RDS')
models <- readRDS('./models/blmodel.RDS')

###################################################

# test. Replace 'bin' with the actual binary outcome.
# optional add weights.

df <- data.frame(days = models$bln$d1,
                 bin = as.numeric(rbernoulli(nrow(models$bln),.86)),
                 weights=1)

#######################################################

power <- function(x,y){
  print(paste('Computing power: ',c(x,y)))
  
  df$bin <- df$days %>% 
    plnorm(meanlog=x,sdlog=y) %>% 
    rbernoulli(length(.),p=.) %>% 
    as.numeric
  
  bernlognorm_power <- update(power_models$bernlognorm,newdata=df)
  bern_null_power <- update(power_models$bern_null,newdata=df)
  
  list(grid=c(x,y),
       ldens=dnorm(x,4.35,.14,log=TRUE) + dnorm(y,3.08,.13,log=TRUE) - log(1-pnorm(0,3.08,.13))
       post_probs=post_prob(bernlognorm_power,
                            bern_null_power))
 }

sims <- 50

x <- c()
for(i in rnorm(sims,3.08,.13)){
    while(i < 0) i <- rnorm(1,3.08,.13)
    x <- c(x,i)
}

seqs <- data.frame(mu=rnorm(sims,4.35,.14),
		   sigma=x)

power_seq <- map2(.x=seqs[,1],.y=seqs[,2],.f=power)

saveRDS(power_seq,file='bltest_power_grid_focused.RDS')
#############################################################
