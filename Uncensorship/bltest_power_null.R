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

sims <- 1000

power <- function(x,n){
  print(paste0('Computing power: ',x))
  print(paste0('At simualtion: ',n))
  
  df$bin <- rbernoulli(nrow(df),p=x) %>% 
    as.numeric

  bernlognorm_power <- update(power_models$bernlognorm,newdata=df)
  bernlognorm_null_power <- update(power_models$bernlognorm_null,newdata=df)
  bern_null_power <- update(power_models$bern_null,newdata=df)

  print((Sys.time() - t1)/n*(sims-n))
  
  list(grid=c(x),
       post_lprobs=map(list(bernlognorm_power=bernlognorm_power,bernlognorm_null_power=bernlognorm_null_power,bern_null_power=bern_null_power),(function(x) bridge_sampler(x)$logml)))
 }


seqs <- list(x=rbeta(sims,1,1),
             n=1:sims)

t1 <- Sys.time()

power_seq <- pmap(seqs,.f=power)

saveRDS(power_seq,file='bltest_power_null.RDS')
#############################################################
# map(1:100,function(x) mm[[x]]$post_prob[1]) %>% unlist %>% sum %>% (function(x){ x/100})
