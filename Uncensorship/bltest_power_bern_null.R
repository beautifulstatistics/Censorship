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

power <- function(x,y,n){
  print(paste0('Computing power: ',x,'::::',y))
  print(paste0('At simualtion: ',n))
  
  df$bin <- df$days %>% 
    plnorm(meanlog=x,sdlog=y) %>% 
    rbernoulli(length(.),p=.) %>% 
    as.numeric

  bernlognorm_power <- update(power_models$bernlognorm,newdata=df)
  bernlognorm_null_power <- update(power_models$bernlognorm_null,newdata=df)
  bern_null_power <- update(power_models$bern_null,newdata=df)

  print((Sys.time() - t1)/n*(sims-n))
  
  list(grid=c(x,y),
       post_lprobs=map(list(bernlognorm_power=bernlognorm_power,bernlognorm_null_power=bernlognorm_null_power,bern_null_power=bern_null_power),(function(x) bridge_sampler(x)$logml)))
 }


y <- c()
for(i in (rt(sims,3) * 2.5)){
    while(i < 0) i <- (rt(1,3) * 2.5)
    y <- c(y,i)
}

seqs <- list(x=(rt(sims,3) * 2.5),
             y=y,
             n=1:sims)

t1 <- Sys.time()

power_seq <- pmap(seqs,.f=power)

saveRDS(power_seq,file='bltest_power_bern_null.RDS')
#############################################################
# map(1:100,function(x) mm[[x]]$post_prob[1]) %>% unlist %>% sum %>% (function(x){ x/100}) = .968
