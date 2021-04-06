setwd("~/Censorship/Uncensorship/")
source('blclean.R')

library(brms)
options(mc.cores=4)

models <- function(family){
    form <- bf(y|cens(cens,ub)+weights(weights)~0+Intercept,family=family)
    br <- brm(form,bld,iter=40000,
              save_pars = save_pars(all = TRUE))
    br
}

brnm <- models('normal')
brex <- models('exponential')
brwe <- models('weibull')
brln <- models('lognormal')

pprobs <- post_prob(brnm,brex,brwe,brln)

saveRDS(list(pprobs=pprobs,
             brnm=brnm,
             brex=brex,
             brwe=brwe,
             brln=brln,
             bld=bld,
             bln=bln),
             file='blmodel.RDS')

#####################################################3

