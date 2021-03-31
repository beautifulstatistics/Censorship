setwd("~/Censorship/Uncensorship/")
source('blclean.R')

library(brms)
options(mc.cores=4)

models <- function(family){
    form <- brmsformula(y|cens(cens,ub)+weights(weights)~1,family=family)
    br <- brm(form,bld,iter=40000,
              save_pars = save_pars(all = TRUE))
    br
}

brex <- models('exponential')
brwe <- models('weibull')
brln <- models('lognormal')

pprobs <- post_prob(brex,brwe,brln)

saveRDS(list(pprobs=pprobs,
             brex=brex,
             brwe=brwe,
             brln=brln,
             bld=bld,
             bln=bln),
             file='blmodel.RDS')

######################################################