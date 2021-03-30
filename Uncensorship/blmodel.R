setwd("~/Thesis")
source('blclean.R')

library(brms)
options(mc.cores=4)

models <- function(family, sample_prior='no'){
    form <- brmsformula(y|cens(cens,ub)+weights(weights)~1,family=family)
    br <- brm(form,bld,
              sample_prior = sample_prior,
              save_pars = save_pars(all = TRUE))
    br
}

brex <- models('exponential')
brwe <- models('weibull')
brln <- models('lognormal')

pprobs <- post_prob(brex,brwe,brln)

brln_prior <- models('lognormal', sample_prior='only')

saveRDS(list(pprobs=pprobs,
             brex=brex,
             brwe=brwe,
             brln=brln,
             brln_prior=brln_prior,
             bld=bld,
             bln=bln),
             file='blmodel.RDS')

######################################################