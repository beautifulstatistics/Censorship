setwd("~/Censorship/Uncensorship/")
library(tidyverse)
library(brms)
options(mc.cores=4)

models <- readRDS('blmodel.RDS')

###################################################

# test. Replace 'bin' with the actual binary outcome.
# optional add weights.

df <- data.frame(days = models$bln$d1,
                 bin = as.numeric(rbernoulli(nrow(models$bln),.86)),
                 weights=1)

#######################################################

# https://cran.r-project.org/web/packages/brms/vignettes/brms_customfamilies.html

bern_lognormal <- custom_family(
  "bern_lognormal", dpars = c("mu", "sigma"),
  links = c("identity", "log"), lb = c(NA, 0),
  type = "int", vars = "vint1[n]"
)

stan_funs <- "
  real bern_lognormal_lpmf(int y, real mu, real sigma, int T) {
    return bernoulli_lpmf(y | lognormal_cdf(T , mu, sigma));
  }
"

stanvars <- stanvar(scode = stan_funs, block = "functions")

form <- bf(bin|vint(days)+weights(weights)~0+Intercept,family=bern_lognormal)

prior <- c(prior('normal(4.35,.14)',coef=Intercept),
           prior('normal(3.08,.13)',class=sigma))

prior_null <- c(prior('student_t(3, 0, 2.5)',coef=Intercept),
           prior('student_t(3, 0, 2.5)',class=sigma))

bernlognorm <- brm(form, 
                   data = df, 
                   prior=prior,
                   iter=40000,
                   family = bern_lognormal,
                   save_pars=save_pars(all=TRUE),
                   stanvars = stanvars)

bernlognorm_null <- brm(form, 
                        data = df, 
                        prior=prior,
                        iter=40000,
                        family = bern_lognormal,
                        save_pars=save_pars(all=TRUE),
                        stanvars = stanvars)

bern_null <- brm(bin~0+Intercept, 
                 data = df,
                 iter=40000,
                 prior=prior('student_t(3, 0, 2.5)',coef=Intercept),
                 family=bernoulli,
                 save_pars=save_pars(all=TRUE))

saveRDS(list(bernlognorm=bernlognorm,bernlognorm_null=bernlognorm_null,bern_null=bern_null),file='bltest_models.RDS')
