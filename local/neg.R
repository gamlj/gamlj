require(boot)
require(MASS)
library(readxl)

################################################################################
# importazione dati 
################################################################################
nb<-openxlsx::readWorkbook("~/Downloads/nb.xlsx")
################################################################################
# parametric bootstrap non-nested models
################################################################################

formula = 'err~group'
fit_pois = glm(formula, data = nb, family = 'poisson')
summary(fit_pois)
fit_nb = glm.nb(formula, data = nb)

fit_nb$theta
fit_nb$linear.predictors

summary(fit_nb)
fit_norm = lm(formula, data = nb)
summary(fit_norm)


ln_nb = logLik(fit_nb)
ln_pois = logLik(fit_pois)
ln_norm = logLik(fit_norm)

mle_nb = c(coef(glm.nb(formula, data = nb)))
mle_pois = c(coef(glm(formula, data = nb, family = 'poisson')))

t_obs = 2*(ln_nb-ln_pois)
t_obs1 = 2*(ln_pois-ln_nb)
t_obs3 = 2*(ln_norm-ln_nb)

f_boot = function(data){
  ln_nb = logLik(glm.nb(formula, data = data))
  ln_pois = logLik(glm(formula, data = data, family = 'poisson'))
  return(2*(ln_nb-ln_pois))
}

f_boot1 = function(data){
  ln_nb = logLik(glm.nb(formula, data = data))
  ln_pois = logLik(glm(formula, data = data, family = 'poisson'))
  return(2*(ln_pois-ln_nb))
}

f_boot2 = function(data){ 
  ln_nb = logLik(glm.nb(formula, data = data))
  ln_norm = logLik(lm(formula, data = data))
  return(2*(ln_norm-ln_nb))
}

f_rng_nb = function(data, mle){
  out = data
  out$n = rnbinom(nrow(data),mu = exp(mle[1]+mle[2]*data$group+mle[3]), 
                  size = mle[4])
  return(out)
}

a<-f_rng_nb(nb,mle_nb)

f_rng_pois = function(data, mle){
  out = data
  print(data$group)
  out$n = rpois(nrow(data), exp(mle[1]+mle[2]*data$group+mle[3]))
  return(out)
}


set.seed(123)
# h0: negbin
boot_nv = boot(data = nb, statistic = f_boot1, sim = 'parametric', 
               R = 10, ran.gen = f_rng_nb, mle = mle_nb)
warningplot(boot_nv)
pval1 = (1+sum(boot_nv$t >= boot_nv$t0))/(1+boot_nv$R)
warnings()
set.seed(123)
# h0: negbin - rejection for small values of t0
boot_nvbis = boot(data = nv, statistic = f_boot, sim = 'parametric', 
                  R = 10^3, ran.gen = f_rng_nb, mle = mle_nb)
plot(boot_nvbis)
pval1bis = (1+sum(boot_nvbis$t < boot_nvbis$t0))/(1+boot_nvbis$R)

# h0: pois
boot_nv2 = boot(data = nv, statistic = f_boot, sim = 'parametric', 
                R = 10^3, ran.gen = f_rng_pois, mle = mle_pois)
plot(boot_nv2)
pval2 = (1+sum(boot_nv2$t >= boot_nv2$t0))/(1+boot_nv2$R)

#h0: negbin vs norm
boot_nv3 = boot(data = nv, statistic = f_boot2, sim = 'parametric', 
                R = 10^3, ran.gen = f_rng_nb, mle = mle_nb)
plot(boot_nv3)
pval3 = (1+sum(boot_nv3$t >= boot_nv3$t0))/(1+boot_nv3$R)
################################################################################

print(mle_nb)
