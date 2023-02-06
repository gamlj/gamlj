library(gamlj)
testthat::context("model comparisons")
tol=.001
data("glmmixeddata")
data<-glmmixeddata

data$cluster<-factor(data$cluster)
data$w<-factor(data$w)
data$yord<-factor(data$yord)
data$ybin<-factor(data$ybin)

options(digits = 3)

mod0 <- gamlj::gamljGlm(
  formula = ycont~x,
  data = data
)
mod1 <- gamlj::gamljGlm(
  formula = ycont~x*w,
  simple_x=x,
  simple_mods=w,
  data = data,
  plot_x=x,
  qq_plot = T
)
mod1$simpleEffects
simple_effects(mod1,simple_x = "x",simple_mods = "w")
simple_effects(mod1)
anova(mod1,nested_terms=~x)
anova(mod1)

coef(mod1)
fit(mod1)
plot(mod1)
a<-assumptions(mod1)

mod1 <- gamlj::gamljGzlm(
  formula = ybin~x*w,
  simple_x=x,
  simple_mods=w,
  data = data,
  model_type="logistic"
)

c(mod1$main$fit,mod1$main$r2)

terms(~w*x)
all.vars(~w*x)
dd<-gamlj::get_data(mod0)

update(mod0,plot_x="x")
mod0$options$comparison
get_data(mod0)
class(mod0)
anova(mod0)
