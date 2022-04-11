
set.seed(1839)
n <- 150
x1 <- rnorm(n)
x2 <- runif(n)

b01 <- 1.5
b02 <- .4
b03 <- -.4
b04 <- -1.5
b1 <- 0.1
b2 <- 0.2
b3 <- 0.8
b4 <- 0.9

b2 <- 0.5

logodds1 <- b01 + b1 * x1 + b2 * x2
logodds2 <- b02 + b2 * x1 + b2 * x2
logodds3 <- b03 + b3 * x1 + b2 * x2
logodds4 <- b04 + b4 * x1 + b2 * x2

inv_logit <- function(logit) exp(logit) / (1 + exp(logit))
prob_2to5 <- inv_logit(logodds1)
prob_3to5 <- inv_logit(logodds2)
prob_4to5 <- inv_logit(logodds3)
prob_5 <- inv_logit(logodds4)

prob_1 <- 1 - prob_2to5
prob_2 <- prob_2to5 - prob_3to5
prob_3 <- prob_3to5 - prob_4to5
prob_4 <- prob_4to5 - prob_5

table(c(prob_1, prob_2, prob_3, prob_4, prob_5) > 0)

y <- c()
for (i in 1:n) {
  y[i] <- sample(
    x = c(1:5), 
    size = 1, 
    prob = c(prob_1[i], prob_2[i], prob_3[i], prob_4[i], prob_5[i])
  )
}
dat <- data.frame(x1, x2, y = factor(y))
 
mod1<-MASS::polr(y ~ x1 + x2, data = dat)
 
form<-y ~ x1 + x2
dat$y<-factor(dat$y)
mod2<-nnet::multinom(form,data=dat)

mm<-emmeans::emmeans(mod1, ~x1*y,mode="prob",  data = dat,at=list(x1=c(pretty(dat$x1,n = 20))))

mm<-as.data.frame(mm)

library(ggplot2)
ggplot(mm,aes(x=x1,y=prob,color=y))+geom_line()

mm<-emmeans::emmeans(mod2, ~x1*y,mode="prob",  data = dat,at=list(x1=c(pretty(dat$x1,n = 20))))

mm<-as.data.frame(mm)

ggplot(mm,aes(x=x1,y=prob,color=y))+geom_line()



library(ordinal)
options(contrasts = c("contr.treatment", "contr.poly"))

## A tabular data set:
(tab26 <- with(soup, table("Product" = PROD, "Response" = SURENESS)))
dimnames(tab26)[[2]] <- c("Sure", "Not Sure", "Guess", "Guess", "Not Sure", "Sure")
dat26 <- expand.grid(sureness = as.factor(1:6), prod = c("Ref", "Test"))
dat26$wghts <- c(t(tab26))

m1 <- clm(sureness ~ prod, data = dat26,
           weights = wghts, link = "logit")

m1$coefficients
model<-m1
parameters::parameters(m1)

deviance(m2)

#https://cran.r-project.org/web/packages/ordinal/vignettes/clm_article.pdf
nominal_test(m1)
xdat26<-dat26
m2 <- MASS::polr(sureness ~ prod, data = xdat26,
          weights = wghts)
xdat26<-NULL
update(m2,data=dat26)
summary(m1)
summary(m2)
mm<-emmeans::emmeans(m1, ~prod*sureness,mode="prob")
mm
## print, summary, vcov, logLik, AIC:
m1
summary(m1)
vcov(m1)
logLik(m1)
AIC(m1)
coef(m1)
coef(summary(m1))


## Fit cumulative link model:
fm <- clm(rating ~ temp + contact, data=wine)
summary(fm)
## test partial proportional odds assumption for temp and contact:
nominal_test(fm)
## no evidence of non-proportional odds.
## test if there are signs of scale effects:
scale_test(fm)
## no evidence of scale effects.

## tests of scale and nominal effects for the housing data from MASS:
if(require(MASS)) {
  fm1 <- clm(Sat ~ Infl + Type + Cont, weights = Freq, data = housing)
  scale_test(fm1)
  nominal_test(fm1)
  ## Evidence of multiplicative/scale effect of 'Cont'. This is a breach
  ## of the proportional odds assumption.
}

library(ordinal)
data<-read.csv2(file="data/hsbdemo.csv")
head(data)
data$cmath<-data$math-mean(data$math)
data$female<-factor(data$female)
data$ses<-factor(data$ses)

form<-ses ~ science
model <- ordinal::clm(form, data=data)
car::Anova(model)
nominal_test(model)
logLik(model)

anova(model,type = 3)
car::Anova(model,type=3)
model2 <- MASS::polr(form, data=data)
model02 <- MASS::polr(ses~1, data=data)
anova(model02,model2)
summary(model2)
car::Anova(model2)
anova(model2)
-2*logLik(model2)

summary(model2)
car::Anova(model2,type=3)
mmod<-nnet::multinom(form, data=data)
insight::get_data(mmod)
summary(mmod)
logLik(mmod)



### 

dat <- read.dta("https://stats.idre.ucla.edu/stat/data/ologit.dta")
head(dat)
#write.csv2(dat,file="../gamlj.github.io/data/ologit.csv",row.names = F)
library(MASS)
m <- polr(apply ~ pared + public + gpa, data = dat, Hess=TRUE)
p<-parameters::parameters(m,ci=.95)
as.data.frame(p)
## view a summary of the model
nnet::multinom(apply ~ pared + public + gpa, data = dat)

ff<-as.formula("apply ~ pared+public  + gpa")
model<-ordinal::clm(ff,data=dat)

a<-function() try_hard(warning("ciao"))

b<-function() try_hard(a())

b()

ff<-NULL
model$formulas
results<-as.data.frame(ordinal::nominal_test(model))
results
results<-results[rownames(results)!="<none>",]
names(results)<-c("df","loglik","aic", "test","p")
results$source<-rownames(results)
results
insight::get_data(mm)
performance::r2(mm)
class(mm)
mm$formula
mm$model

gmodel<-gamljGzlm(formula = apply ~ pared + public + gpa,
                  data=dat,
                  modelSelection = "ordinal",
                  propodds = T)
gmodel
