library(testthat)
N=100
b=0
tol=.00001
one<-function(N=100,b=2,tol=.0001) {
form<-y~a*z

x<-  rnorm(N)
z<- -rep(c(1,2),each=N/2)
#a<-  b*x+rnorm(N)
a<-  rep(c(1,2),N/2)
a<-  b*z+rnorm(N)
y<-  a+z+x+rnorm(N)

data<-as.data.frame(cbind(y,x,z,a))
data$z<-factor(data$z)
#data$a<-factor(data$a)
write.csv(data,file="extdata/sstest.csv")
model<-lm(form,data=data)
sumr<-summary(model)
f<-sumr$fstatistic[[1]]
dfres<-sumr$fstatistic[[3]]
dfmod<-sumr$fstatistic[[2]]
ssmod<-f*sigma(model)^2*dfmod/dfres
ca<-car::Anova(model,type=3)
gmod<-gamlj::gamljGlm(formula = form,data,effectSize = c("eta","etap","omega","omegap","epsilon","epsilonp"))

ganov<-gmod$main$anova$asDF
ganeff<-ganov[ganov$name!="Residuals" & ganov$name!="Model" & ganov$name!="Total",]
SS<-ganeff$ss
ssres<-ganov$ss[ganov$name=="Residuals"]
sstot<-ganov$ss[ganov$name=="Total"]
ssmod<-ganov$ss[ganov$name=="Model"]
df<-ganeff$df
dfres<-ganov$df[ganov$name=="Residuals"]
yss<-sum((data$y-mean(data$y))^2)
p<-predict(model)
sshat<-sum((p-mean(p))^2)
print(c(yss=sshat,mod=ssmod,sum=sum(SS)))
test_that("tot", {
  expect_equal(sstot,yss)
  expect_equal(sshat,ssmod)
})


test_that("app", {
  expect_equal(sstot,sum(SS)+ssres)
})

eta<-SS/(sstot)
test_that("eta", {
  expect_equal(eta,ganeff$etaSq)
})

etap<-SS/(SS+ssres)

test_that("etap", {
  expect_equal(etap,ganeff$etaSqP)
})

SS_star<-ganeff$df*ganeff$f*ssres/dfres
omega<-(SS-(ssres*df/dfres))/(ssmod+(ssres*(dfres+1)/dfres))
omega2<-(SS_star-(ssres*df/dfres))/(ssmod+(ssres*(dfres+1)/dfres))

test_that("omega", {
  expect_equal(omega,ganeff$omegaSq,tol=tol)
  expect_equal(omega2,ganeff$omegaSq,tol=tol)
  
})


omegap<-(SS-(ssres*df/dfres))/(SS+(ssres*(N-df)/dfres))
omegap2<-(ganeff$f-1)/(ganeff$f+(dfres+1)/df)
omegap3 <- (ganeff$f - 1) * df/(ganeff$f * df + dfres + 1)

test_that("omegap", {
  expect_equal(omegap,ganeff$omegaSqP,tol=tol)
})
test_that("omegap may fail", {
  expect_equal(omegap2,ganeff$omegaSqP,tol=.01)
})

test_that("omegap should fail", {
  expect_equal(omegap3,ganeff$omegaSqP,tol=.01)
})

epsilon<-(SS-(ssres*df/dfres))/(ssmod+ssres)
test_that("epsilon", {
  expect_equal(epsilon,ganeff$epsilonSq)
})


epsilonp<-(SS-(ssres*df/dfres))/(SS+ssres)
test_that("epsilonp", {
  expect_equal(epsilonp,ganeff$epsilonSqP)
})

}

one(N=20,b=0)

min(5:1, pi) #-> one number
pmin(5:1, pi) #->  5  numbers



 data<-read.csv("extdata/exercise.csv")
 mod<-lm(yendu~xage+zexer,data=data)
 summary(mod)
 ano<-car::Anova(mod,type=3)
 ee<-effectsize::omega_squared(ano,partial = F, ci_method="boot")
 as.data.frame(ee)
 
.get_ncp_F <- function(f, df, df_error, conf.level = 0.9) {
  alpha <- 1 - conf.level
  probs <- c(alpha / 2, 1 - alpha / 2)
  
  lambda <- f * df
  ncp <- suppressWarnings(stats::optim(
    par = 1.1 * rep(lambda, 2),
    fn = function(x) {
      p <- stats::pf(q = f, df, df_error, ncp = x)
      
      abs(max(p) - probs[2]) +
        abs(min(p) - probs[1])
    },
    control = list(abstol = 1e-09)
  ))
  f_ncp <- sort(ncp$par) / df
  
  if (f <= stats::qf(probs[1], df, df_error)) {
    f_ncp[2] <- 0
  }
  
  if (f <= stats::qf(probs[2], df, df_error)) {
    f_ncp[1] <- 0
  }
  
  return(f_ncp)
}

df<-ano$Df[2]
f<-ano$`F value`[2]
df_error<-ano$Df[rownames(ano)=="Residuals"]
as.data.frame(ee)
.anova<-ano[!rownames(ano) %in% c("(Intercept)","Residuals"),]
ee<-effectsize::omega_squared(ano,partial = F,ci = .95)
ee<-as.data.frame(ee)
res<-ee$Omega2
(f <- pmax(0, (res / df) / ((1 - res) / df_error)))
(f <- .anova$`F value`)
(ncp<-.get_ncp_F(f[1], df[1], df_error, conf.level = 0.95))

(co <- (ncp - 1) * df/(ncp * df + df_error + 1))
(omegap3 <- (f - 1) * df/(f * df + df_error + 1))
ee

library(lmtest)

aa<-lmtest::coeftest(model,vcov=sandwich::vcovHC(model, type="HC1"))
confint(aa)
as.data.frame(parameters::parameters(aa))
confint(model)

as.data.frame(parameters::parameters(model,bootstrap=TRUE,ci_method="bci",iterations=1000))

summary(model)
lmtest::coeftest(model, vcov = sandwich::vcovHC(model, type="HC1"))

d<-data.frame(y=factor(rep(c(0,1),10)),x=rnorm(20)/1000)
mod<-glm(y~x,data=d,family=binomial())
parameters::parameters(mod,standardize="refit")    
parameters::parameters(model,bootstrap=T,ci_method="bcai")    

