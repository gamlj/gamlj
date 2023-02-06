testthat::context("R interface")
tol<-0.001
library(ggplot2)
data("qsport")

obj<-gamlj::gamljGlm(
    formula = performance ~ hours+type,
    posthoc = ~type,
    data = qsport)


preds<-predict(obj)
reds<-residuals(obj)

testthat::test_that("test glm", {
  testthat::expect_equal(round(mean(preds),2),37.88)
  testthat::expect_equal(length(preds),100)
  testthat::expect_equal(mean(reds),0,tol)
})

sums<-summary(obj)

testthat::test_that("summary glm", {
  testthat::expect_equal(sums[[1]]$info[2],"Model")
  testthat::expect_equal(sums[[2]]$ar2,.408,tol)
  testthat::expect_equal(sums[[3]]$df[4],97)
})



newopt<-list(var="hours",type="standardized") 


qsport$z<-as.numeric(scale(qsport$performance))
zobj<-gamlj::gamljGlm(
  formula = z ~ hours,
  data = qsport,
  covs_scale =newopt)

cc<-zobj$main$coefficients$asDF  
testthat::test_that("standardizing", {
  testthat::expect_equal(cc$estimate[2],.1942,tol)
})

upd<-update(obj,covs_scale=newopt,es = c("beta", "etap", "omega"))
res1<-upd$main$coefficients$asDF
res2<-upd$main$anova$asDF

testthat::test_that("updating", {
  testthat::expect_equal(res1$estimate[2],1.4073,tol)
  testthat::expect_equal(res2$etaSqP[2],0.4147,tol)
})


data("hsbdemo")

mod<-gamlj::gamljGzlm(
  formula = prog ~ write +  ses*female,
  data = hsbdemo,
  estimates_ci = TRUE,
  plotHAxis = write,
  plotSepLines = ses,
  plotSepPlots = female,
  model_type =  "multinomial")

mod2<-update(mod,vcov=T)

vcov(mod2)
mplots<-plot(mod)

testthat::test_that("plot ok", {
                    testthat::expect_true(is.list(mplots))
                    testthat::expect_true(is.ggplot(mplots[[1]]))
}
)

data("subjects_by_stimuli")
names(subjects_by_stimuli)
mod<-gamlj::gamljMixed(
  formula = y ~ cond+( 1 | subj ),
  data = subjects_by_stimuli)

p0<-predict(mod)
p1<-predict(mod,random.only=T)



testthat::test_that("Mixed dots work", {
  testthat::expect_equal(mean(p0),19.6043,tol=tol)
  testthat::expect_equal(mean(p1),0,tol=tol)
})


data("schoolexam")
mod<-gamlj::gamljGlmMixed(
  formula = pass ~ 1 + math+( 1 | school ),
  data = schoolexam,
  plotHAxis = math,
  cimethod = "wald")

preds<-predict(mod)
n<-dim(gamlj::gamlj_data(mod))[1]
testthat::test_that("glmixed predict", {
  testthat::expect_equal(round(mean(preds),2),0.51)
  testthat::expect_equal(n,5041)
  
})

rmod0<-gamlj_model(mod)

rmod1<-lme4::glmer(formula = pass ~ 1 + math + (1 | school), family = binomial(logit), 
             data = schoolexam)




testthat::test_that("glmixed get model", {
  testthat::expect_equal(rmod0@optinfo$val[[1]],rmod1@optinfo$val[[1]], tolerance = .0001)
  testthat::expect_equal(rmod1@optinfo$val[[1]],mod$model@optinfo$val[[1]],tolerance = .0001)
})


mod1<-gamlj::gamljGlmMixed(
  formula = formula("pass ~ 1 + math+( 1 | school )"),
  data = schoolexam,
  plotHAxis = math,
  cimethod = "wald")


mplot<-plot(mod1)

testthat::test_that("plot ok", 
  testthat::expect_true(ggplot2::is.ggplot(mplot))
)
data("wicksell")
wicksell$time<-factor(wicksell$time)
wicksell$group<-factor(wicksell$group)

gobj<-gamlj::gamljMixed(
  formula = dv ~ 1 +group+ time:group+ time+( 1 | subj ),
  data = wicksell)

r1<-gamlj::posthoc(gobj)
r2<-gamlj::posthoc(gobj,formula=~group+group:time,postHocCorr=c("bonf","holm"))
tab<-r2[[2]]$asDF
testthat::test_that("posthoc function", {
                    testthat::expect_false(r1)
                    testthat::expect_equal(tab[6,8],6.86382,tolerance = tol)
                    testthat::expect_true(tab[3,4]==1)
}
)

gobj<-gamlj::gamljMixed(
  formula = dv ~ 1 +group+ time:group+ time+( 1 | subj ),
  data = wicksell)

r1<-gamlj::simple_effects(gobj)

r2<-simpleEffects(gobj,formula=~group:time,postHocCorr=c("bonf","holm"))
tab<-r2[[2]]$asDF

testthat::test_that("simple effect function", {
  testthat::expect_false(r1)
  testthat::expect_equal(tab[4,8],-1.4187,tolerance = tol)
}
)




data("subjects_by_stimuli")
subjects_by_stimuli$cond<-factor(subjects_by_stimuli$cond)
contrasts(subjects_by_stimuli$cond)<-contr.sum(2)/2
mod1<-gamlj::gamljMixed(
  formula =y ~ 1 + cond+( 1+cond|subj ),
  data = subjects_by_stimuli,
  randHist=T
  
)

res<-gamlj::gamlj_assumptionsPlots(mod1)

testthat::test_that("assumptions plots are there", {
  testthat::expect_equal(length(res),2)
  testthat::expect_equal(res[[1]]$name,"randHist1")
  testthat::expect_true(ggplot2::is.ggplot(res[[2]]$plot))
})



mod<-gamlj::gamljMixed(
  formula =y ~ 1 + cond+( 1|subj ),
  data = subjects_by_stimuli
)

preds<-predict(mod)
n<-dim(gamlj::gamlj_data(mod))[1]

testthat::test_that("mixed predict", {
  testthat::expect_equal(round(mean(preds),2),19.6)
  testthat::expect_equal(n,3000)
  
})


rmod0<-gamlj::gamlj_model(mod)

rmod1<-lme4::lmer(
  formula =y ~ 1 + cond+( 1|subj ),
  data = gamlj_data(mod),
  REML = TRUE
)

testthat::test_that("mixed get model", {
  testthat::expect_equal(rmod0@theta,rmod1@theta,tolerance = 0.001)
  testthat::expect_equal(rmod1@theta,mod$model@theta,tolerance = 0.001)
})


mod<-gamlj::gamljMixed(
  formula =y ~ 1 + cond+( 1|subj ),
  data = subjects_by_stimuli,
  contrasts = c(cond="deviation")
)

res<-mod$main$fixed$asDF[2,3]

testthat::test_that("contrast option works", {
  testthat::expect_equal(res,.484954,tolerance = 0.001)
})




data("hsbdemo")
mod0<-stats::glm(schtyp ~ write + honors + honors:write,data=hsbdemo,family = binomial())
preds0<-predict(mod0,type = "response")

mod1<-gamlj::gamljGzlm(
  formula = schtyp ~ write + honors + honors:write,
  data = hsbdemo,
  showParamsCI = TRUE,
  modelSelection = "logistic")

preds<-predict(mod1)
dd<-gamlj_data(mod1)

testthat::test_that("gzlm predict ", {
  testthat::expect_equal(round(mean(preds),2),round(mean(preds0),2))
  testthat::expect_equal(round(mean(dd$write),2),0)
  
})

rmod<-gamlj_model(mod1)


testthat::test_that("glm get model ", {
  testthat::expect_equal(sigma(rmod),sigma(mod0),tolerance = 0.01)
  testthat::expect_equal(sigma(mod0),sigma(mod1$model),tolerance = 0.01)
})



se<-gamlj_simpleEffects(mod1,variable="write",moderator="honors")
res<-se$Anova$asDF
testthat::test_that("simple effects ", {
  testthat::expect_equal(round(res[2,2],2),6.64)
  testthat::expect_equal(round(res[2,3],2),1)
})


mod<-gamlj::gamljGzlm(
  formula = schtyp ~ write + honors + honors:write,
  data = hsbdemo,
  showParamsCI = TRUE,
  modelSelection = "logistic",
  scaling = c(write="standardized"))


res<-mod$main$fixed$asDF[2,3]
testthat::test_that("test scaling works ", {
  testthat::expect_equal(res,-0.214873,tol=.001)
})

