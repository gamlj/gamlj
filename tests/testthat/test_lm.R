testthat::context("lm")
tol<-.001

mod0<-GAMLj3::gamlj_lm(
  data = ToothGrowth,
  formula=len~supp+dose)

mod1<-GAMLj3::gamlj_lm(
  data = ToothGrowth,
  dep = "len",
  factors = "supp",
  covs="dose")


testthat::test_that("equivalent model input (1)", {
  testthat::expect_equal(mod0$info$asDF$specs[2],mod1$info$asDF$specs[2])
  testthat::expect_equal(mod0$main$anova$asDF$f[3],mod1$main$anova$asDF$f[3])
  testthat::expect_equal(mod0$main$coefficients$asDF$label[2],mod0$main$coefficients$asDF$label[2])
  testthat::expect_equal(mod0$main$coefficients$asDF$est.ci.lower[1],mod0$main$coefficients$asDF$est.ci.lower[1])
  })



mod0<-GAMLj3::gamlj_lm(
  data = ToothGrowth,
  dep = "len",
  factors = "supp",
  covs="dose",
  model_terms = list("dose","supp",c("dose","supp")))

mod1<-GAMLj3::gamlj_lm(
  data = ToothGrowth,
  dep = "len",
  factors = "supp",
  covs="dose",
  model_terms = ~dose+supp+dose:supp)

testthat::test_that("equivalent model input (2)", {
  testthat::expect_equal(mod0$info$asDF$specs[2],mod1$info$asDF$specs[2])
  testthat::expect_equal(mod0$main$anova$asDF$f[3],mod1$main$anova$asDF$f[3])
  testthat::expect_equal(mod0$main$coefficients$asDF$label[2],mod0$main$coefficients$asDF$label[2])
  testthat::expect_equal(mod0$main$coefficients$asDF$est.ci.lower[1],mod0$main$coefficients$asDF$est.ci.lower[1])
})


testthat::test_that("glm estimates are correct", {
  testthat::expect_equal(mod0$main$coefficients$asDF$estimate[3], -3.70,tolerance = tol)
  testthat::expect_equal(mod0$main$coefficients$asDF$est.ci.upper[2], 11.45,tolerance = tol)
  testthat::expect_equal(mod0$main$coefficients$asDF$est.ci.lower[3],-5.811,tol)
  testthat::expect_equal(mod0$main$coefficients$asDF$p[3],0.00089,tol)
})

a<-mod0$main$anova$asDF
resid<-a$ss[a$source=="Residuals"]
eff<-a$ss[a$source=="supp"]
peta<-eff/(eff+resid)

testthat::test_that("glm p eta2 are correct", {
  testthat::expect_equal(a$etaSqP[a$source=="supp"],peta)
})



data("hsbdemo")

mod<-GAMLj3::gamlj_lm(
  data = hsbdemo,
  formula=science~1
)

testthat::test_that("intercept only works",
                    testthat::expect_equal(round(mod$main$coefficients$asDF[1,3],digits=2),51.85)
          )

mod<-GAMLj3::gamlj_lm(
  data = hsbdemo,
  formula=science~math+schtyp+math:schtyp,
  ci_width = 90,
  simple_x  = math,
  simple_mods  = schtyp,
  es = c("eta","etap","omega","omegap")
)


mod$simpleEffects$anova$asDF
r.anova<-mod$main$anova$asDF

testthat::test_that("glm anova is correct", {
  testthat::expect_equal(as.character(r.anova[3,1]),"schtyp")
  testthat::expect_equal(round(r.anova[4,4],3),0.276)
  testthat::expect_equal(round(r.anova[3,6],5),7e-05)
  testthat::expect_equal(round(r.anova[3,8],5),-0.000)
  testthat::expect_equal(round(r.anova[1,9],5),.38828)
  
})

se.params<-mod$simpleEffects$coefficients$asDF

testthat::test_that("glm anova simple effects", {
  testthat::expect_equal(as.character(se.params[1,1]),"private")
  testthat::expect_equal(round(se.params[2,5],3),0.574)
})

testthat::test_that("glm CI width", {
  testthat::expect_equal(round(mod$main$coefficients$asDF[2,5],3),0.495)
})




mod<-GAMLj3::gamlj_lm(
  data = hsbdemo,
  formula=science~math*schtyp*write,
  ci_width=90,
  simple_x = "math",
  simple_mods = list("schtyp","write"),
)

testthat::test_that("SE names are fine",{
         testthat::expect_equal(mod$simpleEffects$coefficients$asDF$mod_write[3],"Mean")
         testthat::expect_equal(mod$simpleEffects$anova$asDF$mod_write[3],"Mean")
}
)

mod<-GAMLj3::gamlj_lm(
  data = hsbdemo,
  formula=science~math*schtyp*write,
  ci_width=90,
  simple_x  = "schtyp",
  simple_mods  = list("math","write"),
  simple_interactions  = T
)
mod
testthat::test_that("SE multiple moderators iv=categorical",{
  testthat::expect_equal(mod$simpleEffects$coefficients$asDF$contrast[1],"public - private")
  testthat::expect_equal(mod$simpleEffects$anova$asDF$test[1],1.469,tol)
}
)

testthat::test_that("simple interaction",{
  testthat::expect_equal(mod$simpleInteractions[[1]]$anova$asDF$effect[1],"schtyp:math")
  testthat::expect_equal(mod$simpleInteractions[[1]]$anova$asDF$test[3],.911,tol)
  testthat::expect_equal(mod$simpleInteractions[[1]]$coefficients$asDF$effect[1],"(public-private):math")
  testthat::expect_equal(mod$simpleInteractions[[1]]$coefficients$asDF$estimate[2],-.0543,tol)
  
}
)

hsbdemo$c1<-factor(rep(c(1,0),length(hsbdemo$id)/2))
hsbdemo$c2<-factor(rep(c(1,0),each=length(hsbdemo$id)/2))

mod<-GAMLj3::gamlj_lm(
  data = hsbdemo,
  formula=science~c1*c2,
  posthoc =  list("c1",c("c1","c2")),
  adjust = "bonf"
)

testthat::test_that("glm labels do not square", {
  testthat::expect_equal(as.character(mod$main$coefficients$asDF[4,1]),"c11:c21")
})

ph<-mod$posthoc
ph1<-ph[[1]]$asDF
ph2<-ph[[2]]$asDF

testthat::test_that("postoh in glm", {
  testthat::expect_equal(as.character(ph1[1,1]),"0")
  testthat::expect_equal(ph1[1,4],0.52)
  testthat::expect_equal(as.character(ph2[4,1]),"0")
  testthat::expect_equal(ph2[6,6],13.46)
  
})


mod<-GAMLj3::gamlj_lm(
  data = hsbdemo,
  formula=science~math+schtyp+math:schtyp,
  posthoc  = list("schtyp")
)

testthat::test_that("glm posthoc", {
  testthat::expect_equal(round(mod$posthoc[[1]]$asDF[[5]],3),1.528)
  testthat::expect_equal(as.character(mod$posthoc[[1]]$asDF[[3]]),"public")
})


mod<-GAMLj3::gamlj_lm(
  data = hsbdemo,
  formula=science~math+schtyp+math:schtyp,
  es_info =  T
)
tab<-mod$main$effectsizes$asDF

testthat::test_that("glm effectsize", {
  testthat::expect_equal(tab[4,3],.21724,tol=.0001)
  testthat::expect_equal(tab[10,5],.0,tol=.00001)
})


data<-hsbdemo

names(data)[3]<-c("Gender (test ?)")
mod<-GAMLj3::gamlj_lm(
  data = data,
  formula=science~math+`Gender (test ?)`+math:`Gender (test ?)`,
  simple_x = `Gender (test ?)`,
  simple_mods  = math,
  posthoc = "Gender (test ?)"
)


testthat::test_that("glm weird names", {
  testthat::expect_equal(as.character(mod$main$coefficients$asDF[3,1]),"Gender (test ?)1")
  testthat::expect_equal(as.character(mod$simpleEffects$coefficients$asDF[2,1]),"Mean")
  testthat::expect_equal(mod$simpleEffects$coefficients$asDF$estimate[3],2.4591,tol)
})

data$sex<-factor(data$`Gender (test ?)`,levels=c("female","male"))

mod2<-GAMLj3::gamlj_lm(
  data = data,
  formula=science~math+sex+math:sex,
  simple_x  = math,
  simple_mods  = sex
)

testthat::test_that("glm weird names", {
  testthat::expect_equal(mod2$simpleEffects$anova$asDF[1,1],"female")
  testthat::expect_true(
    all(mod$main$coefficients$asDF$estimate==mod2$main$coefficients$asDF$estimate)
  )
})

mod3<-GAMLj3::gamlj_lm(
  data = data,
  formula=science~math+math:`Gender (test ?)`+`Gender (test ?)`,
  simple_x  = `Gender (test ?)`,
  simple_mods  = math
)

res<-mod$main$anova$asDF
res3<-mod3$main$anova$asDF

testthat::test_that("glm order does not count", {
  testthat::expect_equal(res[2,4],res3[2,4])
  testthat::expect_equal(as.character(res[4,1]),as.character(res3[4,1]))
})


data("hsbdemo")
data<-hsbdemo
levels(data$ses)<-c("s1","s2","s3")
levels(data$female)<-c("f1","f2")

mod0<-GAMLj3::gamlj_lm(
  data = data,
  formula=science~math*ses*female,
  posthoc = list(c("ses","female")),
  posthoc_es  = c("dm")
)

mod1<-GAMLj3::gamlj_lm(
  data = data,
  formula=science~math*ses*female,
  posthoc = ~ses:female,
  posthoc_es  = c("dm")
)

testthat::test_that("glm posthoc interfaces", {
  testthat::expect_equal(mod0$posthoc[[1]]$asDF[4,5],mod0$posthoc[[1]]$asDF[4,5])
  testthat::expect_equal(mod1$posthoc[[1]]$asDF[7,14],mod0$posthoc[[1]]$asDF[7,14])
})


data$`weird ?`<-hsbdemo$ses
data$`weird !`<-hsbdemo$female
levels(data$`weird !`)<-c("a!","b!")
levels(data$`weird ?`)<-c("a?","b?","c?")

weird<-GAMLj3::gamlj_lm(
  data = data,
  formula=science~math*`weird !`*`weird ?`,
  simple_mods  = list("weird !","weird ?"),
  simple_x  = math,
  posthoc = list(c("weird !","weird ?"))
)

wres<-weird$posthoc[[1]]$asDF

testthat::test_that("weird posthoc", {
  testthat::expect_equal(wres[2,4],"c?")
  testthat::expect_equal(as.character(wres[10,1]),as.character(wres[15,4]))
})



mod<-GAMLj3::gamlj_lm(
  data = hsbdemo,
  formula=science~math+schtyp+math:schtyp, 
  emmeans = ~schtyp,
  homo_test  = T,
  norm_test  = T
)
res<-mod$emmeans[[1]]$asDF
testthat::test_that("glm EMM", {
  testthat::expect_equal(round(res[1,2],2),52.07)
})

res1<-mod$assumptions$homotest$asDF
res2<-mod$assumptions$normtest$asDF

testthat::test_that("glm assumptions", {
  testthat::expect_equal(round(res1[1,5],2),0.06)
  testthat::expect_equal(round(res2[1,3],2),0.86)
})


mod<-GAMLj3::gamlj_lm(
  formula = science ~ math + schtyp + schtyp:math,
  data = hsbdemo,
  contrasts = list(list(
      var="schtyp",
      type="deviation")),
  qq_plot  = T
)
res<-mod$main$coefficients$asDF
testthat::test_that("glm contrasts", {
  testthat::expect_equal(round(res[3,3],2),-0.11)
  testthat::expect_equal(round(res[1,3],2),51.96)
})

#plot<-mod$assumptions$qqplot$plot$fun()
#testthat::test_that("glm assumptions plot", {
#  testthat::expect_true(ggplot2::is.ggplot(plot))
#})



mod<-GAMLj3::gamlj_lm(
  formula = science ~ math + I(math^2),
  data = data
)

res<-mod$main$anova$asDF

testthat::test_that("glm contrasts", {
  testthat::expect_equal(as.character(res$source[3]),"math²")
  testthat::expect_equal(round(res[1,4],2),65.07)
})


mod<-GAMLj3::gamlj_lm(
  formula = read ~ 1,  data = data
)
res<-mod$main$anova$asDF
testthat::test_that("glm intercept only model", {
  testthat::expect_equal(as.character(res$source[1]),"Residuals")
  testthat::expect_equal(round(res[2,2],2),20919.42)
})



mod<-GAMLj3::gamlj_lm(
  formula = read ~ 0,  data = data,
  fixed_intercept =FALSE
)
res<-mod$main$anova$asDF
testthat::test_that("glm zero-intercept model", {
  testthat::expect_equal(as.character(res$source[1]),"Residuals")
  testthat::expect_equal(round(res[2,2],2),566514)
})


### bootstrap
mod<-GAMLj3::gamlj_lm(
  data = hsbdemo,
  formula=science~math+prog+math:prog, 
  emmeans = ~prog,
  betas_ci=T,
  ci_method = "bcai"
)


mod$main$coefficients$asDF

testthat::test_that("bootstrap ci make sense", {
  testthat::expect_true(mod$main$coefficients$asDF[["est.ci.lower"]][3]<mod$main$coefficients$asDF[["estimate"]][3])
  testthat::expect_true(mod$main$coefficients$asDF[["est.ci.upper"]][1]>mod$main$coefficients$asDF[["estimate"]][1])
})


mod<-GAMLj3::gamlj_lm(
  data = hsbdemo,
  formula=science~0+math+prog+math:prog, 
  emmeans = ~prog
)
testthat::test_that("glm zero-intercept model", {
  testthat::expect_equal(abs(mod$main$coefficients$asDF$estimate[1]),.72,tol)
  testthat::expect_equal(mod$main$coefficients$asDF$se[4],1.3814,tol)
})

mod<-GAMLj3::gamlj_lm(
  data = hsbdemo,
  formula=science~1+math+prog+math:prog, 
  omnibus = "LRT"
)

testthat::test_that("model comparison", {
  testthat::expect_equal(
    sum(mod$main$r2$asDF$r2),.41604
    ,tol)
})



mod<-GAMLj3::gamlj_lm(
  data = hsbdemo,
  formula=science~1+prog+math+math:prog, 
  nested_terms = ~math:prog
)
testthat::test_that("model comparison", {
  testthat::expect_equal(
    mod$main$r2$asDF$ar[1]-mod$main$r2$asDF$ar[2],mod$main$r2$asDF$ar[3]
    ,tol)
})


mod<-GAMLj3::gamlj_lm(
  data = hsbdemo,
  formula=science~1,
  nested_terms=~0
)

testthat::test_that("intercept model comparison", {
  testthat::expect_equal(
    mod$main$r2$asDF$ar[1]-mod$main$r2$asDF$ar[2],mod$main$r2$asDF$ar[3]
    ,tol)
})


