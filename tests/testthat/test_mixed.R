testthat::context("mixed")
tol<-0.001
data("subjects_by_stimuli")

subjects_by_stimuli$subj<-factor(subjects_by_stimuli$subj)
subjects_by_stimuli$stimulus<-factor(subjects_by_stimuli$stimulus)
subjects_by_stimuli$cond<-factor(subjects_by_stimuli$cond)
levels(subjects_by_stimuli$cond)<-c("A","B")

formula<-y~1+cond+(1|subj)+(1|stimulus)

model<-GAMLj3::gamlj_mixed(
  formula =y ~ 1 + cond+( 1|subj ),
  data = subjects_by_stimuli,
  plot_x="cond",
  norm_plot= T, resid_plot = T, cluster_boxplot = T, cluster_respred = T, rand_hist = T
)


infotable<-model$info$asDF

testthat::test_that("info is ok", {
  testthat::expect_equal(as.numeric(infotable$value[7]),3000)
  testthat::expect_equal(infotable$info[8],"Converged")
})

ftable<-model$main$anova$asDF

testthat::test_that("f-table is ok", {
  testthat::expect_equal(ftable[1,4],2949)
})

ptable<-model$main$coefficients$asDF

testthat::test_that("p-table is ok", {
  testthat::expect_equal(ptable[1,4],0.307,tol)
  testthat::expect_equal(ptable[2,5],0.703,tol)
  testthat::expect_equal(as.character(ptable[2,2]),"B - A")
})

rtable<-model$main$random$asDF

testthat::test_that("p-table is ok", {
  testthat::expect_equal(rtable[1,3],4.49,tol)
  testthat::expect_equal(as.character(rtable[1,"groups"]),"subj")
})

testthat::test_that("a mainplot is produced", {
  testthat::expect_true(ggplot2::is.ggplot(plot(model)))
})


testthat::test_that("a  residplot is produced", {
  testthat::expect_true(ggplot2::is.ggplot(model$assumptions$residPlot$plot$fun()))
})


testthat::test_that("a  normplot is produced", {
  testthat::expect_true(ggplot2::is.ggplot(model$assumptions$normPlot$plot$fun()))
})


testthat::test_that("a  resid boxplot is produced", {
  testthat::expect_true(ggplot2::is.ggplot(model$assumptions$clusterBoxplot[[1]]$plot$fun()))
})

testthat::test_that("a randhist is produced", {
  testthat::expect_true(ggplot2::is.ggplot(model$assumptions$randHist[[1]]$plot$fun()))
  testthat::expect_true(ggplot2::is.ggplot(model$assumptions$clusterResPred[[1]]$plot$fun()))
  
})



model<-GAMLj3::gamlj_mixed(
  dep=y,
  factors = "cond",
  model_terms = "cond",
  cluster = "subj",
  re = list(list(c("Intercept","subj"))),
  data = subjects_by_stimuli
)


ftable<-model$main$anova$asDF

testthat::test_that("list interface is ok", {
  testthat::expect_equal(ftable[1,4],2949)
})


testthat::test_that("uncorrelated error", {
  testthat::expect_error(  
    model<-GAMLj3::gamlj_mixed(
      dep=y,
      factors = "cond",
      modelTerms = "cond",
      cluster = list("subj","stimulus"),
      randomTerms = list(list(list("Intercept","subj")),list(list("cond","subj"),list("Intercept","stimulus"))),
      data = data
    )
    )
})


model1<-GAMLj3::gamlj_mixed(
  dep=y,
  factors = "cond",
  model_terms = "cond",
  cluster = "subj",
  re = list(list(c("Intercept","subj")),list(c("cond","subj"))),
  data = subjects_by_stimuli
)

model2<-GAMLj3::gamlj_mixed(
   formula = y ~ 1 + cond+( 1 | subj )+( 0+cond | subj ),
   data = subjects_by_stimuli
 )

testthat::test_that("uncorrelated works", {
  testthat::expect_equal(as.character(model1$info$asDF[2,2]),as.character(model2$info$asDF[2,2]))
})

formula<-y~1+cond+(1+cond|subj)+(1|stimulus)


model<-GAMLj3::gamlj_mixed(
  formula =formula,
  data = subjects_by_stimuli, 
  plot_x = cond,
  plot_re = T,
  re_lrt=T  
)

testthat::test_that("ranova works", {
                    testthat::expect_equal(model$main$ranova$asDF[2,2],6)
                    testthat::expect_equal(model$main$ranova$asDF[2,2],6)
}
)

testthat::test_that("mixed plot works", {
                    testthat::expect_true(ggplot2::is.ggplot(model$mainPlots[[1]]$plot$fun()))
                    testthat::expect_true(ggplot2::is.ggplot(plot(model)))
}
)


adddata<-subjects_by_stimuli
adddata$x<-rnorm(length(adddata$nrow))
adddata$subj<-factor(adddata$subj)
adddata$stimulus<-factor(adddata$stimulus)

formula<-y~1+cond+x+(1+cond|subj)+(1|stimulus)
adddata$cond<-factor(adddata$cond)
model<-GAMLj3::gamlj_mixed(
  formula =formula,
  data = adddata, 
  covs_scale = c(x="standardized")
  
)

testthat::test_that("standardizing with more clusters", {
                    testthat::expect_equal(as.character(model$main$coefficients$asDF$source[3]),"x")
                    testthat::expect_equal(model$main$coefficients$asDF$se[2],.3224,tol)
}
)

model<-GAMLj3::gamlj_mixed(
  formula =formula,
  data = adddata, 
  covs_scale = c("x"="clusterbasedstandardized")
)

testthat::test_that("standardizing with more clusters",{
                    testthat::expect_equal(as.character(model$main$coefficients$asDF$source[3]),"x")
                    testthat::expect_equal(model$main$coefficients$asDF$estimate[1],19.60,tolerance = tol)
}
                    
)


data("beers_bars")
data<-beers_bars

model<-GAMLj3::gamlj_mixed(
  formula = smile ~ 1 + beer + I(beer^2)+( 1 + beer + I(beer^2) | bar ),
  data = data)
###### this has changed with lme4 1.1 
testthat::test_that("some poly", {
  testthat::expect_lt(model$main$anova$asDF[2,2],0.43)
  testthat::expect_gt(model$main$anova$asDF[2,2],0.31)
})


model<-GAMLj3::gamlj_mixed(
  formula = smile ~ 1 + beer +( 1 + beer  | bar ),
  data = data,
  covs_scale = list(list(
    var="beer",
    type="standardized")))

testthat::test_that("standardizing", {
  testthat::expect_equal(model$main$coefficients$asDF$estimate[2],.8506,tolerance = .002)
})

model<-GAMLj3::gamlj_mixed(
  formula = smile ~ 1 + beer +( 1 + beer  | bar ),
  data = data,
  covs_scale  = list("beer"="clusterbasedstandardized")
  )

testthat::test_that("cluster-based-standardizing", {
  testthat::expect_equal(model$main$coefficients$asDF$estimate[2],.6111,tolerance = tol)
})

model<-GAMLj3::gamlj_mixed(
  formula = smile ~ 1 + beer +( 1 + beer  | bar ),
  data = data,
  covs_scale = list("beer"="clusterbasedcentered")
  )


testthat::test_that("cluster-based-centering", {
  testthat::expect_equal(model$main$coefficients$asDF$estimate[2],.607,tol)
})


model<-GAMLj3::gamlj_mixed(
   formula =smile ~ 1 +(1|bar),
   data = data
 )
testthat::test_that("intercept only works",
          testthat::expect_equal(round(model$main$random$asDF[1,4],digits = 2),1.74)
)

data("subjects_by_stimuli")
subjects_by_stimuli$cond<-factor(subjects_by_stimuli$cond)
subjects_by_stimuli$subj<-factor(subjects_by_stimuli$subj)
subjects_by_stimuli$stimulus<-factor(subjects_by_stimuli$stimulus)

formula<-y~1+cond+(1+cond|subj)+(1|stimulus)
model<-GAMLj3::gamlj_mixed(
  formula =formula,
  data = subjects_by_stimuli, 
  re_lrt=T , 
  plot_x=cond,
  plot_re=T
)

testthat::test_that("ranova works",
                    testthat::expect_equal(model$main$ranova$asDF$AIC[1],15722.49,tol)
)

testthat::test_that("plot works",{
                    testthat::expect_true(ggplot2::is.ggplot(plot(model)))
}
)


## simple effects and polynomial
data("wicksell")
data<-wicksell
data$subj<-factor(data$subj)
data$time<-factor(data$time)
data$group<-factor(data$group)

gobj<-GAMLj3::gamlj_mixed(
  formula = dv ~ 1 + group + time + group:time+( 1 | subj ),
  data = data,
  contrasts = c("group"="simple","time"="polynomial"),
   simple_x = time,
   simple_mods = group)

testthat::test_that("simple effects", {
  testthat::expect_equal(as.character(gobj$simpleEffects$coefficients$asDF$contrast[1]),"linear")
  testthat::expect_equal(as.character(gobj$simpleEffects$coefficients$asDF$contrast[2]),"quadratic")
  testthat::expect_equal(gobj$simpleEffects$coefficients$asDF$estimate[3],-7.3231,tol)
 })


gobj2<-GAMLj3::gamlj_mixed(
  formula = dv ~ 1 +group+ time:group+ time+( 1 | subj ),
  data = data, 
  posthoc = list(c("time","group")),
  emmeans = ~time:group)

mm<-gobj2$emmeans[[1]]$asDF
mm32<-mm$estimate[mm$time==3 & mm$group==2]
mm61<-mm$estimate[mm$time==6 & mm$group==1]
po<-gobj2$posthoc[[1]]$asDF
d1<-mm32-mm61
d2<-po$estimate[po$time_lev1==3 & po$group_lev1==2 & po$time_lev2==6 & po$group_lev2==1]

testthat::test_that("posthoc in mixed", {
  testthat::expect_equal(gobj2$posthoc[[1]]$asDF$group_lev1[8],"2")
  testthat::expect_equal(d1,d2)
})


testthat::test_that("order does not count", {
  testthat::expect_equal(gobj$main$anova$asDF[1,2],gobj2$main$anova$asDF[1,2])

})



