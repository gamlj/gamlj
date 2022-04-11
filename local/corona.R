data<-read.csv("../gamlj_docsource/data/corona/2019_nCoV_data_rec.csv")

names(data)
str(data)
data$Day<-data$Day-mean(data$Day)
gamlj::gamljGlmMixed(
  formula = Confirmed ~ 1 + Day+I(Day^2)+( 1 + Day+I(Day^2) | Country ),
  data = data,
  showParamsCI = TRUE,
  showExpbCI = FALSE,
  plotHAxis = Day,
  plotRaw = TRUE,
  modelSelection = "custom",
  custom_family = "Gamma",
  custom_link = "log",
  cimethod = "wald")

library(lme4)
form<-Confirmed ~ 1 + Day+I(Day^2)+( 1 + Day | Country )
mod<-glmer(form,data=data,family = Gamma("log"),glmerControl(optCtrl = list(maxfun=5000)))
summary(mod)
