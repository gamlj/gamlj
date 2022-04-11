library(gamlj)
data("qsport")

obj<-gamlj::gamljGLM(
  formula = performance ~ hours,
  data = qsport)

