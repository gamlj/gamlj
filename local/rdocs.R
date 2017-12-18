library(gamlj)
exercise<-read.csv("extdata/exercise.csv")

names(exercise)

gamljGLM(data=dat,model=yendu~xage,dep="yendu")

gamljGLM()

library(roxygen2)
roxygenize(".")
library(devtools)
document()
data("exercise")
