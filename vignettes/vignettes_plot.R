## ---- include = FALSE---------------------------------------------------------
knitr::opts_chunk$set(
  collapse = FALSE,
  comment = "",
  fig.align = "center",
  fig.height = 4,
  fig.width = 6
)
options(digits=3)


## ----setup--------------------------------------------------------------------
library(gamlj)

## ----warning=FALSE, message=FALSE---------------------------------------------
mod<-gamljGlm(formula=Sepal.Length~Sepal.Width*Petal.Length*Petal.Width, 
              data=iris,
              plotHAxis = "Sepal.Width"
              )

plot(mod)

## ----warning=FALSE, message=FALSE---------------------------------------------
plot(mod,formula = ~Petal.Length)


## ----warning=FALSE, message=FALSE---------------------------------------------
plot(mod,formula = ~Sepal.Width:Petal.Length)

## ----warning=FALSE, message=FALSE---------------------------------------------
plot(mod,formula = ~Sepal.Width:Petal.Length, simpleScale="percent")

## ----warning=FALSE, message=FALSE---------------------------------------------
plots<-plot(mod,formula = ~Sepal.Width:Petal.Length:Petal.Width)
print<-lapply(plots,print)

## ----warning=FALSE, message=FALSE---------------------------------------------
mod2<-gamljGlm(formula=Sepal.Length~Species,  data=iris)
plot(mod2,formula=~Species)

## ----warning=FALSE, message=FALSE---------------------------------------------
plot(mod,plotHAxis="Sepal.Width", plotSepLines="Petal.Length")

## ----warning=FALSE, message=FALSE---------------------------------------------
plot(mod,plotHAxis="Sepal.Width", plotSepLines="Petal.Length", plotRaw=T,plotError="ci")

plot(mod2,formula=~Species,plotRaw=T,plotError="ci")


## ----warning=FALSE, message=FALSE---------------------------------------------
p<-plot(mod,plotHAxis="Sepal.Width", plotSepLines="Petal.Length", plotRaw=T,plotError="ci")
p+ggplot2::ggtitle("A nice plot")+ggplot2::theme_minimal()

## ----warning=FALSE, message=FALSE, results='hide'-----------------------------
data("schoolexam")
mod3<-gamljGlmMixed(formula=pass~math+(1|school),data=schoolexam)

p<-plot(mod3, plotHAxis = "math",plotRandomEffects=T)

## ----warning=FALSE, message=FALSE, echo=FALSE---------------------------------
p


