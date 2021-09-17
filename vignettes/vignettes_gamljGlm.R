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

## -----------------------------------------------------------------------------
data<-read.csv("https://raw.githubusercontent.com/mcfanda/gamlj_docs/master/data/exercise.csv")
summary(data[,c("xage","zexer","yendu")])


## ----warning=FALSE, message=FALSE---------------------------------------------
mod1<-gamljGlm(formula = yendu~xage+zexer, data=data)
mod1

## -----------------------------------------------------------------------------
anofaDF<-mod1$main$anova$asDF
anofaDF

## -----------------------------------------------------------------------------
mod2<-gamljGlm(formula = yendu~xage+zexer, 
              data=data,
              effectSize = c("beta", "eta","partEta","omega","epsilon"))
mod2$main$anova

## -----------------------------------------------------------------------------
mod2_2<-update(mod1,effectSize = c("beta", "eta","partEta","omega","epsilon"))
mod2_2$main$anova

## ----warning=FALSE, message=FALSE---------------------------------------------
mod3<-gamljGlm(formula = yendu~xage*zexer, data=data)
mod3

## ----warning=FALSE, message=FALSE---------------------------------------------
#mod4<-gamljGlm(formula = yendu~xage*zexer,
#               data=data,
#               simpleVariable = "xage",
#               simpleModerator = "zexer")
### equivalent results :
gamlj_simpleEffects(mod3,variable = "xage",moderator = "zexer")

## ----warning=FALSE, message=FALSE---------------------------------------------
gamlj_simpleEffects(mod3,variable = "xage",moderator = "zexer",simpleScale="percent")

## ----warning=FALSE, message=FALSE---------------------------------------------
gamlj_simpleEffects(mod3,variable = "xage",moderator = "zexer",
                    simpleScale="percent", 
                    simpleScaleLabels="values_labels")

## ----warning=FALSE, message=FALSE---------------------------------------------
mod4<-gamljGlm(formula = yendu~xage*zexer, data=data, 
               plotHAxis = "xage",plotSepLines = "zexer",plotError = "ci")
plot(mod4)

## ----warning=FALSE, message=FALSE---------------------------------------------
plot(mod4,plotRaw=T)

## ----warning=FALSE, message=FALSE---------------------------------------------
myplot<-plot(mod4)
myplot+ggplot2::theme_grey()


