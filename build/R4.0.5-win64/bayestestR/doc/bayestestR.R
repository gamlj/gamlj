## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
if (!requireNamespace("rstanarm", quietly = TRUE)) {
  knitr::opts_chunk$set(eval = FALSE)
}

library(knitr)
options(knitr.kable.NA = '')
knitr::opts_chunk$set(comment=">")
options(digits=2)

## ----echo=FALSE, fig.cap="Accurate depiction of a regular Bayesian user estimating a credible interval.", fig.align='center', out.width="50%"----
knitr::include_graphics("https://github.com/easystats/easystats/raw/master/man/figures/bayestestR/bayesianMaster.jpg")

## ----eval=FALSE, message=FALSE, warning=FALSE---------------------------------
#  install.packages("devtools")
#  devtools::install_github("easystats/easystats")

## ----message=FALSE, warning=FALSE, eval=FALSE---------------------------------
#  install.packages("rstanarm")
#  library(rstanarm)

## ----message=FALSE, warning=FALSE, eval=FALSE---------------------------------
#  model <- lm(Sepal.Length ~ Petal.Length, data=iris)
#  summary(model)

## ----echo=FALSE, message=FALSE, warning=FALSE, comment=NA---------------------
library(dplyr)

lm(Sepal.Length ~ Petal.Length, data=iris) %>% 
  summary()

## ----message=FALSE, warning=FALSE, eval=FALSE---------------------------------
#  model <- stan_glm(Sepal.Length ~ Petal.Length, data=iris)
#  describe_posterior(model)

## ----echo=FALSE, message=FALSE, warning=FALSE, comment=NA---------------------
library(rstanarm)
library(bayestestR)
set.seed(333)

model <- stan_glm(Sepal.Length ~ Petal.Length, data=iris, refresh = 0)
knitr::kable(describe_posterior(model), digits=2)

