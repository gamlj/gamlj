## ----message=FALSE, warning=FALSE, include=FALSE------------------------------
if (!requireNamespace("see", quietly = TRUE) ||
      !requireNamespace("dplyr", quietly = TRUE) ||
      !requireNamespace("ggplot2", quietly = TRUE) ||
      !requireNamespace("performance", quietly = TRUE) ||
      !requireNamespace("BayesFactor", quietly = TRUE) ||
      !requireNamespace("rstanarm", quietly = TRUE)) {
  knitr::opts_chunk$set(eval = FALSE)
}

data(iris)
library(knitr)
library(bayestestR)
options(knitr.kable.NA = '')
knitr::opts_chunk$set(comment=">")
knitr::opts_chunk$set(dpi=150)
options(digits=2)

set.seed(333)

## ----message=FALSE, warning=FALSE---------------------------------------------
result <- cor.test(iris$Sepal.Width, iris$Sepal.Length)
result

## ----message=FALSE, warning=FALSE, results='hide'-----------------------------
library(BayesFactor)
result <- correlationBF(iris$Sepal.Width, iris$Sepal.Length)

## ----message=FALSE, warning=FALSE, eval=FALSE---------------------------------
#  describe_posterior(result)

## ----echo=FALSE---------------------------------------------------------------
structure(list(Parameter = "rho", Median = -0.114149129692488, 
    CI = 89, CI_low = -0.240766308855643, CI_high = 0.00794997655649642, 
    pd = 91.6, ROPE_CI = 89, ROPE_low = -0.1, ROPE_high = 0.1, 
    ROPE_Percentage = 42.0949171581017, BF = 0.509017511647702, 
    Prior_Distribution = "cauchy", Prior_Location = 0, Prior_Scale = 0.333333333333333), row.names = 1L, class = "data.frame")

## ----message=FALSE, warning=FALSE---------------------------------------------
bayesfactor(result)

## ----echo=FALSE, fig.cap="Wagenmakers' pizza poking analogy. From the great 'www.bayesianspectacles.org' blog.", fig.align='center', out.width="80%"----
knitr::include_graphics("https://github.com/easystats/easystats/raw/master/man/figures/bayestestR/LetsPokeAPizza.jpg")

## ----message=FALSE, warning=FALSE---------------------------------------------
library(see)

plot(bayesfactor(result)) +
  scale_fill_pizza()

## ----message=FALSE, warning=FALSE---------------------------------------------
library(dplyr)
library(ggplot2)

# Select only two relevant species
data <- iris %>% 
  filter(Species != "setosa") %>% 
  droplevels()

# Visualise distributions and observations
data %>% 
  ggplot(aes(x = Species, y = Sepal.Width, fill = Species)) +
  geom_violindot(fill_dots = "black", size_dots = 1) +
  scale_fill_material() +
  theme_modern()

## ----message=FALSE, warning=FALSE---------------------------------------------
result <- BayesFactor::ttestBF(formula = Sepal.Width ~ Species, data = data)
describe_posterior(result)

## ----message=FALSE, warning=FALSE, eval=FALSE---------------------------------
#  library(rstanarm)
#  
#  model <- stan_glm(Species ~ Sepal.Width, data = data, family = "binomial")

## ----message=FALSE, warning=FALSE, echo=FALSE---------------------------------
library(rstanarm)

model <- stan_glm(Species ~ Sepal.Width, data = data, family = "binomial", refresh = 0)

## ----message=FALSE, warning=FALSE---------------------------------------------
library(performance)

model_performance(model)

## ----message=FALSE, warning=FALSE---------------------------------------------
describe_posterior(model, test = c("pd", "ROPE", "BF"))

## ----message=FALSE, warning=FALSE---------------------------------------------
# plot(rope(result))

