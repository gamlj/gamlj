# GAMLj Mixed Model for Jamovi

Mixed Models module of the GAMLj suite for Jamovi

<img src="docs/i1.png" class="img-responsive" alt="">


Estimates a Mixed Linear Model with categorial and/or continuous variables, with options to facilitate estimation of interactions, simple slopes, simple effects, etc.

* Continuous and categorical independent variables
* F-test and parameter estimates
* Moderation made easy
* Simple slopes analysis
* Simple effects
* post-hocs analysis
* Plots for up two three-way interactions
* Automatic selection of best estimation methods and degrees of freedom selection
* Type III estimation

More informations can be found at [GAMLj_Mixed page](https://mcfanda.github.io/gamlj_mixed/)

# Installation

You will first need to download [Jamovi](https://www.jamovi.org/download.html). 


You can clone this repository and compile the module within R with 

``` jmvtools::install() ```



## Model
The module can estimates REML and ML linear mixed models for any combination of categorical and continuous variables, thus providing an easy way of obtaining multilevel or hierarchical linear models for any combination of independent variables types.

## Estimates
The modules provides  fixed effects parameter estimates and random coefficients variances and correlations for any estimated model. 

## Variable Scaling
Within the module, by simply point-and-click, categorial variable can be coded in many different ways, as dummies, centered, difference, helmeret, etc.
Continuous variables can be centered, standardized. 


## Plots
The "plots" menu allows for plotting  main effects and interactions for any combination of types of variables, 
making it easy to plot interaction means plots, simple slopes, and combinations of them. The best plot is chosen automatically.
Plots can plot up to a three-way interaction.
## Simple effects
Simple effects can be computed for any combination of types of variables, 
making it easy to proble interaction, simple slopes, and combinations of them. 
Simple effects can estimated  up to a three-way interaction.

## Post-hocs
Major post-hoc tests can be accomplished for the categorical variables groups estimated means.


# Installation


You can clone this repository and compile the module within R with 

```
library(jmvtools)

jmvtools::install()

```

or, without cloning anything, just run this in R


```
library("devtools")
install_github("mcfanda/gamlj_mixed")

````


