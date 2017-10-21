# GAMLj Suite for Linear Models in Jamovi

General, Mixed and Generalized Models module for Jamovi

<img src="docs/i1.png" class="img-responsive" alt="">


Estimates a General Linear Model, Mixed Linear Models and Generalized Linear Model (the latter yet to be implemented) with categorial and/or continuous variables, with options to facilitate estimation of interactions, simple slopes, simple effects, post-hoc etc.


* Continuous and categorical independent variables
* F-test and parameter estimates
* Moderation made easy
* Simple slopes analysis
* Simple effects
* post-hocs analysis
* Plots for up two three-way interactions
* Automatic selection of best estimation methods and degrees of freedom selection
* Type III estimation

As regards the GLM module, more informations can be found at [GAMLj_GLM page](https://mcfanda.github.io/gamlj_glm/)
As regards the Mixed Models module, more informations can be found at [GAMLj_Mixed page](https://mcfanda.github.io/gamlj_mixed/)

# Installation

You will first need to download [Jamovi](https://www.jamovi.org/download.html). 


You can clone this repository and compile the module within R with 

``` jmvtools::install() ```



You can clone this repository and compile the module within R with 

```
library(jmvtools)

jmvtools::install()

```

# Troubleshooting

The modules installs dependencies automatically, but in some R installation the installation process seems to fail when it comes to the dependencies. The solution is to install in the module build folder the required package that fails before installing the module. For example, if "pbkrtest" fails and you cloned the source in YOURCLONEFOLDER folder, run this before installing the module.


```
package<-"pbkrtest"
dirroot<-YOURCLONEFOLDER
instdir<-paste0(dirroot,"/build/R")
install.packages(package,lib = instdir)


```


