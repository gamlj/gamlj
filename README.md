# GAMLj Suite for Linear Models

General, Mixed, Generalized, and Generalized Mixed Models 

version 3.0.*

<center>
<img width="300" src="https://gamlj.github.io/commons/pics/ui.png" class="img-responsive" alt="">
</center>
<br>

Estimates a General Linear Model, Mixed Linear Models, Generalized Linear Models and Generalized Mixed Models with categorial and/or continuous variables, with options to facilitate estimation of interactions, simple slopes, simple effects, post-hoc etc.


* Continuous and categorical independent and dependent variables
* F-test and parameter estimates
* Confidence intervals
* Moderation made easy
* Simple slopes analysis
* Simple effects
* Polynomial effects
* post-hoc analysis
* Plots up to three-way interactions
* Automatic selection of best estimation methods and degrees of freedom selection
* Type III estimation

More informations can be found at [GAMLj page](https://gamlj.github.io/)

# Install in jamovi

Please install [jamovi](https://www.jamovi.org/download.html) and run it. Select the jamovi modules library and install GAMLj from there


<center>
<img width="600" src="https://gamlj.github.io/glm/install.png" class="img-responsive" alt="">
</center>

# Version 3

GAMLj has undergone a major upgrading with version 3.*. The upgrading made version 3 not compatible with previous versions of the module, meaning
that analyses that are done with previous versions cannot be opened with version 3. If you need to open and work with analyses done with GAMLj version < 3.0.0, you can install GAMLj legacy from the jamovi library. It will not interfere with GAMLj3.

## From source


You will first need to download [jamovi](https://www.jamovi.org/download.html). 

You can clone this repository and compile the module within R with 

```
library(jmvtools)

jmvtools::install()

```

# Install in R

```

devtools::install_github("gamlj/gamlj")

```


# Versions

Please check out the  [release notes](https://gamlj.github.io/release_notes.html). 


# Troubleshooting




## Installing from source

The module installs dependencies automatically, but in some R installations the process seems to fail when it comes to the dependencies. The solution is to install in the module build folder the required package that fails before installing the module. For example, if "pbkrtest" fails and you cloned the source in YOURCLONEFOLDER folder, run this before installing the module.



```
package<-"pbkrtest"
dirroot<-YOURCLONEFOLDER
instdir<-paste0(dirroot,"/build/R")
install.packages(package,lib = instdir)


```

# Programmatic name

```
paste(paste(LETTERS[c(7,1,13,12)],collapse =""),paste(letters[10]),sep="")

```
