# GAMLj Suite for Linear Models

General, Mixed, Generalized, and Generalized Mixed Models 

<em id="version">Version 3.3.2 </em>












































<center>
<img width="300" src="https://gamlj.github.io/commons/pics/ui.png" class="img-responsive" alt="">
</center>
<br>

Estimates a General Linear Model, Mixed Linear Models, Generalized Linear Models and Generalized Mixed Models with categorical and/or continuous variables, with options to facilitate estimation of interactions, simple slopes, simple effects, post-hoc etc.


* ANOVA and Regression approach
* Continuous and categorical independent variables
* F-test, LR tests, and parameter estimates
* Confidence intervals, standard, profile and bootstrap
* Moderation made easy
* Simple slopes analysis
* Simple effects analysis
* Simple interaction analysis
* Post-hoc analysis
* Contrasts analysis
* Plots for any order of interactions
* Automatic selection of best estimation methods and degrees of freedom selection
* Type III estimation
* A large set of effect size indices, depending on the model being estimated, such as $\eta^2$, $\omega^2$, $\epsilon^2$, and Cohen's $d$, both partial and not partial. 

Available models are:

* OLS Regression (GLM)
* OLS ANOVA (GLM)
* OLS ANCOVA (GLM)
* Random coefficients regression (Mixed)
* Random coefficients ANOVA-ANCOVA (Mixed)
* Logistic regression (GZLM)
* Logistic ANOVA-like model (GZLM)
* Probit regression (GZLM)
* Probit ANOVA-like model (GZLM)
* Multinomial regression (GZLM)
* Multinomial ANOVA-like model (GZLM)
* Poisson regression (GZLM)
* Poisson ANOVA-like model (GZLM)
* Overdispersed Poisson regression (GZLM)
* Overdispersed Poisson ANOVA-like model (GZLM)
* Negative binomial regression (GZLM)
* Negative binomial  ANOVA-like model (GZLM)
* Ordinal regression (GZLM)
* Ordinal ANOVA-like model (GZLM)
* Mixed Logistic regression (GMixed)
* Mixed Logistic ANOVA-like model (GMixed)
* Mixed Probit regression (GMixed)
* Mixed Probit ANOVA-like model (GMixed)
* Mixed Multinomial regression (GMixed)
* Mixed Multinomial ANOVA-like model (GMixed)
* Mixed Poisson regression (GMixed)
* Mixed Poisson ANOVA-like model (GMixed)
* Mixed Overdispersed Poisson regression (GMixed)
* Mixed Overdispersed Poisson ANOVA-like model (GMixed)
* Mixed Negative binomial regression (GMixed)
* Mixed Negative binomail  ANOVA-like model (GMixed)
* Mixed Ordinal regression (GMixed)
* Mixed Ordinal ANOVA-like model (GMixed)

# Docs

More informations can be found at [GAMLj page](https://gamlj.github.io/)

# Install in jamovi

Please install [jamovi](https://www.jamovi.org/download.html) and run it. Select the jamovi modules library and install GAMLj from there


<center>
<img width="600" src="https://gamlj.github.io/glm/install.png" class="img-responsive" alt="">
</center>

# Version 3

GAMLj has undergone a major upgrading with version 3.*. The upgrading made version 3 not compatible with previous versions of the module, meaning
that analyses that are done with previous versions cannot be opened with version 3. If you need to open and work with analyses done with GAMLj version < 3.0.0, you can install GAMLj legacy from the jamovi library. It will not interfere with GAMLj3.

## From GitHub

In your R script (or Rstudio) simply issue 

```

devtools::install_github("gamlj/gamlj")

```

## From source


You will first need to download [jamovi](https://www.jamovi.org/download.html). 

You can clone this repository and compile the module within R with 

```
library(jmvtools)

jmvtools::install()

```

# Previous Version (GAMLj 2.6.6 or below)

If you want to install the latest previous version of the module (Version 2.6.6) in R, run this:

```
devtools::install_github("gamlj/gamlj",ref="Version.2.6.6")

```




# Programmatic name

```
paste(paste(LETTERS[c(7,1,13,12)],collapse =""),paste(letters[10]),sep="")

















































































































































































































































































































































































































































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

## From GitHub

In your R script (or Rstudio) simply issue 

```

devtools::install_github("gamlj/gamlj")

```

## From source


You will first need to download [jamovi](https://www.jamovi.org/download.html). 

You can clone this repository and compile the module within R with 

```
library(jmvtools)

jmvtools::install()

```

# Previous Version (GAMLj 2.6.6 or below)

If you want to install the latest previous version of the module (Version 2.6.6) in R, run this:

```
devtools::install_github("gamlj/gamlj",ref="Version.2.6.6")

```




# Programmatic name

```
paste(paste(LETTERS[c(7,1,13,12)],collapse =""),paste(letters[10]),sep="")






















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

## From GitHub

In your R script (or Rstudio) simply issue 

```

devtools::install_github("gamlj/gamlj")

```

## From source


You will first need to download [jamovi](https://www.jamovi.org/download.html). 

You can clone this repository and compile the module within R with 

```
library(jmvtools)

jmvtools::install()

```

# Previous Version (GAMLj 2.6.6 or below)

If you want to install the latest previous version of the module (Version 2.6.6) in R, run this:

```
devtools::install_github("gamlj/gamlj",ref="Version.2.6.6")

```




# Programmatic name

```
paste(paste(LETTERS[c(7,1,13,12)],collapse =""),paste(letters[10]),sep="")

```
