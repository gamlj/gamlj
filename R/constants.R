#' @import lmerTest

GAMLj_DEBUG=T
GAMLj_INFO=T

IMPROBABLE_SEQ<-";._.Xd2ludGVyaXNjb21pbmc._.;"
DUMMY_TAIL<-"_._._"

TCONV<-list()
TCONV[["glm.f"]]<-c("test","df","p")
TCONV[["mixed.f"]]<-c("test","df1","df2","p")

################ model info for gzlm ###############
DINFO<-list()
DINFO[["gaussian"]]<-c("Gaussian","Normal distribution of residual")
DINFO[["poisson"]]<-c("Poisson","Model for count data")
DINFO[["binomial"]]<-c("Binomial","Dichotomous event distribution of y")
DINFO[["multinomial"]]<-c("Multinomial","Multi-event distribution of y")
DINFO[["nb"]]<-c("Negative binomial","Rare event with overdispersion")
DINFO[["poiover"]]<-c("Quasi-Poisson","Rare event with overdispersion")
DINFO[["Gamma"]]<-c("Gamma","Skewed continuous distribution")

LINFO<-list()
LINFO[["identity"]]<-c("Identity","Coefficients in the same scale of y")
LINFO[["log"]]<-c("log","Coefficients are in the log(y) scale")
LINFO[["logit"]]<-c("Logit","Log of the odd of y=1 over y=0")
LINFO[["slogit"]]<-c("Logit","Log of the odd of each level of y over y=0")
LINFO[["probit"]]<-c("Probit","Inverse of normal CDF for P(y=1)")
LINFO[["1/mu^2"]]<-c("1/mu^2","Inverse of y squared")
LINFO[["inverse"]]<-c("1/mu","Inverse of y")
LINFO[["sqrt"]]<-c("Square root","Square root of y")

MINFO<-list()
MINFO[["linear"]]<-list("name"=c("Linear","Classical Regression/ANOVA"),
                        "call"="glm",emmeanTitle="Mean")
MINFO[["poisson"]]<-list("name"=c("Poisson","Model for count data"),
                         "call"="glm",emmeanTitle="Mean Count")
MINFO[["logistic"]]<-list("name"=c("Logistic","Model for binary y"),
                          "call"="glm",emmeanTitle="Prob.")
MINFO[["probit"]]<-list("name"=c("Probit","Model for binary y"),
                        "call"="glm",emmeanTitle="Prob.")

MINFO[["multinomial"]]<-list("name"=c("Multinomial","Model for categorical y"),
                             "call"="multinom",emmeanTitle="Prob.")

MINFO[["nb"]]<-list("name"=c("Negative binomial","Model for count data"),
                    "call"="glm.nb",emmeanTitle="Mean Count")

MINFO[["poiover"]]<-list("name"=c("Quasi-Poisson","Model for count data"),
                         "call"="glm",emmeanTitle="Mean Count")

MINFO[["custom"]]<-list("name"=c("Custom","Model with custom family"),
                         "call"="glm",emmeanTitle="Mean")


###############################################################

######### warning ######################

WARNS<-list()



WARNS["means.covariates"]<-"Estimated means are estimated keeping constant
other effects in the model to the mean"
WARNS["means.interactions"]<-"Estimated means are estimated averaging across interacting variables"
WARNS["means.noemms"]<-"Estimated marginal means cannot be estimated. Please try to simplify your model"

### simple effects #############
WARNS["se.interactions"]<-"Simple effects are estimated setting higher order
 moderator (if any) in covariates to zero and averaging
 across moderating factors levels (if any)"

WARNS["se.covariates"]<-"Simple effects are estimated keeping constant other
 independent variable(s) in the model"

WARNS["se.noint"]<-"No interaction involving the simple effects variables 
 is present in the model. Simple effects are equivalent to main effects"

WARNS["se.df"]<-"Simple effects F-tests are Type III Wald F tests with Kenward-Roger df"

WARNS["se.noluck"]<-"Simple effects cannot be estimated. Refine the model or the covariates conditioning (if any)"

WARNS["se.largen"]<-"z-tests are computed because the number of observations exceeds 3000"

#### anova #########

WARNS["ano.aliased"]<-"WARNING: Some of the coefficients cannot be estimated because
they are perfectly correlated with other coefficients in the model.
This can be due to empty cells in the design or perfectly correlated covariates.
The results may be uninterpretable."

######### mixed model ################
WARNS["lmer.df"]<-"DF and p-values cannot be computed for fixed effect parameters"
WARNS["lmer.zerovariance"]<-"The model did not converge, so DF and p-values cannot be computed"
WARNS["lmer.init"]<-"Specify at least one cluster variable: A random intecepts model
will be estimated"
WARNS["lmer.nogood"]<-"Results may be uninterpretable or misleading. Try to refine your model."
WARNS["lmer.chisq"]<-"ML estimation of F-Tests failed. Chi-squared tests were performed."
WARNS["lmer.singular"]<-"(Almost) singular fit. Maybe random coefficients variances are too small or correlations among them too large."

######### posst hoc ###################
WARNS["ph.nojoy"]<-"Post-hoc tests cannot be estimated. Try simplifying your model removing interactions or covariates"
WARNS["ph.covariates"]<-"Post-hocs means are estimated keeping constant
other independent variable(s) in the model"
WARNS["ph.interactions"]<-"Post-hocs means are estimated averaging across interacting factors (if any) and setting interacting covariates to zero (if any)"

#### GLM Specifics ##########

WARNS["glm.zeromodel"]<-"SS are computed for predicted values equal to zero"


### end of warnings ###
WARNS<-sapply(WARNS,function(a) gsub("\n"," ",a,fixed=T))

###############################################################


########## contrast definition info ################

CONTR<-list()


######## lme4 optimizers #########

OPTIMIZERS<-c("bobyqa","Nelder_Mead","nloptwrap")

