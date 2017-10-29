TCONV<-list()
TCONV[["glm.f"]]<-c("test","df","p")

MINFO<-list()
MINFO[["linear"]]<-list("name"=c("Linear","Classical Regression/ANOVA"),
                         "link"=c("Indentiy","Coefficients in the same scale of y"),
                         "distribution"=c("Gaussian","Normal distribution of residual"))
MINFO[["poisson"]]<-list("name"=c("Poisson","Model for count data"),
                        "link"=c("log","Coefficients are in the log(y) scale"),
                        "distribution"=c("Poisson","Rare events distribution of y"))
MINFO[["logistic"]]<-list("name"=c("Logistic","Model for binary y"),
                         "link"=c("logit","Log of the odd of y=1 over y=0"),
                         "distribution"=c("Binomial","Dicotomous event distribution of y"))
MINFO[["multinomial"]]<-list("name"=c("Multinomial","Model for categorical y"),
                          "link"=c("logit","Log of the odd of each category over y=0"),
                          "distribution"=c("Multinomial","Multi-event distribution of y"),
                          "note"=c("Warning","Not yet implemented"))

WARNS<-list()

WARNS["ph.covariates"]<-"Post-hocs means are estimated keeping constant
other independent variable(s) in the model"
WARNS["ph.interactions"]<-"Post-hocs means are estimated averaging across interacting factors (if any) and setting interacting covariates to zero (if any)"


WARNS["means.covariates"]<-"Estimated means are estimated keeping constant
other independent variable(s) in the model to the mean"
WARNS["means.interactions"]<-"Estimated means are estimated averaging across interacting variables"

WARNS["se.interactions"]<-"Simple effects are estimated setting higher order
 moderator (if any) in covariates to zero and averaging
 across moderating factors levels (if any)"

WARNS["se.covariates"]<-"Simple effects are estimated keeping constant other
 independent variable(s) in the model"
WARNS["se.noint"]<-"No interaction involving the simple effects variables 
 is present in the model. Simple effects are equivalent to main effects"

WARNS["se.df"]<-"Simple effects F-tests are Type III Wald F tests with Kenward-Roger df"

WARNS["ano.aliased"]<-"WARNING: Some of the coefficients cannot be estimated because
they are perfectly correlated with other coefficients in the model.
This can be due to empty cells in the design or perfectly correlated covariates.
The results may be uninterpretable."

WARNS["lmer.noreml"]<-"Simple effects F test available only for linear mixed model fit by REML"

WARNS["lmer.df"]<-"DF and p-values cannot be computed without fixed effects"

WARNS["lmer.zerovariance"]<-"The model did not converge, so DF and p-values cannot be computed"

WARNS["lmer.init"]<-"Specify at least one cluster variable: A random intecepts model
will be estimated"


WARNS["lmer.nogood"]<-"Results may be uninterpretable or misleading. Try to refine your model."


WARNS<-sapply(WARNS,function(a) gsub("\n"," ",a,fixed=T))
#WARNS<-sapply(WARNS,function(a) gsub("  ","",a,fixed=T))
