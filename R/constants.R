

IMPROBABLE_SEQ<-"Xd2ludGVyaXNjb21pbmc;"


FACTOR_SYMBOL= "..f_f_f.."
LEVEL_SYMBOL = "..l_l_l.."
INTERACTION_SYMBOL=":"
B64_SYMBOL="..b_b_b.."
B64_REGEX="\\.\\.b_b_b\\.\\."
TCONV<-list()
TCONV[["glm.f"]]<-c("test","df","p")
TCONV[["mixed.f"]]<-c("test","df1","df2","p")

######## options not in the R syntax #####
NO_R_OPTS<-c(
            "model_terms",
            "factors",
            "covs",
            "dep",
            "re",
            "cluster",
            "donotrun",
            "comparison",
            "re_modelterms",
            "re_listing"
           )


######### warning ######################

WARNS<-list()

WARNS["noconv"]<-"The model did not converge. Results may be misleading or uninterpretable."


WARNS["means.covariates"]<-"Estimated means are estimated keeping constant
other effects in the model to the mean"
WARNS["means.interactions"]<-"Estimated means are estimated averaging across interacting variables"
WARNS["means.noemms"]<-"Estimated marginal means cannot be estimated. Please try to simplify your model"

### standard error #############

WARNS["stde.robust_test"]<-"Inferential tests and p-values are adjusted for heteroschedasticity."

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

WARNS["aliased"]<-"WARNING: Some of the coefficients cannot be estimated because
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
WARNS["r2.nogood"]<-"R-squared cannot be computed for this model"

######### posst hoc ###################
WARNS["ph.nojoy"]<-"Post-hoc tests cannot be estimated. Try simplifying your model removing interactions or covariates"
WARNS["ph.covariates"]<-"Post-hocs means are estimated keeping constant
other independent variable(s) in the model"
WARNS["ph.interactions"]<-"Post-hocs means are estimated averaging across interacting factors (if any) and setting interacting covariates to zero (if any)"

#### GLM Specifics ##########

WARNS["lm.zeromodel"]<-"SS are computed for predicted values equal to zero"
WARNS["error.zeromodel"]<-"Tests not available for zero intercept only models"
WARNS["nointercept"]<-"With zero intercept, the first factor coefficients are the estimated means of the factor levels"

### end of warnings ###
WARNS<-sapply(WARNS,function(a) gsub("\n"," ",a,fixed=T))

###############################################################

TRANS_WARNS<-list()
TRANS_WARNS[[1]]<-list(original="Respecify random",new="Random component variances may be to small to compute R-squares")
TRANS_WARNS[[2]]<-list(original="Model failed to converge with",new="There were problems in model convergence. Results may be biased. Try to specify a different random component.")
TRANS_WARNS[[3]]<-list(original="emmeans() results may be corrupted by removal",new=NULL)
TRANS_WARNS[[4]]<-list(original="Could not recover model data from",new=NULL)
TRANS_WARNS[[5]]<-list(original="Random slopes not present",new=NULL)
TRANS_WARNS[[5]]<-list(original="too close to zero",new=NULL)

########### Greek Letters  ###############

greek_vector <- c( # lowercase Greek letters
  alpha='\u03b1', beta='\u03b2', gamma='\u03b3', delta='\u03b4', epsilon='\u03b5', zeta='\u03b6',
  eta='\u03b7', theta='\u03b8', iota='\u03b9', kappa='\u03ba', lambda='\u03bb', mu='\u03bc',
  nu='\u03bd', xi='\u03be', omicron='\u03bf', pi='\u03c0', rho='\u03c1', sigma='\u03c3', tau='\u03c4',
  upsilon='\u03c5', phi='\u03c6', chi='\u03c7', psi='\u03c8', omega='\u03c9',
  # uppercase Greek letters
  Alpha='\u0391', Beta='\u0392', Gamma='\u0393', Delta='\u0394', Epsilon='\u0395', Zeta='\u0396',
  Eta='\u0397', Theta='\u0398', Iota='\u0399', Kappa='\u039a', Lambda='\u039b', Mu='\u039c',
  Nu='\u039d', Xi='\u039e', Omicron='\u039f', Pi='\u03a0', Rho='\u03a1', Sigma='\u03a3', Tau='\u03a4',
  Upsilon='\u03a5', Phi='\u03a6', Chi='\u03a7', Psi='\u03a8', Omega='\u03a9',
  # mathematical symbols
  infinity ='\u221e', leftrightarrow ='\u21d4', forall='\u2200', exist ='\u2203', notexist ='\u2204',
  emptyset ='\u2205', elementof='\u2208', notelementof='\u2209', proportional='\u221d',
  asymptoticallyEqual='\u2243', notasymptoticallyEqual='\u2244', approxEqual='\u2245', almostEqual='\u2248',
  leq='\u2264', lt="\u003c",gt="\u003e", geq='\u2265', muchless='\u226a', muchgreater='\u226b', leftarrow='\u21d0', rightarrow='\u21d2',
  equal='\uff1d', notEqual='\u2260', integral='\u222b', doubleintegral='\u222c', tripleintegral='\u222d',
  logicalAnd='\u2227', logicalOr='\u2228', intersection='\u2229', union='\u222a')


letter_chi2<-paste(greek_vector["chi"],'\u00B2',sep="")
letter_eta2<-paste(greek_vector["eta"],'\u00B2',sep="")
letter_peta2<-paste(greek_vector["eta"],'\u00B2',"p",sep="")
letter_omega2<-paste(greek_vector["omega"],'\u00B2',sep="")
letter_pomega2<-paste(greek_vector["omega"],'\u00B2',"p",sep="")
letter_epsilon2<-paste(greek_vector["epsilon"],'\u00B2',sep="")
letter_pepsilon2<-paste(greek_vector["epsilon"],'\u00B2',"p",sep="")


##########################

