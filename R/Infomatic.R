Infomatic <- R6::R6Class(
  "Infomatic",
  class=TRUE, ## this and the next 
  cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
  public=list(
    caller=NULL,
    modeltype=NULL,
    model=NULL,
    distribution=NULL,
    family=NULL,
    link=NULL,
    direction=NULL,
    rcall=NULL,
    call=NULL,
    emmeans=NULL,
    predict="response",
    fit=NULL,
    deptype=NULL,
    depnlevels=NULL,
    calloptions=NULL,
    r2=NULL,
    optimized=FALSE,
    df=NULL,
    comparison="Difference",
    initialize=function(options,datamatic) {
      self$modeltype<-options$model_type
      self$caller<-options$.caller
      dep<-options$dep
      dlevs<-datamatic$variables[[tob64(dep)]]$levels_labels
      
      
      if (self$caller=="glm") {
          self$fit<-c("lik" , "aic",  "bic",  "dev",  "dfr",  "over")
      }
      
      

      if (self$modeltype=="lm") {
        self$model        <-   c("Linear Model","OLS Model for continuous y")
        self$distribution <-  "gaussian"
        self$call         <-  "lm"
        self$rcall        <-   "stats::lm"
        self$deptype      <-  c("numeric","integer")
        
      }
      if (self$modeltype=="linear") {
        self$model        <-   c("Linear Model","ML Model for continuous y")
        self$distribution <-  "gaussian"
        self$call          <-   CALLS[[self$caller]]
        self$rcall         <-   FUNCS[[self$caller]]
        self$link         <-  "identity"
        self$direction    <-   c("y","Dependent variable scores")
        self$deptype      <-   c("numeric","integer")
        
      }
      
      if (self$modeltype=="logistic") {
        
        self$model        <-   c("Logistic Model","Model for binary y")
        self$distribution  <-  "binomial"
        self$family        <-   "binomial"
        self$call          <-   CALLS[[self$caller]]
        self$rcall         <-   FUNCS[[self$caller]]
        self$link          <-   "logit"
        self$emmeans       <-    "probabilities"
        self$comparison    <-  "OR"
        self$deptype       <-    "factor"
        self$depnlevels    <-    2
        
        ### compute directions ###
        dlevs              <-  datamatic$variables[[tob64(options$dep)]]$levels
        actual             <-  paste("P(",dep,"=",dlevs[2],") / P(",dep,"=",dlevs[1],")")
        theory             <-  "P(y=1)/P(y=0)"
        self$direction     <-  c(theory,actual)
        
      }
      if (self$modeltype=="probit") {
        
        self$model         <-   c("Probit Model","Model for binary y")
        self$distribution  <-  "binomial"
        self$family        <-   "binomial(link='probit')"
        self$call          <-   CALLS[[self$caller]]
        self$rcall         <-   FUNCS[[self$caller]]
        self$link          <-   "probit"
        self$emmeans       <-   "probabilities"
        self$deptype       <-    "factor"
        self$depnlevels    <-    2
        self$comparison    <-    "OR"
        ### compute direction ###
        dlevs              <-   datamatic$variables[[tob64(options$dep)]]$levels
        theory             <-   "P(y=1)"
        actual             <-   paste("P(",dep,"=",dlevs[2],")")
        self$direction     <-   c(theory,actual)
      }
      if (self$modeltype=="poisson") {
        
        self$model         <-   c("Poisson Model","Model for count y")
        self$distribution  <-   "poisson"
        self$family        <-   "stats::poisson()"
        self$call          <-   CALLS[[self$caller]]
        self$rcall         <-   FUNCS[[self$caller]]
        self$link          <-   "log"
        self$emmeans       <-   "expected counts"
        self$direction     <-   c("y","Dependent variable counts")
        self$deptype       <-   "integer"
        self$comparison    <-    "Ratio"
        
      }
      if (self$modeltype=="poiover") {
        
        self$model         <-   c("Poisson Model","Model for overdispersed count y")
        self$distribution  <-   "quasi-poisson"
        self$family        <-   "stats::quasipoisson()"
        self$call          <-   CALLS[[self$caller]]
        self$rcall         <-   FUNCS[[self$caller]]
        self$link          <-   "log"
        self$emmeans       <-   "expected counts"
        self$deptype       <-   "integer"
        self$comparison    <-    "Ratio"
        
        ### compute direction ###
        self$direction     <-   c("y","Dependent variable counts")
        self$deptype       <-   "integer"
        
      }
      if (self$modeltype=="nb") {
        
        self$model         <-   c("Negative Binomial Model","Model for overdispersed count y")
        self$distribution  <-   "nb"
        self$rcall         <-   "MASS::glm.nb"
        self$link          <-   "log"
        self$emmeans       <-   "expected counts"
        self$direction     <-   c("y","Dependent variable counts")
        self$deptype       <-   "integer"
        self$comparison    <-    "Ratio"
      }
      
      if (self$modeltype=="custom") {
        
        self$model         <-   c("User Model","Generalized model")
        self$family        <-    paste(options$custom_family,"(",options$custom_link,")")
        self$distribution  <-    options$custom_family
        self$call          <-    CALLS[[self$caller]]
        self$rcall         <-    FUNCS[[self$caller]]
        self$link          <-    options$custom_link
        self$emmeans       <-   "the response metric"
        self$direction     <-   c("y","Dependent variable scores")
        self$deptype       <-   c("numeric","integer","factor")
      }

      if (self$modeltype=="ordinal") {
        
        self$model         <-   c("Ordinal GLM","Proportional odds logistic")
        self$distribution  <-    "logistic"
        self$call          <-    "ordinal"
        #self$rcall         <-    "ordinal::clm"  
        self$rcall        <-    "MASS::polr"
        self$calloptions   <-    list(model=TRUE, Hess=TRUE)
        self$link          <-    "logit"
        self$emmeans       <-   "expected class"
        self$comparison    <-    "Ratio"
        
        ### compute direction ###
        theory             <-     paste('P(Y',greek_vector['leq'],'j)/P(Y', greek_vector['gt'],' j)')
        actual             <-     paste("j=",paste(dlevs,collapse = " | "))
        self$direction     <-   c(theory, actual)
        #########################
        self$deptype       <-   "factor"
        self$fit           <-   c("lik" , "aic",  "bic",  "dev")
        
      }
      
      if (self$modeltype=="multinomial") {
        
        self$model         <-   c("Multinomial Model","Model for categorical y")
        self$distribution  <-    "multinomial"
        self$call          <-    "multinomial"
        self$rcall         <-    "nnet::multinom"
        self$calloptions   <-    list(model=TRUE)
        self$link          <-    "logit"
        self$emmeans       <-   "probabilities"
        self$predict       <-   "probs"
        ### compute direction ###
        theory             <-   "P(Y=j)/P(Y=0)"
        actual             <-  paste(paste0("P(",dep,"=",dlevs[-1],")"),paste0("P(",dep,"=",dlevs[1],")"),sep="/",collapse = " , ")
        self$direction     <-   c(theory, actual)
        #########################
        self$deptype       <-   "factor"
        self$depnlevels    <-   -3
        self$fit           <-   c("lik" , "aic",  "bic",  "dev")
        
      }
      
      if (self$modeltype=="lmer") {
        
        self$model         <-   c("Mixed Model","Linear Mixed model for continuous y")
        self$distribution  <-    "gaussian"
        self$call          <-    "lmer"
        self$rcall         <-    "estimate_lmer"
        self$calloptions   <-    list(reml=options$reml,optimizers=c("bobyqa","Nelder_Mead","nloptwrap"))
        self$optimized     <-   TRUE
        self$direction     <-   c("y","Dependend variable scores")
        self$deptype       <-   c("numeric","integer")
        self$fit           <-   c("lik" , "aic",  "bic")
        self$r2            <-   c("Marginal","Conditional")
        self$df            <-   options$df_method
      }
      
      
      
    },
    
    info_table=function() {
      
      alist<-list()
      alist[["model"]]  <-  private$.model()
      alist[["call"]]   <-  private$.call()
      alist[["dist"]]   <-  private$.dist()
      
      if (is.something(self$link))
        alist[["link"]]   <-  private$.link()
      
      if (is.something(self$direction))
             alist[["dir"]]   <-  private$.dir()
     
       if (self$optimized)
             alist[["optim"]]<-list(info="Optimizer",value="",specs="") 

      if (is.something(self$df))
        alist[["df"]]<-list(info="DF method",value=self$df,specs="") 
      
      alist[["sample"]]   <-  list(info="Sample size",value=".",specs="")
      alist[["conv"]]   <-  list(info="Converged",value="no yet", specs="")
      
      return(alist)
      
    },
    info_fit=function() {
      
      if (is.null(self$fit))
        return()
      
      alist<-list()
      for (f in self$fit) {
        alist[[f]]<-FIT[[f]]  
      }
      return(alist)
      
    }
    
    
  ), # end of public
  private = list(
    
    .model=function() {
      return(list(info="Model Type", value=self$model[[1]], specs=self$model[[2]]))
    },
    
    .call=function() {
      list(info="Model",value=self$call,specs="")
    },
    .dist=function() {
      list(info="Distribution",value=DINFO[[self$distribution]][[1]],specs=DINFO[[self$distribution]][[2]])
    },
    .link=function() {
      list(info="Link function",value=LINFO[[self$link]][[1]],specs=LINFO[[self$link]][[2]])
    },
    .dir=function() {
      list(info="Direction",value=self$direction[1],specs=self$direction[2])
    }
    
    
    
  ) # end of private
)

###### info definition for models ##########

DINFO<-list()
DINFO[["gaussian"]]<-c("Gaussian","Normal distribution of residuals")
DINFO[["poisson"]]<-c("Poisson","Model for count data")
DINFO[["binomial"]]<-c("Binomial","Dichotomous event distribution of y")
DINFO[["multinomial"]]<-c("Multinomial","Multi-event distribution of y")
DINFO[["nb"]]<-c("Negative binomial","Rare event with overdispersion")
DINFO[["quasi-poisson"]]<-c("Quasi-Poisson","Rare event with overdispersion")
DINFO[["Gamma"]]<-c("Gamma","Skewed continuous distribution")
DINFO[["logistic"]]<-c("Logistic","")

LINFO<-list()
LINFO[["identity"]]   <-  c("Identity","Coefficients in the same scale of y")
LINFO[["log"]]        <-  c("log","Coefficients are in the log(y) scale")
LINFO[["logit"]]      <-  c("Logit","Log of the odd of y=1 over y=0")
LINFO[["slogit"]]     <-  c("Logit","Log of the odd of each level of y over y=0")
LINFO[["probit"]]     <-  c("Probit","Inverse of normal CDF for P(y=1)")
LINFO[["1/mu^2"]]     <-  c("1/mu^2","Inverse of y squared")
LINFO[["inverse"]]    <-  c("1/mu","Inverse of y")
LINFO[["sqrt"]]       <-  c("Square root","Square root of y")


CALLS<-list()
CALLS[["glm"]]<-"glm"

FUNCS<-list()
FUNCS[["glm"]]<-"stats::glm"

FIT<-list()
FIT[["lik"]]   <-  list(info="LogLikelihood",  specs="")
FIT[["aic"]]   <-  list(info="AIC",            specs="Less is better")
FIT[["bic"]]   <-  list(info="BIC",            specs="Less is better")
FIT[["dev"]]   <-  list(info="Deviance",       specs="Less is better")
FIT[["dfr"]]   <-  list(info="Residual DF",    specs="")
FIT[["over"]]  <-  list(info="Chi-squared/DF", specs="Overdispersion indicator")



.explainPrediction=function() {
  
  if (!hasName(self$tab_info,"dir"))
    return()
  
  dlevs<-self$datamatic$variables[[tob64(self$options$dep)]]$levels
  dep<-self$options$dep
  if (self$options$model_type %in% c("logistic")) {
    self$tab_info[["dir"]]$value<-paste("P(",dep,"=",dlevs[2],") / P(",dep,"=",dlevs[1],")")
    
  }
  if (self$options$model_type %in% c("probit")) {
    self$tab_info[["dir"]]$value<-paste("P(",dep,"=",dlevs[2],")")
  }
  if (self$options$model_type %in% c("multinomial")) {
    self$tab_info[["dir"]]$value<-paste(paste0("P(",dep,"=",dlevs[-1],")"),paste0("P(",dep,"=",dlevs[1],")"),sep="/",collapse = " , ")
  }
  
  if (self$tab_info[["dir"]]$value=="")
    self$tab_info[["dir"]]$value<-paste(dep,"scores")
}
