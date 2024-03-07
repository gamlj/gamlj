Infomatic <- R6::R6Class(
  "Infomatic",
  class=TRUE, ## this and the next 
  cloneable=FALSE, ## should improve performance https://r6.r-lib.org/articles/Performance.html ###
  inherit = Scaffold,
  public=list(
    operator=NULL,
    caller=NULL,
    model_type=NULL,
    model=NULL,
    formula=NULL,
    distribution=NULL,
    family=NULL,
    link=NULL,
    direction=NULL,
    rcall=NULL,
    call=NULL,
    emmeans     =  NULL,
    predict     =  "response",
    fit         =  NULL,
    deptype     =  NULL,
    depnlevels  =  NULL,
    calloptions =  NULL,
    r2          =  NULL,
    optimized   =  FALSE,
    df          =   NULL,
    comparison  =  "Difference",
    posthoc_adjust   = c("bonferroni","holm","sidak","tukey","scheffe"),   
    omnibus_test= NULL,
    bootstrap   = TRUE,
    extra_info  = NULL,
    datamatic   = NULL,
    initialize=function(jmvobj,datamatic,formulas) {
      
      super$initialize(jmvobj)
      self$model_type        <- self$options$model_type
      self$datamatic         <- datamatic
      ## handle strange cases
      if (self$options$model_type=="lmer" && self$options$res_struct!="id")
                    self$model_type<-"lme"

      if (self$options$model_type=="logistic" && self$options$input_method=="success")
                    self$model_type<-"logistic_success"
      
      if (self$options$model_type=="probit" && self$options$input_method=="success")
                    self$model_type<-"probit_success"

      ## done
      
      self$formula           <- formulas$formula64()
      self$caller            <- self$options$.caller
      dep                    <- self$options$dep
      if (hasName(self$options,"dep2")) dep2<-self$options$dep2
      dlevs                  <- datamatic$variables[[tob64(dep)]]$levels_labels
      
      if (utils::hasName(options,"omnibus"))
          self$omnibus_test<-toupper(self$options$omnibus)
      
      if (self$caller=="glm") {
          self$fit<-c("lik" , "aic",  "bic",  "dev",  "dfr",  "over")
          self$r2 <-list(list(model=""))
      }
      if (self$caller=="glmer") {
        self$fit<-c("lik" , "aic",  "bic",  "dev",  "dfr",  "over")
        self$r2<-list(list(type="Marginal"),list(type="Conditional"))
      }
      
      if (self$model_type=="logistic_success") {
        self$fit<-c("lik" , "aic",  "bic")
        self$r2 <-list(list(model=""))
        self$warning<-list(topic="main_r2",message=paste("R-squared are not reliable for logistic models estimated with tabular data."))
      }
      if (self$model_type=="probit_success") {
        self$fit<-c("lik" , "aic",  "bic")
        self$r2 <-list(list(model=""))
        self$warning<-list(topic="main_r2",message=paste("R-squared are not reliable for probit models estimated with tabular data."))
      }
      

      if (self$model_type=="lm") {
        self$model        <-   c("Linear Model","OLS Model for continuous y")
        self$distribution <-  "gaussian"
        self$call         <-  "lm"
        self$rcall        <-   "stats::lm"
        self$deptype      <-  c("numeric","integer")
        self$r2 <-list(list(model=""))
        
      }
      if (self$model_type=="linear") {
        self$model        <-   c("Linear Model","ML Model for continuous y")
        self$distribution <-  "gaussian"
        self$call          <-   CALLS[[self$caller]]
        self$rcall         <-   FUNCS[[self$caller]]
        self$link         <-  "identity"
        self$direction    <-   c("y","Dependent variable scores")
        self$deptype      <-   c("numeric","integer")
        
      }
      
      if (self$model_type=="logistic") {
        
        self$model        <-   c("Logistic Model","Model for binary y")
        self$distribution  <-  "binomial"
        self$family        <-   "binomial"
        self$call          <-   CALLS[[self$caller]]
        self$rcall         <-   FUNCS[[self$caller]]
        if (self$caller=="glmer")
            self$calloptions<-list(control=lme4::glmerControl(optimizer = "bobyqa"))
        self$link          <-   "logit"
        self$emmeans       <-    "probabilities"
        self$comparison    <-  "OR"
        self$deptype       <-    "factor"
        self$depnlevels    <-    2
        
        ### compute directions ###
        dlevs              <-  datamatic$variables[[tob64(self$options$dep)]]$levels
        actual             <-  paste("P(",dep,"=",dlevs[2],") / P(",dep,"=",dlevs[1],")")
        theory             <-  "P(y=1)/P(y=0)"
        self$direction     <-  c(theory,actual)
        
      }
      
      if (self$model_type=="logistic_success") {
        
        self$model        <-   c("Logistic Model","Model for tabular data")
        self$distribution  <-  "binomial"
        self$family        <-   "binomial"
        self$call          <-   CALLS[[self$caller]]
        self$rcall         <-   FUNCS[[self$caller]]
        self$link          <-   "logit"
        self$emmeans       <-    "probabilities"
        self$comparison    <-  "OR"
        self$deptype       <-    "integer"

        ### compute directions ###
        actual             <-  paste("P(",dep,") / P(",dep2,")")
        theory             <-  "P(Success)/P(Failure)"
        self$direction     <-  c(theory,actual)
        self$bootstrap     <-  FALSE
        
      }

      if (self$model_type=="probit") {
        
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
        dlevs              <-   datamatic$variables[[tob64(self$options$dep)]]$levels
        theory             <-   "P(y=1)"
        actual             <-   paste("P(",dep,"=",dlevs[2],")")
        self$direction     <-   c(theory,actual)
      }
      
      if (self$model_type=="probit_success") {
        
        self$model        <-   c("Probit Model","Model for tabular data")
        self$distribution  <-  "binomial"
        self$family        <-  "binomial(link='probit')"
        self$call          <-  CALLS[[self$caller]]
        self$rcall         <-  FUNCS[[self$caller]]
        self$link          <-  "probit"
        self$emmeans       <-  "probabilities"
        self$comparison    <-  "OR"
        self$deptype       <-  "factor"

        ### compute directions ###
        actual             <-  paste("P(",dep,") / P(",dep2,")")
        theory             <-  "P(Success)/P(Failure)"
        self$direction     <-  c(theory,actual)
        self$bootstrap     <-  FALSE
      }

      
      
      
      if (self$model_type=="poisson") {
        
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
      if (self$model_type=="poiover") {
        
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
      if (self$model_type=="nb") {
        
        self$model         <-   c("Negative Binomial Model","Model for overdispersed count y")
        self$call          <-   "glm(er).nb"
        self$distribution  <-   "nb"
        self$rcall         <-   NB[[self$caller]]
        self$link          <-   "log"
        self$emmeans       <-   "expected counts"
        self$direction     <-   c("y","Dependent variable counts")
        self$deptype       <-   "integer"
        self$comparison    <-    "Ratio"
      }
      
      if (self$model_type=="beta") {
        
        self$model         <-   c("Beta regression","Model for proportions")
        self$call          <-   "betareg"
        self$distribution  <-   "beta"
        self$rcall         <-   "betareg::betareg"
        self$link          <-   "logit"
        self$emmeans       <-   "prop."
        self$direction     <-   c("y","Dependent variable proportion")
        self$deptype       <-   "numeric"
        if (self$options$preds_phi)
              self$formula <-   paste(formulas$fixed_formula64(),strsplit(formulas$fixed_formula64(),"~")[[1]][[2]],sep = "|")
      }

      if (self$model_type=="custom") {
       
        self$model         <-   c("User Model","Generalized model")
        self$family        <-    paste0(self$options$custom_family,"('",self$options$custom_link,"')")
        self$distribution  <-    self$options$custom_family
        self$call          <-    CALLS[[self$caller]]
        self$rcall         <-    FUNCS[[self$caller]]
        self$link          <-    self$options$custom_link
        self$emmeans       <-   "the response metric"
        self$direction     <-   c("y","Dependent variable scores")
        self$deptype       <-   c("numeric","integer","factor")
      }

      if (self$model_type=="ordinal") {
        
        self$model         <-   c("Cumlative Link Model","Proportional odds logistic")
        self$distribution  <-    "logistic"
        self$call          <-    ORDINAL[[self$caller]]
        self$rcall         <-     ORDINAL[[self$caller]]
        self$calloptions   <-    list(model=TRUE, Hess=TRUE)
        self$link          <-    "logit"
        self$emmeans       <-   "expected class"
        self$comparison    <-    "Ratio"
        self$predict       <-     "class"
        ### compute direction ###
        theory             <-     paste('P(Y',greek_vector['leq'],'j)/P(Y', greek_vector['gt'],' j)')
        actual             <-     paste("j=",paste(dlevs,collapse = " | "))
        self$direction     <-   c(theory, actual)
        #########################
        self$deptype       <-   "factor"
        self$fit           <-   c("lik" , "aic",  "bic",  "dev")
        
      }
      
      if (self$model_type=="multinomial") {
        
        
        self$model         <-   c("Multinomial Model","Model for categorical y")
        self$distribution  <-    "multinomial"
        if (self$caller=="glm") {
            self$call          <-    "nnet::multinom"
            self$rcall         <-    "nnet::multinom"
            self$calloptions   <-    list(model=TRUE)
        } 
        if (self$caller=="glmer") {

          self$call          <-    "mclogit::mblogit"
          self$rcall         <-    "mclogit::mblogit"
          self$formula       <-    NULL
          self$calloptions   <-    list(formula=as.formula(formulas$fixed_formula64()),
                                        random=formulas$listify_random_formulas64(),
                                        model=TRUE,
                                        maxit=50,
                                        estimator="REML",
                                        trace=FALSE,
                                        trace.inner=FALSE)
          self$r2 <-list(list(type="McFadden"))
        } 
        
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
        if (self$caller=="glmer")         self$fit           <-   c("aic",  "bic",  "dev")

        
      }
      
      if (self$model_type=="lmer") {
        
        self$model         <-   c("Mixed Model","Linear Mixed model for continuous y")
        self$distribution  <-    "gaussian"
        self$call          <-    "lmer"
        self$rcall         <-    "estimate_lmer"
        self$calloptions   <-    list(reml=self$options$reml,optimizers=c("bobyqa","Nelder_Mead","nloptwrap"))
        self$optimized     <-   TRUE
        self$direction     <-   c("y","Dependend variable scores")
        self$deptype       <-   c("numeric","integer")
        self$fit           <-   c("lik" , "aic",  "bic")
        self$r2            <-   list(list(type="Marginal"),list(type="Conditional"))
        self$df            <-   self$options$df_method
      }
      

      if (self$model_type=="lme") {
        
        self$formula       <-   NULL
        self$model         <-   c("Mixed Model","Linear Mixed model for continuous y")
        self$distribution  <-    "gaussian"
        self$call          <-    "lme"
        self$rcall         <-    "estimate_lme"
        form               <-    paste("~1|",tob64(self$options$cluster[1]))
        if (self$options$res_struct=="cs")  cor<-nlme::corCompSymm 
        if (self$options$res_struct=="un")  cor<-nlme::corSymm 
        if (self$options$res_struct=="ar1") cor<-nlme::corAR1
        if (self$options$reml) method="REML" else method="ML"
        self$calloptions   <-    list(fixed=as.formula(formulas$fixed_formula64()),
                                      random=formulas$listify_random_formulas64(),
                                      cor=cor,
                                      form=form,
                                      method=method
        )
        self$optimized     <-   FALSE
        self$direction     <-   c("y","Dependend variable scores")
        self$deptype       <-   c("numeric","integer")
        self$fit           <-   c("lik" , "aic",  "bic")
        self$r2            <-   list(list(type="Marginal"),list(type="Conditional"))
        if (self$options$res_struct=="un") res<-"Unstructured" else res<-"AutoRegressive 1"
        self$extra_info    <-   list(info="Residuals",value=res,specs=paste("within cluster",self$options$cluster[1]))
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

      if (is.something(self$omnibus_test))
        alist[["omnibus"]]<-list(info="Omnibus Tests",value=self$omnibus_test,specs="") 
      
      if (is.something(self$df))
        alist[["df"]]<-list(info="DF method",value=self$df,specs="") 

      if (is.something(self$extra_info))
        alist[["extra_info"]]<-self$extra_info 
      
      alist[["sample"]]   <-  list(info="Sample size",value=".",specs="")
      alist[["conv"]]     <-  list(info="Converged",value="no yet", specs="")

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
      
    },
    check_dependent = function() {

      if (self$model_type %in% c("logistic_success","probit_success")) {
         dep2 <- self$optionValue("dep2")
         dep2obj<-self$datamatic$variables[[tob64(dep2)]]

         if (! (dep2obj$type %in%  c("integer", "numeric")) )
             stop(self$model[1]," for tabular data requires numeric table columns (",dep2obj,")")

         if (! dep2obj$isInteger )
             stop(self$model[1]," for tabular data requires table columns (",dep2obj,") of type integer")


         if (! (self$datamatic$dep$type %in%  c("integer", "numeric")) )
             stop(self$model[1]," for tabular data requires numeric table columns (",self$datamatic$dep,")")

         if (! self$datamatic$dep$isInteger )
             stop(self$model[1]," for tabular data requires table columns (",self$datamatic$dep,") of type integer")

         return()
      }
      
      if (!(self$datamatic$dep$type %in% self$deptype)) {

        t2  <-  paste(self$deptype,collapse = " or ")
        t1  <-  self$datamatic$dep$type
        m   <-   self$model[1]
        if (self$options$.interface=="jamovi") {
                t2<-gsub("numeric","Continuous",t2,fixed = TRUE)
                t2<-gsub("factor","Nominal",t2,fixed=TRUE)
                t2<-gsub("integer","Measurement type=`Continuous`, Data type=`Integer`",t2,fixed=TRUE)
                }
        msg<-paste("Dependent variable is of type",t1,".",m,"requires variable of type: ",t2)
                                    stop(msg)
      }

    ### when necessary, check the number of levels
        if (is.something(self$infomatic$depnlevels)) {
                nvar  <-  self$datamatic$dep$nlevels
                nreq  <-  self$infomatic$depnlevels
                if ( nreq > 0 ) {
                     if ( nreq !=  nvar)
                        stop(paste(self$model[1],"requires exactly",nreq,"levels"))
                     } else {                               
                        if (nvar < abs(nreq) )
                           stop(paste(self$model[1],"requires a minimum of",abs(nreq),"levels"))
                     }
        }
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
LINFO[["logit"]]      <-  c("Logit","Log of the odd of y")
LINFO[["slogit"]]     <-  c("Logit","Log of the odd of each level of y over y=0")
LINFO[["probit"]]     <-  c("Probit","Inverse of normal CDF for P(y=1)")
LINFO[["1/mu^2"]]     <-  c("1/mu^2","Inverse of y squared")
LINFO[["inverse"]]    <-  c("1/mu","Inverse of y")
LINFO[["sqrt"]]       <-  c("Square root","Square root of y")


CALLS<-list()
CALLS[["glm"]]<-"stats::glm"
CALLS[["glmer"]]<-"lme4::glmer"

FUNCS<-list()
FUNCS[["glm"]]<-"stats::glm"
FUNCS[["glmer"]]<-"lme4::glmer"

NB<-list()
NB[["glm"]]<-"MASS::glm.nb"
NB[["glmer"]]<-"lme4::glmer.nb"

ORDINAL<-list()
ORDINAL[["glm"]]<-"ordinal::clm"
ORDINAL[["glmer"]]<-"ordinal::clmm"



FIT<-list()
FIT[["lik"]]   <-  list(info="LogLikelihood",  specs="")
FIT[["aic"]]   <-  list(info="AIC",            specs="Less is better")
FIT[["bic"]]   <-  list(info="BIC",            specs="Less is better")
FIT[["dev"]]   <-  list(info="Deviance",       specs="Less is better")
FIT[["dfr"]]   <-  list(info="Residual DF",    specs="")
FIT[["over"]]  <-  list(info="Chi-squared/DF", specs="Overdispersion indicator")



# .explainPrediction=function() {
#   
#   if (!utils::hasName(self$tab_info,"dir"))
#     return()
#   
#   dlevs<-self$datamatic$variables[[tob64(self$self$options$dep)]]$levels
#   dep<-self$self$options$dep
#   if (self$self$options$model_type %in% c("logistic")) {
#     self$tab_info[["dir"]]$value<-paste("P(",dep,"=",dlevs[2],") / P(",dep,"=",dlevs[1],")")
#     
#   }
#   if (self$self$options$model_type %in% c("probit")) {
#     self$tab_info[["dir"]]$value<-paste("P(",dep,"=",dlevs[2],")")
#   }
#   if (self$self$options$model_type %in% c("multinomial")) {
#     self$tab_info[["dir"]]$value<-paste(paste0("P(",dep,"=",dlevs[-1],")"),paste0("P(",dep,"=",dlevs[1],")"),sep="/",collapse = " , ")
#   }
#   
#   if (self$tab_info[["dir"]]$value=="")
#     self$tab_info[["dir"]]$value<-paste(dep,"scores")
# }
