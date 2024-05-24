
### marginal effects ###

es.marginals<- function(x,...) UseMethod(".margins")

.margins.default<-function(obj) {

     model       <-  obj$model
     jinfo("EFFECTSIZE: margins default for model of class", class(model))  
     ciWidth     <-  obj$ciwidth
     results     <-  try_hard(marginaleffects::avg_slopes(model))
     params      <-  results$obj
     if (!isFALSE(results$error)) {
         obj$error<-list(topic="main_marginals",message="Marginal effects cannot be computed for this model")
        return()
      }    
  
     if (obj$option("ci_method",c("quantile","bcai"))) {
          method="boot"
          switch (object$options$ci_method,
             quantile = type <- "perc",
             bcai     = type  <- "bca"
          )
          params<-as.data.frame(marginaleffects::inferences(params,method="boot",R=obj$options$boot_r,conf_type=type))
       }
  params<-as.data.frame(params)
  names(params)<-transnames(names(params),
                            list("source"="term",
                                 "response" ="group",
                                 "se"="std.error",
                                 "estimate"="AME",
                                 "test"="statistic",
                                 "est.ci.lower"="conf.low",
                                 "est.ci.upper"="conf.high",
                                 "p"="p.value")
                             )
      params$contrast <- fromb64(params$contrast)
      params$source   <- fromb64(params$source)
      if (hasName(params,"response"))
            params$response    <- fromb64(params$response)
      mark(params)
      return(params)
  
}      

.zmargins<- function(x,...) UseMethod("..margins")

..margins.default<-function(model,obj) {
}

..margins.multinom<-function(model,obj) {

  jinfo("EFFECTSIZE: margins multinom for model of class", class(model))  
  
  offset         <-  stats::offset
  ciWidth        <-  obj$ciwidth
  data           <-  insight::get_data(model,source="frame")
  groups         <-  model$lev[-1]
  ref_lev        <-  model$lev[1]
  form           <-  stats::formula(model$terms)
  dep            <-  all.vars(form)[1]
  results        <-  list()

  if (obj$option("ci_method",c("quantile","bcai")))
        obj$warning<-list(topic="main_marginals",message="C.I for multinomial margins are available only with the delta method.")    
  for (g in groups) {
    .data<-data[data[[dep]]==g | data[[dep]]==ref_lev,]
    try_hard( {
      .model<-stats::glm(form,.data,family=stats::binomial())
      .results<-summary(margins::margins(.model),level=ciWidth,by_factor=FALSE)
    })
    .results$or<-1:nrow(.results)
    ladd(results) <- .results
  }
  results<-as.data.frame(do.call("rbind",results))
  results$response<-fromb64(paste(groups,"-",ref_lev))
  results<-results[order(results$response,results$or),]
  results
}

# at the moment, ordinal::clm does not give confidence intervals or inferential tests
# in margings. So, we re-estimaste the model with MASS::polr.  
# We do not use MASS:polr for other estimation because it does not work well with parameters::bootstrap_model() emmeans

..margins.clma<-function(model,obj) {
  jinfo("EFFECTSIZE: margins clm for model of class", class(model))  
   vce<- "delta"
   if (obj$option("ci_method",c("quantile","bcai")))
     vce <- "bootstrap"
   ciWidth        <-  obj$ciwidth
   data           <-  model$model
   .model <- MASS::polr(model$call$formula,data=model$model,Hess = T)
   m              <-  try_hard(margins::margins(.model,data=data))
   results        <-  try_hard(summary(m$obj,level=ciWidth,by_factor=FALSE))
   results$obj
   
}


..margins.clmm<-function(model,obj) {
  jinfo("EFFECTSIZE: margins not available for model of class", class(model))  
  
  return(NULL)
}


#### Relative risk ######


es.relativerisk<-function(obj) {
  
      model          <-  obj$model
      data           <-  insight::get_data(model,source="frame")
      ciWidth        <-  obj$ciwidth
      depobj         <-  obj$datamatic$variables[[tob64(obj$options$dep)]]
      levs           <-  levels(data[[depobj$name64]])

      
      data[,depobj$name64]  <-  as.numeric(data[[depobj$name64]]==levs[2])
      
      ## in previous versions the geepack::gee() poisson model was used
      ## now we use glm poisson(log) because with robust standard errors
      ## results are practically the same and it's faster (and we do not
      ## need to load geepack)
      
      results   <- stats::update(model,data=data,family = stats::poisson())
      params<-as.data.frame(parameters::parameters(results,
                                                   vcov=sandwich::vcovHC,
                                                   ci_method="wald",
                                                   exponentiate=TRUE,
                                                   effects="fixed"))

      if (!obj$option("ci_method","wald"))
          warning("Wald method for confidence intervals has been used")
      
      names(params)<-c("source","estimate","se","nothing", "est.ci.lower","est.ci.upper","test","df","p")
      
      return(params)

}      

es.glm_variances<-function(model,obj) {
  
    .anova         <-  car::Anova(model,type="III")
     atable        <-  as.data.frame(.anova[c(-1,-dim(.anova)[1]),])
     names(atable) <-  c("SS","df","test","p")
     df            <-  atable$df
     dfres         <-  model$df.residual
     sumr          <-  summary(model)
     N             <-  dfres+sumr$fstatistic[[2]]+1
     ssres         <-  stats::sigma(model)^2*dfres
     ssmod         <-  sumr$fstatistic[[1]]*sumr$fstatistic[[2]]*ssres/dfres
     SS            <-  df*atable$test*ssres/dfres
     
     es            <-  SS/(ssmod+ssres)
     etaSq         <-  ci_effectsize(es,df,dfres,obj,"eta")
     
     es            <- SS/(SS+ssres)
     etaSqP        <- ci_effectsize(es,df,dfres,obj,"etap")
     
     es            <- (SS-(ssres*df/dfres))/(ssmod+(ssres*(dfres+1)/dfres))
     omegaSq       <- ci_effectsize(es,df,dfres,obj,"omega")
     
     es            <- (SS-(ssres*df/dfres))/(SS+(ssres*(N-df)/dfres))
     omegaSqP      <- ci_effectsize(es,df,dfres,obj,"omegap")
     
     es            <- (SS-(ssres*df/dfres))/(ssmod+ssres)
     epsilonSq     <- ci_effectsize(es,df,dfres,obj,"epsilon")
     
     es            <- (SS-(ssres*df/dfres))/(SS+ssres)
     epsilonSqP    <- ci_effectsize(es,df,dfres,obj,"epsilonp")

     alist<-list()
     for (i in seq_along(etaSq$es)) {
       ladd(alist)  <-  list(estimate=etaSq[i,1],     est.ci.lower=etaSq[i,2],      est.ci.upper=etaSq[i,3])
       ladd(alist)  <-  list(estimate=etaSqP[i,1],    est.ci.lower=etaSqP[i,2],     est.ci.upper=etaSqP[i,3])
       ladd(alist)  <-  list(estimate=omegaSq[i,1],   est.ci.lower=omegaSq[i,2],    est.ci.upper=omegaSq[i,3])
       ladd(alist)  <-  list(estimate=omegaSqP[i,1],  est.ci.lower=omegaSqP[i,2],   est.ci.upper=omegaSqP[i,3])
       ladd(alist)  <-  list(estimate=epsilonSq[i,1], est.ci.lower=epsilonSq[i,2],  est.ci.upper=epsilonSq[i,3])
       ladd(alist)  <-  list(estimate=epsilonSqP[i,1],est.ci.lower=epsilonSqP[i,2], est.ci.upper=epsilonSqP[i,3])
      }
  
  return(alist)
}
  
  


### ES estimates ###
add_effect_size<- function(x,...) UseMethod(".add_es")

.add_es.default<-function(atable,model,variable) {
  return(atable)
  
}
  
.add_es.simple_params_lm<-function(atable,model,variable) {

  xstd<-1
  if (!is.factor(model$model[,variable])) xstd<-stats::sd(model$model[,variable])
  y<-names(attr(model$terms,"dataClass"))[1]
  ystd<-stats::sd(model$model[,y])
  atable$beta<-atable$estimate*(xstd/ystd) 
  atable
}

.add_es.simple_params_glm<-function(atable,model,variable=NULL) {

  atable$expb<-exp(atable$estimate)
  atable$expb.ci.lower<-exp(atable$est.ci.lower)
  atable$expb.ci.upper<-exp(atable$est.ci.upper)
  atable
}

.add_es.simple_params_glmer<-function(atable,model,variable=NULL) 
                     .add_es.simple_params_glm(atable,model,variable) 
  


.add_es.simple_anova_lm<-function(atable,model) {
  
  dfres<-model$df.residual
  sumr<-summary(model)
  N<-dfres+sumr$fstatistic[[2]]+1
  ssres<-stats::sigma(model)^2*dfres
  ssmod<-sumr$fstatistic[[1]]*sumr$fstatistic[[2]]*ssres/dfres
  df<-atable$df1
  SS<-df*atable$test*ssres/dfres
  atable$etaSq  <- SS/(ssmod+ssres)
  atable$etaSqP <- SS/(SS+ssres)
  atable$omegaSq <- (SS-(ssres*df/dfres))/(ssmod+(ssres*(dfres+1)/dfres))
  atable$omegaSqP <- (SS-(ssres*df/dfres))/(SS+(ssres*(N-df)/dfres))
  atable$epsilonSq<-(SS-(ssres*df/dfres))/(ssmod+ssres)
  atable$epsilonSqP<-(SS-(ssres*df/dfres))/(SS+ssres)
  as.data.frame(atable)  
}



# .add_es.summary_emm<-function(atable) {
#   
#   atable$etaSqP<-effectsize::F_to_eta2(atable$F.ratio,df=atable$df1,df_error = atable$df2)[,1]
#   atable$omegaSq<-effectsize::F_to_omega2(atable$F.ratio,df=atable$df1,df_error = atable$df2)[,1]
#   atable$epsilonSq<-effectsize::F_to_epsilon2(atable$F.ratio,df=atable$df1,df_error = atable$df2)[,1]
#   
#   
#   return(atable)
# }




### confidence intervals for effect size indices

ci_effectsize<-function(es,df,dfres,obj,what="any") {
  
  if (is.null(obj$boot_variances)) {
  
        fs<-.v_to_F(es,df,dfres)
        cilist<-lapply(seq_along(fs),function(i) {
                  res<- effectsize:::.get_ncp_F(fs[i],df[i],dfres,conf.level = obj$ciwidth)
                  res[is.na(res)]<-0
                  c(es[i],.F_to_v(res,df = df[i],dfres))
              })
        res<-as.data.frame(do.call(rbind,cilist))
        names(res)<-c("es","es.ci.lower","es.ci.upper")
        res
  } else {
    terms<-seq_along(es)  
    N<-df+dfres+1
    get_boot_ci(what,terms,obj$boot_variances,type=obj$options$ci_method,width=obj$ciwidth,df = df,dfres=dfres,N=N)
    
    }
}
.F_to_v<-function(f,df,dfres)  {(f*df) / (f*df + dfres)}

.v_to_F<-function(e,df,dfres) pmax(0, (e/df) / ((1-e)/dfres))



### bootstrap ####

es.var_boot_fun<-function(data,indices,model=NULL) {
  .data<-data[indices,]
  .model<-stats::update(model,data=.data)
  .anova<-car::Anova(.model,type="III",singular.ok=T)
   atable<-as.data.frame(.anova[!(rownames(.anova) %in% c("(Intercept)","Residuals")),])
   names(atable)<-c("ss","df","test","p")
   dfres<-model$df.residual
   sumr<-summary(model)
   ssres<-stats::sigma(model)^2*dfres
   ssmod<-sumr$fstatistic[[1]]*sumr$fstatistic[[2]]*ssres/dfres
   ss<-atable$ss
   unlist(c(ss,ssmod,ssres))
}

## computes bootstrap conf int for variances effect size indices 
get_boot_ci<-function(effsize,terms,bootresults,type,width,df,dfres,N) {

  type<-switch(type,
               quantile="perc",
               bcai="bca")  
  fun<-switch(effsize,
              eta=function(ss,ssmod,ssres,df,dfres,N) ss/(ssmod+ssres),
              etap=function(ss,ssmod,ssres,df,dfres,N) ss/(ss+ssres),
              omega=function(ss,ssmod,ssres,df,dfres,N) (ss-(ssres*df/dfres))/(ssmod+(ssres*(dfres+1)/dfres)),
              omegap=function(ss,ssmod,ssres,df,dfres,N) (ss-(ssres*df/dfres))/(ss+(ssres*(N-df)/dfres)),
              epsilon=function(ss,ssmod,ssres,df,dfres,N) (ss-(ssres*df/dfres))/(ssmod+ssres),
              epsilonp=function(ss,ssmod,ssres,df,dfres,N) (ss-(ssres*df/dfres))/(ss+ssres),
  )
  
  sterms<-seq_along(terms)
  l<-length(bootresults$t0)
  ss<-bootresults$t0[sterms]
  ssmod<-bootresults$t0[l-1]
  ssres<-bootresults$t0[l]
  N<-dim(bootresults$data)[1]
  es  <- fun(ss,ssmod,ssres,df,dfres,N)
  bootresults$t0[sterms]<-es
  
  for (i in 1:nrow(bootresults$t)) {
    
    ss<-bootresults$t[i,sterms]
    ssmod<-bootresults$t[i,l-1]
    ssres<-bootresults$t[i,l]
    es  <- fun(ss,ssmod,ssres,df,dfres,N)
    bootresults$t[i,sterms]<-es
    
  }
  alist<-lapply(sterms, function(i) {
    r<-boot::boot.ci(bootresults,type=type,conf = width,index=i)
    c(r$t0,r[[length(r)]][c(4:5)])
  })
  res<-as.data.frame(do.call(rbind,alist))
  names(res)<-c("es","est.ci.lower","est.ci.upper")
  res$type<-effsize
  res$effect<-terms
  res
}

