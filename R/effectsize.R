
es.relativerisk<-function(obj) {

      model          <-  obj$model
      data           <-  model$data
      data$id_id_id  <- seq_len(dim(data)[1])
      depobj         <- obj$datamatic$variables[[tob64(obj$options$dep)]]
      levs           <- depobj$levels
      ciWidth        <- obj$ciwidth
      
      data[,depobj$name64]  <-  as.numeric(data[[depobj$name64]]==levs[2])

      results<-geepack::geeglm(as.formula(obj$formula64),
                                        family = poisson(link = "log"),
                                        id = id_id_id, 
                                        corstr = "exchangeable", data = data)
      
      params<-as.data.frame(parameters::parameters(results$obj,exponentiate=TRUE))
      names(params)<-c("source","estimate","se","nothing", "ci.lower","ci.upper","test","df","p")
      
      if (params$source[1]=="(Intercept)")
             params <- params[-1,] 
      
    return(params)

}      

es.glm_variances<-function(model,obj) {
  
  .anova<-car::Anova(model,type="III")
  atable<-as.data.frame(.anova[c(-1,-dim(.anova)[1]),])
  names(atable)<-c("SS","df","test","p")
  df<-atable$df
  dfres<-model$df.residual
  sumr<-summary(model)
  N<-dfres+sumr$fstatistic[[2]]+1
  ssres<-sigma(model)^2*dfres
  ssmod<-sumr$fstatistic[[1]]*sumr$fstatistic[[2]]*ssres/dfres
  SS<-df*atable$test*ssres/dfres
  N<-dfres+sumr$fstatistic[[2]]+1
  ssres<-sigma(model)^2*dfres
  ssmod<-sumr$fstatistic[[1]]*sumr$fstatistic[[2]]*ssres/dfres
  SS<-df*atable$test*ssres/dfres
  es  <- SS/(ssmod+ssres)
  etaSq<-ci_effectsize(es,df,dfres,obj,"eta")
  es <- SS/(SS+ssres)
  etaSqP<-ci_effectsize(es,df,dfres,obj,"etap")
  es <- (SS-(ssres*df/dfres))/(ssmod+(ssres*(dfres+1)/dfres))
  omegaSq <- ci_effectsize(es,df,dfres,obj,"omega")
  es <- (SS-(ssres*df/dfres))/(SS+(ssres*(N-df)/dfres))
  omegaSqP<-ci_effectsize(es,df,dfres,obj,"omegap")
  es<-(SS-(ssres*df/dfres))/(ssmod+ssres)
  epsilonSq<-ci_effectsize(es,df,dfres,obj,"epsilon")
  es<-(SS-(ssres*df/dfres))/(SS+ssres)
  epsilonSqP<-ci_effectsize(es,df,dfres,obj,"epsilonp")

  alist<-list()
  for (i in seq_along(etaSq$es)) {
    alist[[length(alist)+1]]<-list(estimate=etaSq[i,1],     est.ci.lower=etaSq[i,2],      est.ci.upper=etaSq[i,3])
    alist[[length(alist)+1]]<-list(estimate=etaSqP[i,1],    est.ci.lower=etaSqP[i,2],     est.ci.upper=etaSqP[i,3])
    alist[[length(alist)+1]]<-list(estimate=omegaSq[i,1],   est.ci.lower=omegaSq[i,2],    est.ci.upper=omegaSq[i,3])
    alist[[length(alist)+1]]<-list(estimate=omegaSqP[i,1],  est.ci.lower=omegaSqP[i,2],   est.ci.upper=omegaSqP[i,3])
    alist[[length(alist)+1]]<-list(estimate=epsilonSq[i,1], est.ci.lower=epsilonSq[i,2],  est.ci.upper=epsilonSq[i,3])
    alist[[length(alist)+1]]<-list(estimate=epsilonSqP[i,1],est.ci.lower=epsilonSqP[i,2], est.ci.upper=epsilonSqP[i,3])
  }
  
  return(alist)
}
  
  
mf.fixTable<- function(x,...) UseMethod(".fixtable")

.fixtable.default<-function(atable) {
  return(atable)
}



### beta in parameter estimates ###
add_effect_size<- function(x,...) UseMethod(".add_es")

.add_es.simple_params_lm<-function(atable,model,variable) {
  
  xstd<-1
  if (!is.factor(model$model[,variable])) xstd<-sd(model$model[,variable])
  y<-names(attr(model$terms,"dataClass"))[1]
  ystd<-sd(model$model[,y])
  atable$beta<-atable$estimate*(xstd/ystd) 
  atable
}

.add_es.simple_anova_lm<-function(atable,model) {
  
  dfres<-model$df.residual
  sumr<-summary(model)
  N<-dfres+sumr$fstatistic[[2]]+1
  ssres<-sigma(model)^2*dfres
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
    get_boot_ci(what,terms,obj$boot_variances,type=obj$options$cimethod,width=obj$ciwidth,df = df,dfres=dfres,N=N)
    
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
   ssres<-sigma(model)^2*dfres
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

