
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

es.glm_variances<-function(model,ciwidth) {
  
  .anova<-car::Anova(model,type=3)
  .anova<-.anova[!(rownames(.anova) %in% c("(Intercept)")),]
  anovatab<-.anova
  colnames(anovatab)<-c("ss","df","f","p")
  effss<-anovatab$ss[!(rownames(anovatab) %in% c("Residuals"))]
  errss<-anovatab$ss[rownames(anovatab)=="Residuals"]
  sumr<-summary(model)
  
  ### whole model ###
  f<-sumr$fstatistic[[1]]
  edf<-sumr$fstatistic[[3]]
  mdf<-sumr$fstatistic[[2]]
  modss<-f*errss*mdf/edf
  #####
  # Here we need a correct to the computation of the effect sizes. To compute the non-partial indices
  ## effectsize:: package uses as total sum of squares the sum of the effects SS (plus residuals)
  ## In unbalanced designs, the sum does not necessarily correspond to the model SS (plus residuals)
  ## so the estimation is biased. Eta-squared does not correspond to semi-partial r^2 any more
  ## and many properties of the non-partial indices are broken. 
  ## Thus, we fixed it by adding a bogus effect whose SS is exactly the discrepancy between
  ## the table SS and the model+error SS. In this way, the estimation uses the correct total SS
  #####
  diff<-modss-sum(effss)
  add<-data.frame(diff,1,1,0)
  names(add)<-names(.anova)
  .canova<-rbind(.anova,add)
  last<-length(effss)[1]+1
  eta<-effectsize::eta_squared(.canova,partial = F,ci=ciwidth,verbose=F)
  etap<-effectsize::eta_squared(.anova,partial = T,ci=ciwidth,verbose=F)
  omega<-  effectsize::omega_squared(.canova,partial = F,ci=ciwidth,verbose=F)
  omegap<-  effectsize::omega_squared(.anova,partial = T,ci=ciwidth,verbose=F)
  epsilon<-  effectsize::epsilon_squared(.canova,partial = F,ci=ciwidth,verbose=F)
  epsilonp<-  effectsize::epsilon_squared(.anova,partial = T,ci=ciwidth,verbose=F)
  alist<-list()
  for (i in seq_along(etap$Parameter)) {
    alist[[length(alist)+1]]<-list(estimate=eta[i,2],est.ci.lower=eta[i,4],est.ci.upper=eta[i,5])
    alist[[length(alist)+1]]<-list(estimate=etap[i,2],est.ci.lower=etap[i,4],est.ci.upper=etap[i,5])
    alist[[length(alist)+1]]<-list(estimate=omega[i,2],est.ci.lower=omega[i,4],est.ci.upper=omega[i,5])
    alist[[length(alist)+1]]<-list(estimate=omegap[i,2],est.ci.lower=omegap[i,4],est.ci.upper=omegap[i,5])
    alist[[length(alist)+1]]<-list(estimate=epsilon[i,2],est.ci.lower=epsilon[i,4],est.ci.upper=epsilon[i,5])
    alist[[length(alist)+1]]<-list(epsilonp[i,1],estimate=epsilonp[i,2],est.ci.lower=epsilonp[i,4],est.ci.upper=epsilonp[i,5])
    
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


