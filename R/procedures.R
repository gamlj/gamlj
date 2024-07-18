## Here are procedures, such as simple effects or posthoc tests, that may
## be different from model to model (so they dispatch a S3 call), and 
## produce one or more tables. 


######  ##########
procedure.beta<- function(x,...) UseMethod(".beta")

.beta.default<-function(model) {
  
  data<-insight::get_data(model,source="frame")
  for (var in names(data)) {
    if (!is.factor(data[[var]]))
        data[[var]]<-as.numeric(scale(data[[var]]))
  }
  z<-stats::update(model,data)
  parameters::parameters(model)$Coefficient
}



############# post hoc ##############

procedure.posthoc <- function(obj) {
  
  
  jinfo("PROCEDURE: Posthoc")
  
  terms <- obj$options$posthoc
  dep <- tob64(obj$options$dep)
  

  ### we set the model, if we need bootstrap ci, we switch to the bootstrapped model
  ### later on
  
   model<-obj$model

  ### check if we need robust standard error  
  vfun<-NULL
  if (obj$option("se_method","robust")) {
       type<-obj$options$robust_method
       vfun<-function(x,...) sandwich::vcovHC(x,type=type,...)
  }
  
  lmer.df = NULL
  if (obj$option("df_method",c("Satterthwaite","Kenward-Roger"))) {
    lmer.df=obj$options$df_method
  }
  mode = NULL

  postHocTables <- list()
  for (.vars in terms) {

    ## emmeans list comparison levels with a strange order. So we pass the inverted order of the term
    ## so the final table will look sorted in a more sensible way
    .revvars<-rev(.vars)
    .term64<-jmvcore::composeTerm(tob64(.revvars))
     
     referenceGrid <- .posthoc(model, .term64, vfun=vfun,df=lmer.df,mode=mode)

     if (obj$option("model_type","multinomial"))
            .pairs<-graphics::pairs(referenceGrid,by=dep)
     else
            .pairs<-graphics::pairs(referenceGrid)
     none <- summary(.pairs, adjust = "none",infer = c(FALSE,TRUE))
     tableData <- as.data.frame(none, stringAsFactors = FALSE)

     for (adj in obj$options$adjust) {
       arow <- summary(.pairs, adjust = adj,infer = c(FALSE,TRUE))
       tableData[[adj]]<- arow$p.value
     }
     
     .transnames<-list(estimate=c("odds.ratio","ratio"),
                       test=c("z.ratio","t.ratio"),
                       p="p.value",
                       se="SE",
                       response=tob64(dep)
     )
     names(tableData)<-transnames(names(tableData),.transnames)  
     ## confidence intervals
     .model<-model
     if (obj$option("ci_method",c("quantile","bcai"))) 
       .model<-obj$boot_model

     cidata<-try_hard(.posthoc_ci(.model,.term64,obj$ciwidth,obj$options$ci_method,vfun=vfun))
     tableData$est.ci.lower<-cidata$obj$est.ci.lower       
     tableData$est.ci.upper<-cidata$obj$est.ci.upper       
     ## we create one column for each contrast level
     tableData$contrast <- as.character(tableData$contrast)

    .cont <- tableData$contrast
    .cont <- gsub("[-,/]","", .cont)

    .labs <- lapply(.cont, function(a) {
      lapply(strsplit(as.character(a), LEVEL_SYMBOL)[[1]], trimws)[-1]
    })

    labs <- do.call("rbind", .labs)

    .vars  <- make.names(.revvars,unique = T)
    .names <- c(paste0(.vars,"_lev1"),paste0(.vars,"_lev2"))
     # emmeans inverts the order for clm, so we should fix it
    
     if (obj$option("model_type","ordinal"))
       .names <- c(paste0(.vars,"_lev2"),paste0(.vars,"_lev1"))
    
      
    
    colnames(labs) <- .names
    tableData <- cbind(labs, tableData)

    
    for (.name in .names)
      tableData[,.name]<-as.character(obj$datamatic$get_params_labels(tableData[,.name]))
    
     
    if (dep %in% names(tableData))
        tableData$response<-fromb64(as.character(tableData[[dep]]))

    if (tableData$df[1]==Inf) attr(tableData,"titles")<-list(test="z")
    
    postHocTables[[length(postHocTables)+1]]<-tableData
  }
    jinfo("PROCEDURE: done")
  

    postHocTables
  
}

procedure.posthoc_effsize <- function(obj) {


  terms <- obj$options$posthoc
  dep <- obj$options$dep
  ### at the moment (version 3.0.0), no bootstrap for d indeces
  model<-obj$model
  
  ### check if we need robust standard error  
  vfun<-NULL
  if (obj$option("se_method","robust")) {
    type<-obj$options$robust_method
    vfun<-function(x,...) sandwich::vcovHC(x,type=type,...)
  }
  
  
  if (length(terms) == 0) 
    return()
  
  dTables <- list()
  
  for (.vars in terms) {
    
   .revvars<-rev(.vars)
    ## emmeans list comparison levels with a strange order. So we pass the inverted order of the term
    ## so the final table will look sorted in a more sensible way
   .term64 <- jmvcore::composeTerm(tob64(.revvars))
    
    referenceGrid <- .posthoc(model, .term64, vfun=vfun)
    none <- summary(graphics::pairs(referenceGrid), adjust = "none",infer = c(FALSE,TRUE))
    
    tableData <- as.data.frame(none, stringAsFactors = FALSE)

    .transnames<-list(estimate=c("odds.ratio","ratio"),
                      test=c("z.ratio","t.ratio"),
                      p="p.value",
                      se="SE"
    )
    names(tableData)<-transnames(names(tableData),.transnames)    
    
    tableData$contrast <- as.character(tableData$contrast)
    
    .cont <- as.character(tableData$contrast)
    .cont <- gsub(" - ", "-", .cont, fixed = T)
    .cont <- gsub(" / ", "/", .cont, fixed = T)
    
    .labs <- sapply(.cont, function(a) {
      sapply(strsplit(as.character(a), "[- ,/]"), trimws, USE.NAMES = F, simplify = F)
    })
     labs <- do.call("rbind", .labs)
     .vars  <- make.names(.revvars,unique = T)
     .names <- c(paste0(.vars,"_lev1"),paste0(.vars,"_lev2"))
      colnames(labs) <- .names
    
      tableData <- cbind(labs, tableData)
      rownames(tableData)<-NULL

      for (.name in .names)
         tableData[,.name]<-as.character(obj$datamatic$get_params_labels(tableData[,.name]))

      d<-effectsize::t_to_d(tableData$test,df_error = obj$model$df.residual,ci = obj$ciwidth)
      tableData$ds<-d$d
      tableData$ds.ci.lower<-d$CI_low
      tableData$ds.ci.upper<-d$CI_high
    
      df<-tableData$df
      J <- exp(lgamma(df / 2) - log(sqrt(df / 2)) - lgamma((df - 1) / 2)) # see effectsize package
      tableData$g<-d$d * J
      tableData$g.ci.lower<-d$CI_low * J
      tableData$g.ci.upper<-d$CI_high * J
    
    ## here we use the model sigma as the denominator. This is the approach used in
    ## emmeans::eff_size default. 
    
    t<-sqrt(df)*tableData$estimate/(2*stats::sigma(model))
    d<-effectsize::t_to_d(t,df_error = df,ci = obj$ciwidth)
    tableData$dm<-d$d
    tableData$dm.ci.lower<-d$CI_low
    tableData$dm.ci.upper<-d$CI_high
    dTables[[length(dTables)+1]]<-tableData
  }
  
  dTables

}



###### post hoc ##########
.posthoc <- function(x, ...) UseMethod(".posthoc")

.posthoc.default <- function(model, term, vfun=NULL,df=NULL,mode=NULL) {

    termf <- stats::as.formula(paste("~", term))
    
    data <- insight::get_data(model,source="frame")
   
    opts_list<-list(object=model,specs=termf,adjust="none", type = "response", data = data)

    if (is.something(mode)) 
         opts_list[["mode"]]<-mode
    
    if (is.something(vfun))
         opts_list[["vcov."]]<-vfun    

    if (is.something(df))
      opts_list[["lmer.df"]]<-tolower(df)    
    
      referenceGrid <- do.call(emmeans::emmeans,opts_list)
      terms <- jmvcore::decomposeTerm(term)
      labs <- referenceGrid@grid[terms]
      newlabs <- sapply(labs, function(a) sapply(a, function(b) as.character(b)))
      referenceGrid@grid[terms] <- newlabs
      return(referenceGrid)
}

.posthoc.clm <- function(model, term, vfun=NULL,df=NULL,mode=NULL) {

  .posthoc.default(model,term,vfun=vfun,df=df,mode="linear.predictor")
  
}

.posthoc.clmm <- function(model, term, vfun=NULL,df=NULL,mode=NULL) {
  
  .posthoc.default(model,term,vfun=vfun,df=df,mode="linear.predictor")
  
}

.posthoc.multinom <- function(model, term, vfun=NULL,df=NULL, mode=NULL) {

  if (inherits(model,"bootstrap_model") )
       model<-attr(model,"original_model")

    dep <- names(attr(stats::terms(model), "dataClass"))[1]
    dep <- jmvcore::composeTerm(dep)
    termf <- stats::as.formula(paste("~", paste(dep, term, sep = "|")))
    data <- insight::get_data(model,source="frame")


    referenceGrid <- emmeans::emmeans(model, termf, data = data)
    terms <- jmvcore::decomposeTerm(term)
    labs <- referenceGrid@grid[terms]
    newlabs <- sapply(labs, function(a) sapply(a, function(b) as.character(b)))
    referenceGrid@grid[terms] <- newlabs
    
  return(referenceGrid)
}

.posthoc.mmblogit <- function(model, term, adjust,ci=FALSE,vfun=NULL,df=NULL,mode=NULL) {
  
  dep <- names(attr(stats::terms(model), "dataClass"))[1]
  dep <- jmvcore::composeTerm(dep)
  termf <- stats::as.formula(paste("~", paste(dep, term, sep = "|")))
  data <- model$data
  referenceGrid <- emmeans::emmeans(model, termf, data = as.data.frame(data))
  terms <- jmvcore::decomposeTerm(term)
  labs <- referenceGrid@grid[terms]
  newlabs <- sapply(labs, function(a) sapply(a, function(b) as.character(b)))
  referenceGrid@grid[terms] <- newlabs
  
  return(referenceGrid)
}


.posthoc_ci <- function(x, ...) UseMethod(".posthoc_ci")

.posthoc_ci.default=function(model,term,width,method,vfun=NULL,mode=NULL) {
  
  termf <- stats::as.formula(paste("pairwise ~", term))

  data <- insight::get_data(model,source="frame")
  
  opts_list<-list(object=model,specs=termf, type = "response",data=data)
  
  if (is.something(mode))
     opts_list[["mode"]]<-mode
  
  if (is.something(vfun))
    opts_list[["vcov."]]<-vfun   
  
  referenceGrid <- do.call(emmeans::emmeans,opts_list)
  results<-as.data.frame(parameters::parameters(referenceGrid$contrasts,ci=width,ci_method=method))
  results<-as.data.frame(cbind(results$CI_low,results$CI_high))
  names(results)<-c("est.ci.lower","est.ci.upper")
  results
  
}

.posthoc_ci.clm=function(model,term,width,method,vfun=NULL,mode=NULL) {
  
  .posthoc_ci.default(model,term,width,method,vfun=vfun,mode="linear.predictor")
    
}

.posthoc_ci.clmm=function(model,term,width,method,vfun=NULL,mode=NULL) {
    
    .posthoc_ci.default(model,term,width,method,vfun=vfun,mode="linear.predictor") 
      
    }
    
.posthoc_ci.multinom=function(model,term,width,method,vfun=NULL, mode=NULL) {

  if (inherits(model,"bootstrap_model") )
    model<-attr(model,"original_model")

    dep <- names(attr(stats::terms(model), "dataClass"))[1]
    dep <- jmvcore::composeTerm(dep)
    tterm <- stats::as.formula(paste("~", paste(dep, term, sep = "|")))
    data <- insight::get_data(model,source="frame")
    
      referenceGrid <- emmeans::emmeans(model, tterm, data = data)
      
      terms <- jmvcore::decomposeTerm(term)
      labs <- referenceGrid@grid[terms]
      newlabs <- sapply(labs, function(a) sapply(a, function(b) tob64(as.character(b))))
      referenceGrid@grid[terms] <- newlabs
      results <- as.data.frame(summary(graphics::pairs(referenceGrid, by=dep),infer = c(TRUE,FALSE)))
      results<-as.data.frame(cbind(results$lower.CL,results$upper.CL))
      names(results)<-c("est.ci.lower","est.ci.upper")
      results
return(results)
  
}

######## end post hoc #############

### Estimated Marginal Means ####

procedure.emmeans<-function(obj) {
  
  jinfo("PROCEDURE: Estimated Marginal Means")
  terms<-obj$options$emmeans
  
  type<-"response"
  results<-list()
  for (term in terms) {
    
    ### we need to reverse the term because emmeans order levels in a strange way
    term64<-tob64(rev(term))
    
    ## include the dependent for multinomial ##
    if (obj$options$model_type=="multinomial")
        term64<-c(obj$datamatic$dep$name64,term64)
    
    ## first we get the levels of covs at which to condition the estimation ##
    ## we also get the labels for those values ####
    conditions<-list()
    for (.term in term64) {
      if (! .term %in% names(obj$datamatic$variables))
            stop("Variable ",fromb64(term)," is not in the model")
      
      var<-obj$datamatic$variables[[.term]]
      if( var$type=="numeric") {
        conditions[[.term]]<-var$levels 
      }
    }
    ### prepare the options ###
    data <- insight::get_data(obj$model,source="frame")
    
    opts_list<-list(object=obj$model,
                    specs=term64,
                    at=conditions,
                    type=type,
                    nesting = NULL,
                    options = c(level=obj$ciwidth),
                    data=data
    )
    if (obj$option("df_method"))
       opts_list[["lmer.df"]]<-tolower(obj$options$df_method)
    
    if (obj$option("se_method","robust")) {
      type <- obj$options$robust_method
      opts_list[["vcov."]]<-function(x,...) sandwich::vcovHC(x,type=type,...)
     }
    if (obj$option("model_type","ordinal"))
      opts_list[["mode"]]<-"mean.class"

    ### now we get the estimated means #######
    referenceGrid<-do.call(emmeans::emmeans,opts_list)
    tableData<-as.data.frame(referenceGrid)
    ### rename the columns ####
    names(tableData)<-c(term64,"estimate","se","df","est.ci.lower","est.ci.upper")


    if (obj$options$ci_method %in% c("quantile","bcai")) {

      referenceGrid@post.beta<-as.matrix(obj$boot_model)
      cidata<-as.data.frame(parameters::parameters(referenceGrid,ci_method=obj$options$ci_method,ci=obj$ciwidth))
      tableData$est.ci.lower<-cidata$CI_low
      tableData$est.ci.upper<-cidata$CI_high
    }
   
    ### fix the labels  ###

    for (.name in term64) {
      tableData[[.name]]<-factor(tableData[[.name]])
      levels(tableData[[.name]])<-obj$datamatic$variables[[.name]]$levels_labels
      tableData[[.name]]<-as.character(tableData[[.name]])
    }

    ## rename the dependent for multinomial ##
    if (obj$options$model_type=="multinomial"){
         names(tableData)[1]<-"response"
         tableData<-tableData[order(tableData$response),]
    }

    names(tableData)<-fromb64(names(tableData))
    results[[length(results)+1]]<-tableData
  }
  jinfo("PROCEDURE: done")
  results
}  
  

procedure.simpleEffects<- function(x,...) UseMethod(".simpleEffects")


.simpleEffects.default<-function(model,obj) {

  jinfo("PROCEDURE: Simple Effects estimated")
  variable<-obj$options$simple_x
  variable64<-tob64(variable)
  varobj<-obj$datamatic$variables[[variable64]]
  term<-obj$options$simple_mods

  term64<-tob64(term)
  results<-list()
  vars<-c("contrast",term64)
  

    ## first we get the levels of covs at which to condition the estimation ##
    ## we also get the labels for those values ####
    conditions<-list()
    labels<-list()
    for (.term in term64) {
      var<-obj$datamatic$variables[[.term]]
      if( var$type=="numeric") {
        conditions[[.term]]<-var$levels 
        labels[[.term]]<-var$levels_labels 
      }
    }
    .get_se<-function(alist) {
    ### now we get the estimated means #######
    ### to handle bootstrap, we need to estimate the effects without CI
    ### and then add the CI with a specific method declared in options$ci_method


    if (varobj$type=="factor") {
      alist[["specs"]]=c(variable64,term64)
      ### in 2021 with custom contrast function (not string), infer=c() does not work ####
      referenceGrid<-do.call(emmeans::emmeans,alist)
      grid<-emmeans::contrast(referenceGrid,
                                   by=term64,
                                   method =.local.emmc,
                                   datamatic=varobj
                                   )
    } else {
      alist[["specs"]] <-term64
      alist[["var"]]   <-variable64
      grid <- do.call(emmeans::emtrends, alist)
    }
    grid
    }
    ##
    data <- insight::get_data(model,source="frame")
    opts_list<-list(object=model,
                    at=conditions,
                    nesting = NULL,
                    infer=c(TRUE,TRUE),
                    estName="estimate",
                    data=data
    )
    
    if (obj$option("df_method"))
      opts_list[["lmer.df"]]<-tolower(obj$options$df_method)
    
    if (obj$option("se_method","robust")) {
      type<-obj$options$robust_method
      opts_list[["vcov."]]<-function(x,...) sandwich::vcovHC(x,type=type,...)
      obj$warning<-list(topic="simpleEffects_anova",message=WARNS[["stde.robust_test"]])
      obj$warning<-list(topic="simpleEffects_coefficients",message=WARNS[["stde.robust_test"]])
      
    }
    
    grid<-.get_se(opts_list)
    estimates<-as.data.frame(grid)

    ## deal with  CI
    .transnames<-list("ci.lower"=c("asymp.LCL","CI_low","lower.CL"),
                     "ci.upper"=c("asymp.UCL","CI_high","upper.CL"))
    
    if (!obj$option("ci_method","wald")) {
      
      opts_list$object<-obj$boot_model
      cidata<-.get_se(opts_list)
      cidata<-as.data.frame(parameters::parameters(cidata,ci_method=obj$options$ci_method))
      
    } else {
    
      cidata<-stats::confint(grid,level = obj$ciwidth)    
    }
    names(cidata)<-transnames(names(cidata),.transnames)
    estimates$est.ci.lower<-cidata$ci.lower
    estimates$est.ci.upper<-cidata$ci.upper

    .transnames<-list(test=c("z.ratio", "t.ratio"),p="p.value",se="SE")
    names(estimates)<-transnames(names(estimates),.transnames)    
    if (!("contrast" %in% names(estimates)))
          estimates$contrast<-variable
    
    
    for (.name in term64) {
            estimates[[.name]]<-factor(estimates[[.name]])
            levels(estimates[[.name]])<-obj$datamatic$variables[[.name]]$levels_labels
            estimates[[.name]]<-as.character(estimates[[.name]])
    }
    
    estimates$contrast<-as.character(estimates$contrast)
    ## fix the names of the moderators columns
    .params<-add_effect_size(estimates,model,variable64)
    names(.params)[names(.params) %in% term64]<-paste0("mod_",make.names(fromb64(term64),unique = T))

    ## add effect sizes
    class(.params)<-c(paste0("simple_params_",obj$options$.caller),class(.params))
    .params<-add_effect_size(.params,model,variable64)

    ### now we build the anova table ###
    .anova<-as.data.frame(emmeans::test(grid, join=TRUE, by = term64))
    .transnames<-list(test=c("F.ratio"),p="p.value")
    names(.anova)<-transnames(names(.anova),.transnames)    

    ### fix labels and make sure they are not factors or stuff    
    for (.name in term64) {
      .anova[[.name]]<-factor(.anova[[.name]])
      levels(.anova[[.name]])<-obj$datamatic$variables[[.name]]$levels_labels
      .anova[[.name]]<-as.character(.anova[[.name]])
    }
    #### fix names of moderators columns ####
    .names<-paste0("mod_",make.names(term,unique = T))
    names(.anova)[1:length(.names)]<-.names
    
    ### make fix depending of the type of model ###    
    class(.anova)<-c(paste0("simple_anova_",obj$options$model_type),class(.anova))
    .anova<-add_effect_size(.anova,model)
    ### check some stuff 
    .all <- c(term64,variable64)
    test <- unlist(sapply(.all,function(x) !(.is.scaleDependent(obj$model,x))))
    .issues <- .all[test]

    if (is.something(.issues))
      obj$warning<-list(topic="simpleEffects_anova",message=paste("Variable",paste(fromb64(.issues),collapse = ","),"is included in the simple effects analysis but it does not appear in any interaction"))

    if (any(.anova$df2==Inf)) {
        attr(.anova,"titles")<-list(test=letter_chi2,df1="df")
        attr(.params,"titles")<-list(test="z")
        
    }
    ## tell users what custom implies
    if (varobj$requireFocus()) {
     custom<-varobj$contrast_labels[[1]]
     obj$warning<-list(topic="simpleEffects_coefficients",message="Simple effects are estimated only for the custom constrast.")
     obj$warning<-list(topic="simpleEffects_coefficients",message=paste("Estimates evaluate the contrast",custom," at different levels of",paste(term,collapse=" and ")))
     obj$warning<-list(topic="simpleEffects_anova",message=paste("Estimates evaluate the contrast",custom," at different levels of",paste(term,collapse=" and ")))
    }
  jinfo("PROCEDURE: done")    
  return(list(.anova,.params))

}  

.local.emmc<-function(levs, datamatic=NULL) {

  # get the contrast weights
  codes <- datamatic$contrast_values
  if (datamatic$method=="dummy")
        codes<-datamatic$contrast_codes("simple")


  # transform the model matrix into the contrast matrix
  M <- as.data.frame(.ginv(t(codes)))
  # set some labels
  names(M)<-datamatic$contrast_labels

  if (datamatic$requireFocus()) {
     M<-data.frame(M[,1])
     names(M)<-datamatic$contrast_labels[1]
  }
  M
}




.simpleEffects.multinom<-function(model,obj) {


        levels <-lapply(obj$options$simple_mods, function(x) {
    
                if (obj$datamatic$variables[[tob64(x)]]$type=="factor")
                        seq_along(obj$datamatic$variables[[tob64(x)]]$levels)
                else 
                        obj$datamatic$variables[[tob64(x)]]$levels
            })
  
  
        vars  <- lapply(obj$options$simple_mods, function(x) obj$datamatic$variables[[tob64(x)]])
  
        rows  <- expand.grid(levels)
        names(rows)  <-  tob64(obj$options$simple_mods)
        variable64   <-  tob64(obj$options$simple_x)
        varobj       <-  obj$datamatic$variables[[variable64]]
  
        .names       <-  names(rows)
         parameters  <-  data.frame()
         anovas      <-  data.frame()
         
         data64      <-  insight::get_data(model,source="frame")
         
         ## here we do the actual simple model for each combination of moderator levels ####
         for (i in 1:nrow(rows)) {
                .data1<-data64
                 for (.name in .names) {
                        if (is.factor(.data1[[.name]]))
                               stats::contrasts(.data1[[.name]])<-stats::contr.treatment(nlevels(.data1[[.name]]),base = rows[i,.name])
                        else
                               .data1[[.name]]<-.data1[[.name]]-rows[i,.name]
                 }
                 .model  <-  stats::update(model,data=.data1)
                 params  <-  as.data.frame(parameters::parameters(.model))
                 params  <-  params[params$Parameter %in% varobj$paramsnames64,]
                 params[,.names]  <-  rows[i,]      
                 parameters  <-  rbind(parameters,params)
                 oneanova <- car::Anova(.model,test="Wald",type=3,singular.ok=T)
                 oneanova <- oneanova[rownames(oneanova) %in% varobj$name64,]
                 oneanova[,.names]  <-  rows[i,]         
                 anovas   <- rbind(anovas,oneanova)
    
         }
         names(parameters)[1:9]<-c("contrast","estimate","se","nothing","est.ci.lower","est.ci.upper","test","df","p")
         
         ## add exp b
         
         parameters$expb<-exp(parameters$estimate)
         parameters$expb.ci.lower<-exp(parameters$est.ci.lower)
         parameters$expb.ci.upper<-exp(parameters$est.ci.upper)

         
         for (.name in .names) {
           parameters[[.name]]<-factor(parameters[[.name]])
           levels(parameters[[.name]])<-obj$datamatic$variables[[.name]]$levels_labels
           parameters[[.name]]<-as.character(parameters[[.name]])
         }
         ### fix labels for  the response contrasts
         parameters$response<-factor(parameters$Response)
         levels(parameters$response)<-unlist(obj$datamatic$dep$contrast_labels)
         parameters$response<-as.character(parameters$response)
         parameters<-parameters[order(parameters$response),]
         
         ### fix labels for the contrast column ###
         parameters$contrast<-factor(parameters$contrast)
         levels(parameters$contrast)<-unlist(varobj$contrast_labels)
         parameters$contrast<-as.character(parameters$contrast)

         ## fix names for moderators and tests
         names(parameters)[names(parameters) %in% .names]<-paste0("mod_",make.names(fromb64(.names),unique = T))
         .transname<-list(test=c("F.ratio","LR Chisq"),df1=c("Df"),p=c("Pr(>Chisq)"))
         names(anovas)<-transnames(names(anovas),.transname)
         
         ### fix anovas
         for (.name in .names) {
           anovas[[.name]]<-factor(anovas[[.name]])
           levels(anovas[[.name]])<-obj$datamatic$variables[[.name]]$levels_labels
           anovas[[.name]]<-as.character(anovas[[.name]])
         }
         names(anovas)[names(anovas) %in% .names]<-paste0("mod_",make.names(fromb64(.names),unique = T))
         
         ### check some stuff 
         .all <- c(.names,variable64)
         test <- unlist(sapply(.all,function(x) !(.is.scaleDependent(obj$model,x))))
         .issues <- .all[test]
         
         if (is.something(.issues))
           obj$warning<-list(topic="simpleEffects_anova",message=paste("Variable",paste(fromb64(.issues),collapse = ","),"is included in the simple effects analysis but it does not appear in any interaction"))
         
         return(list(anovas,parameters))
}

.simpleEffects.mmblogit<-function(model,obj) {
  
  levels <-lapply(obj$options$simple_mods, function(x) {
    
    if (obj$datamatic$variables[[tob64(x)]]$type=="factor")
      seq_along(obj$datamatic$variables[[tob64(x)]]$levels)
    else 
      obj$datamatic$variables[[tob64(x)]]$levels
  })
  
  
  vars  <- lapply(obj$options$simple_mods, function(x) obj$datamatic$variables[[tob64(x)]])
  
  rows  <- expand.grid(levels)
  names(rows)  <-  tob64(obj$options$simple_mods)
  variable64   <-  tob64(obj$options$simple_x)
  varobj       <-  obj$datamatic$variables[[variable64]]
  
  .names       <-  names(rows)
  parameters  <-  data.frame()

  data64      <-  model$data
  
  ## here we do the actual simple model for each combination of moderator levels ####
  for (i in 1:nrow(rows)) {

    .data1<-data64
    for (.name in .names) {
      if (is.factor(.data1[[.name]])) {
        stats::contrasts(.data1[[.name]])<-stats::contr.treatment(nlevels(.data1[[.name]]),base = rows[i,.name])
      }
      else
        .data1[[.name]]<-.data1[[.name]]-rows[i,.name]
    }
    .model  <-  stats::update(model,data=.data1)
     params<-gparameters(.model,obj)
     params  <-  params[params$source %in% varobj$paramsnames64,]
     params[,.names]  <-  rows[i,]      
     parameters  <-  rbind(parameters,params)
  }
  
  ## add exp b
   parameters$expb<-exp(parameters$estimate)
   parameters$expb.ci.lower<-exp(parameters$est.ci.lower)
   parameters$expb.ci.upper<-exp(parameters$est.ci.upper)

  for (.name in .names) {
    parameters[[.name]]<-factor(parameters[[.name]])
    levels(parameters[[.name]])<-obj$datamatic$variables[[.name]]$levels_labels
    parameters[[.name]]<-as.character(parameters[[.name]])
  }
  ### fix labels for  the response contrasts
  parameters$response<-factor(parameters$response)
  levels(parameters$response)<-unlist(obj$datamatic$dep$contrast_labels)
  parameters$response<-as.character(parameters$response)
  parameters<-parameters[order(parameters$response),]
  ### fix labels for the contrast column ###
  parameters$contrast<-factor(parameters$source)
  levels(parameters$contrast)<-unlist(varobj$contrast_labels)
  parameters$contrast<-as.character(parameters$contrast)
  
  ## fix names for moderators and tests
  names(parameters)[names(parameters) %in% .names]<-paste0("mod_",make.names(fromb64(.names),unique = T))
  

  ### check some stuff 
  .all <- c(.names,variable64)
  test <- unlist(sapply(.all,function(x) !(.is.scaleDependent(obj$model,x))))
  .issues <- .all[test]
  
  if (is.something(.issues))
    obj$warning<-list(topic="simpleEffects_coefficients",message=paste("Variable",paste(fromb64(.issues),collapse = ","),"is included in the simple effects analysis but it does not appear in any interaction"))
  
  return(list(NULL,parameters))
}




## this is humbly supercool ###
## we want the simple interactions nested in the highest interaction required
procedure.simpleInteractions<-function(obj) {
  
     jinfo("PROCEDURE: simple Interactions")
  
      variable<-obj$options$simple_x
      variable64<-tob64(variable)
      term<-obj$options$simple_mods
      term64<-tob64(term)
      varobj<-obj$datamatic$variables[[variable64]]
      termobj<-sapply(term64,function(term) obj$datamatic$variables[[term]])
      n<-length(term64)
      j<-n
      

      resultsList<-list()
      while(j>1) {
        
        results=try_hard({
            ### mods are the variables that form the interaction with simple (variable)
            mods<-term64[j:n]
            conditions<-lapply(seq_along(term64),function(i) {
                              .obj<-obj$datamatic$variables[[term64[i]]]
                              ## if numeric are part of the interactions (i<j)
                              ## they are conditioned to 0 1 to create a proper interaction
                              if (.obj$type=="numeric") {
                                    if (i<j) c(0,1) else .obj$levels
                              }  else
                                      NULL
              })
         
            names(conditions)<-term64
            conditions<-conditions[sapply(conditions, function(x) is.something(x))]
            ## these are the selected moderators
            .names<-setdiff(term64,mods)
            ## this is the table key, if needed
            .key<-c(variable,rev(term[1:(j-1)]))
            test<-any(jmvcore::composeTerm(sort(.key)) %in% jmvcore::composeTerms(lapply(obj$options$model_terms,sort)))
            if (!test) {
              msg<-paste("Interaction",jmvcore::composeTerm(.key),"is not in the model, results may be misleading.")
              obj$warning<-list(topic="simpleInteractions",message=msg)
            }
            
            grid_list<-list(object=obj$model,
                            at = conditions,
                            estName="estimate"
                            )
            if (obj$option("df_method"))
              grid_list[["lmer.df"]]<-tolower(obj$options$df_method)
            
            if (obj$option("se_method","robust")) {
              type<-obj$options$robust_method
              grid_list[["vcov."]]<-function(x,...) sandwich::vcovHC(x,type=type,...)
            }
            
            if (varobj$type=="numeric") {
                grid_list[["specs"]]<-term64
                grid_list[["var"]]<-variable64
                emgrid<-do.call(emmeans::emtrends,grid_list)
              
            }
            else  { 
                  grid_list[["specs"]]<-c(variable64,term64)
                  emgrid<-do.call(emmeans::emmeans,grid_list)
                  .names<-c(variable64,.names)
            }
            
            ### we need the interaction contrast to be in obj because it should count the times
            ## it is called to know which variable should be contrasted
            .datamatic<-rev(sapply(.names, function(.name) obj$datamatic$variables[[.name]]))
            
            data <- insight::get_data(obj$model,source="frame")
            opts_list<-list(object=emgrid,by=mods,interaction=list(obj$interaction_contrast),datamatic=.datamatic)
            resgrid<-do.call(emmeans::contrast,opts_list)

            res<-as.data.frame(resgrid)
  
            ### deal with confidence intervals ###

            
            if (!obj$option("ci_method","wald")) {
              
              grid_list$object<-obj$boot_model
              
              if (varobj$type=="numeric") 
                emgrid<-do.call(emmeans::emtrends,grid_list)
              else  
                emgrid<-do.call(emmeans::emmeans,grid_list)
              
              opts_list[["object"]]<-emgrid
              resgrid<-do.call(emmeans::contrast,opts_list)
              cidata<-as.data.frame(parameters::model_parameters(resgrid,ci_method=obj$options$ci_method))
              res$est.ci.lower<-cidata$CI_low
              res$est.ci.upper<-cidata$CI_high
              
            } else {
              
              ci<-as.data.frame(stats::confint(resgrid,level=obj$ciwidth))
              res<-cbind(res,ci[,c(ncol(ci)-1,ncol(ci))])
              
            }
            
            ##
            names(res)[(ncol(res)-6):ncol(res)]<-c("estimate","se","df","test","p","est.ci.lower","est.ci.upper")
            names(res)[1:length(.names)]<-.names
          
            if (varobj$type=="numeric") {
                    res$focal<-variable
                    labnames<-c("focal",.names)
            } else
                    labnames<-.names
            
            res$effect<-apply(res[,labnames],1,jmvcore::stringifyTerm)
            
            for (.name in mods) {
               res[[.name]]<-factor(res[[.name]])
               levels(res[[.name]])<-obj$datamatic$variables[[.name]]$levels_labels
               res[[.name]]<-as.character(res[[.name]])
            }
           
            names(res)[names(res)%in% mods]<-make.names(paste0("mod_",fromb64(mods)))
            res
      })
        
      key<-smartTableName("simpleInteractions",.key,"coefficients")
        
      if (!isFALSE(results$error)) 
          obj$error<-list(topic=key,message=results$error)
      if (!isFALSE(results$warning)) 
          obj$warning<-list(topic=key,message=results$warning)
            
      params<-results$obj            

      class(params)<-c(paste0("simple_params_",obj$options$.caller),class(params))
      params<-add_effect_size(params,obj$model,variable64)
      
###### now we build the anova table ##########      
      results<-try_hard({ 
              res<-emmeans::test(resgrid,by=mods,join=T)
              names(res)[(ncol(res)-3):ncol(res)]<-c("df1","df2","test","p")
        
              if (varobj$type=="numeric") 
                  res$effect<-jmvcore::stringifyTerm(c(variable,fromb64(.names)))
              else
                  res$effect<-jmvcore::stringifyTerm(c(fromb64(.names)))
        
              for (.name in mods) {
                  res[[.name]]<-factor(res[[.name]])
                  levels(res[[.name]])<-obj$datamatic$variables[[.name]]$levels_labels
                  res[[.name]]<-as.character(res[[.name]])
              }
              names(res)[1:length(mods)]<-make.names(paste0("mod_",fromb64(mods)))
              class(res)<-c(paste0("simple_anova_",obj$options$model_type),class(res))
              res<-add_effect_size(res,obj$model)
              res
        })
      
      key<-smartTableName("simpleInteractions",.key,"anova")
      

      if (!isFALSE(results$error)) 
              obj$dispatcher$error<-list(topic=key,message=results$error)
      if (!isFALSE(results$warning)) 
              obj$warning<-list(topic=key,message=results$warning)
#      if (is.something(attr(obj$obj,"mesg")))
#           for ()
      anova<-results$obj      
      resultsList[[length(resultsList)+1]]<-list(anova,params)
      j<-j-1
        }
      
#      key<-"simpleInteractions_.._anova"
      
    if (varobj$requireFocus()) {
     custom<-varobj$contrast_labels[[1]]
     obj$warning<-list(topic="simpleInteractions_coefficients",message=paste("Simple interactions are estimated only for the custom constrast of variable",varobj$name))
     obj$warning<-list(topic="simpleInteractions_coefficients",message=paste("Estimates evaluate the interactions of contrast",custom," at different levels of the moderator"))
     obj$warning<-list(topic="simpleInteractions_anova",message=paste("Estimates evaluate the interactions of contrast",custom," at different levels of the moderator"))
    }

      jinfo("PROCEDURE: done")
 
      return(resultsList)
}

  

### this tells if a model term is dependent on the interaction
.is.scaleDependent<-function(model,term) {
  if (is.null(term))
    return(FALSE)
  try({
    modelterms<-stats::terms(model)
    modelterms<-attr(modelterms,"term.labels")
    nterm<-paste(term,collapse = ":")
    count<-length(grep(nterm,modelterms,fixed=T))
    if (count>1)
      return(TRUE)
  })
  FALSE
}



simple_models<-function(model,mods,obj, conditions=list()) {

  ## mods are supposed to be in b64
        levels <-lapply(mods, function(x) {
                 if (x %in% names(conditions))
                     return(conditions[[x]])
          
                if (obj$datamatic$variables[[x]]$type=="factor")
                        seq_along(obj$datamatic$variables[[x]]$levels)
                else 
                        obj$datamatic$variables[[x]]$levels
            })
        rows  <- expand.grid(levels)
        names(rows)  <-  mods
         data64      <-  insight::get_data(model,source="frame")
         results     <-  list()
         ## here we do the actual simple model for each combination of moderator levels ####
         for (i in 1:nrow(rows)) {
                .data1<-data64
                 for (.name in mods) {
                        if (is.factor(.data1[[.name]]))
                               stats::contrasts(.data1[[.name]])<-stats::contr.treatment(nlevels(.data1[[.name]]),base = rows[i,.name])
                        else
                               .data1[[.name]]<-.data1[[.name]]-rows[i,.name]
                 }
                 ladd(results)  <-  stats::update(model,data=.data1)

         }
         return(results)
}

simple_models_labels<-function(mods,obj) {

  
  ## mods are supposed to be in plain name
        levels <-lapply(mods, function(x) {

                if (obj$datamatic$variables[[tob64(x)]]$type=="factor")
                        seq_along(obj$datamatic$variables[[tob64(x)]]$levels_labels)
                else 
                        obj$datamatic$variables[[tob64(x)]]$levels_labels
            })
        rows  <- expand.grid(levels)
        names(rows)<-mods
        
        return(rows)
}

