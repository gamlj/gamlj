######  ##########

procedure.beta<- function(x,...) UseMethod(".beta")

.beta.default<-function(model) {
  
  data<-.getData(model)
  for (var in names(data)) {
    if (!is.factor(data[[var]]))
        data[[var]]<-as.numeric(scale(data[[var]]))
  }
  z<-update(model,data)
  parameters::parameters(model)$Coefficient
}


##### get model data #########

.getData<- function(x,...) UseMethod(".getData")

.getData.default<-function(model) {
  return(model$model)
}

.getData.lmer<-function(model) 
  return(model@frame)



############# post hoc ##############

procedure.posthoc <- function(obj) {
  
  terms <- tob64(obj$options$posthoc)
  dep <- obj$options$dep
  
  ### we set the model, if we need bootstrap ci, we give the bootstrapped model
   model<-obj$model
   
  .model<-model
   if (!obj$option("cimethod","wald")) 
       .model<-obj$boot_model

  ### check if we need robust standard error  
  vfun<-NULL
  if (obj$option("semethod","robust")) {
       vfun<-function(x,...) sandwich::vcovHC(x,type="HC3",...)
  }
  
  
  if (length(terms) == 0) 
    return()
  
  postHocTables <- list()
  
  for (ph in terms) {
    
  
    ## emmeans list comparison levels with a strange order. So we pass the inverted order of the term
    ## so the final table will look sorted in a more sensible way
    term <- jmvcore::composeTerm(rev(ph))
    
      none <- .posthoc(model, term, "none", vfun=vfun)
      bonferroni <- .posthoc(model, term, "bonferroni",vfun=vfun)
      holm <- .posthoc(model, term, "holm",vfun=vfun)
      tukey <- .posthoc(model, term, "tukey",vfun=vfun)
      sidak <- .posthoc(model, term, "sidak",vfun=vfun)
      scheffe <- .posthoc(model, term, "scheffe",vfun=vfun)
      
      cidata <- .posthoc_ci(.model,term,obj$ciwidth,obj$options$cimethod,vfun=vfun)


      tableData <- as.data.frame(none, stringAsFactors = FALSE)
      tableData$contrast <- as.character(tableData$contrast)
     
      tableData$bonf <- bonferroni$p
      tableData$holm <- holm$p
      tableData$tukey <- tukey$p
      tableData$scheffe <- scheffe$p
      tableData$sidak <- sidak$p
      tableData$est.ci.lower<-cidata$est.ci.lower       
      tableData$est.ci.upper<-cidata$est.ci.upper       

    .cont <- as.character(tableData$contrast)
    .cont <- gsub(" - ", "-", .cont, fixed = T)
    .cont <- gsub(" / ", "/", .cont, fixed = T)


    .labs <- sapply(.cont, function(a) {
      sapply(strsplit(as.character(a), "[- ,/]"), trimws, USE.NAMES = F, simplify = F)
    })
    .labs <- fromb64(.labs)
    labs <- do.call("rbind", .labs)

    cols <- make.names(c(rev(ph),rev(ph)),unique = T)
    colnames(labs) <- cols

    tableData <- cbind(labs, tableData)
    rownames(tableData)<-NULL
    
    for (col in cols)
      tableData[,col]<-as.character(tableData[,col])
    
   .names<-make.names(fromb64(rev(ph)))
    names(tableData)[1:length(cols)]<-c(paste0(.names,"1"),paste0(.names,"2"))

    if ("Response" %in% names(tableData))
        tableData$Response<-as.character(tableData$Response)
    postHocTables[[length(postHocTables)+1]]<-tableData
  }

    postHocTables
  
}

procedure.posthoc_effsize <- function(obj) {


  terms <- tob64(obj$options$posthoc)
  dep <- obj$options$dep
  
  ### at the moment (version 3.0.0), no bootstrap for d indeces
  model<-obj$model
  
  ### check if we need robust standard error  
  vfun<-NULL
  if (obj$option("semethod","robust")) {
    vfun<-function(x,...) sandwich::vcovHC(x,type="HC3",...)
  }
  
  
  if (length(terms) == 0) 
    return()
  
  dTables <- list()
  
  for (ph in terms) {
    
    
    ## emmeans list comparison levels with a strange order. So we pass the inverted order of the term
    ## so the final table will look sorted in a more sensible way
    term <- jmvcore::composeTerm(rev(ph))
    
    base <- .posthoc(model, term, "none", vfun=vfun)

    tableData <- as.data.frame(base, stringAsFactors = FALSE)
    tableData$contrast <- as.character(tableData$contrast)
    
    .cont <- as.character(tableData$contrast)
    .cont <- gsub(" - ", "-", .cont, fixed = T)
    .cont <- gsub(" / ", "/", .cont, fixed = T)
    
    .labs <- sapply(.cont, function(a) {
      sapply(strsplit(as.character(a), "[- ,/]"), trimws, USE.NAMES = F, simplify = F)
    })
    .labs <- fromb64(.labs)
    labs <- do.call("rbind", .labs)
    
    cols <- make.names(c(rev(ph),rev(ph)),unique = T)
    colnames(labs) <- cols
    
    tableData <- cbind(labs, tableData)
    rownames(tableData)<-NULL
    
    for (col in cols)
      tableData[,col]<-as.character(tableData[,col])
    
    .names<-make.names(fromb64(rev(ph)))
    names(tableData)[1:length(cols)]<-c(paste0(.names,"1"),paste0(.names,"2"))
   
    
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
    
    t<-sqrt(df)*tableData$estimate/(2*sigma(model))
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

.posthoc.default <- function(model, term, adjust,vfun=NULL) {

    termf <- stats::as.formula(paste("~", term))
    
    data <- mf.getModelData(model)
    opts_list<-list(object=model,specs=termf, type = "response", data = data)

    if (is.something(vfun))
         opts_list[["vcov."]]<-vfun    

      referenceGrid <- do.call(emmeans::emmeans,opts_list)
      terms <- jmvcore::decomposeTerm(term)
      labs <- referenceGrid@grid[terms]
      newlabs <- sapply(labs, function(a) sapply(a, function(b) tob64(as.character(b))))
      referenceGrid@grid[terms] <- newlabs
      results <- summary(graphics::pairs(referenceGrid), adjust = adjust,infer = c(FALSE,TRUE))
      names(results)<-c("contrast","estimate","se","df","test","p")
      
      
  results
}

.posthoc.multinom <- function(model, term, adjust,ci=FALSE) {
  
  results <- try({
    dep <- names(attr(stats::terms(model), "dataClass"))[1]
    dep <- jmvcore::composeTerm(dep)
    tterm <- stats::as.formula(paste("~", paste(dep, term, sep = "|")))
    data <- mf.getModelData(model)
    suppressMessages({
      referenceGrid <- emmeans::emmeans(model, tterm, transform = "response", data = data)
      terms <- jmvcore::decomposeTerm(term)
      labs <- referenceGrid@grid[terms]
      newlabs <- sapply(labs, function(a) sapply(a, function(b) tob64(as.character(b))))
      referenceGrid@grid[terms] <- newlabs
      results <- summary(graphics::pairs(referenceGrid, by=dep, adjust = adjust),infer = c(ci,TRUE))
    })
  })
  
  return(results)
}


.posthoc_ci <- function(x, ...) UseMethod(".posthoc_ci")

.posthoc_ci.default=function(model,term,width,method,vfun=NULL) {
  
  termf <- stats::as.formula(paste("pairwise ~", term))

  opts_list<-list(object=model,specs=termf, type = "response")
  
  if (is.something(vfun))
    opts_list[["vcov."]]<-vfun    
  
  referenceGrid <- do.call(emmeans::emmeans,opts_list)
  results<-as.data.frame(parameters::parameters(referenceGrid$contrasts,ci=width,ci_method=method))
  results<-as.data.frame(cbind(results$CI_low,results$CI_high))
  names(results)<-c("est.ci.lower","est.ci.upper")
  results
  
}

######## end post hoc #############

### Estimated Marginal Means ####

procedure.emmeans<-function(obj) {
  
  gstart("PROCEDURE: Estimated Marginal Means")
  terms<-obj$options$emmeans
  
  type<-"response"
  results<-list()
  for (term in terms) {
    ### we need to reverse the term because emmeans order levels in a strange way
    term64<-tob64(rev(term))
    
    ## include the dependent for multinomial ##
    if (obj$options$modelSelection=="multinomial")
        term64<-c(obj$datamatic$dep$name64,term64)
    
    ## first we get the levels of covs at which to condition the estimation ##
    ## we also get the labels for those values ####
    conditions<-list()
    for (.term in term64) {
      var<-obj$datamatic$variables[[.term]]
      if( var$type=="numeric") {
        conditions[[.term]]<-var$levels 
      }
    }
  
    ### prepare the options ###
    opts_list<-list(obj$model,
                    specs=term64,
                    at=conditions,
                    type=type,
                    mode=mode,
                    nesting = NULL,
                    options = c(level=obj$ciwidth)
    )
    if (obj$option("dfmethod"))
       opts_list[["lmer.df"]]<-tolower(obj$options$dfmethod)
    
    if (obj$option("semethod","robust")) 
      opts_list[["vcov."]]<-function(x,...) sandwich::vcovHC(x,type="HC3",...)

    if (obj$option("modelSelection","ordinal"))
      opts_list[["mode"]]<-"mean.class"
    
    ### now we get the estimated means #######
    referenceGrid<-do.call(emmeans::emmeans,opts_list)
    tableData<-as.data.frame(referenceGrid)
    ### rename the columns ####
    names(tableData)<-c(term64,"estimate","se","df","est.ci.lower","est.ci.upper")
   
    ### fix the labels  ###

    for (.name in term64) {
      tableData[[.name]]<-factor(tableData[[.name]])
      levels(tableData[[.name]])<-obj$datamatic$variables[[.name]]$levels_labels
      tableData[[.name]]<-as.character(tableData[[.name]])
    }

    ## rename the dependent for multinomial ##
    if (obj$options$modelSelection=="multinomial")
         names(tableData)[1]<-"Response"

    names(tableData)[1:length(term64)]<-rev(term)
    results[[length(results)+1]]<-tableData
  }
  gend()
  results
}  
  

procedure.simpleEffects<- function(x,...) UseMethod(".simpleEffects")


.simpleEffects.default<-function(model,obj) {

  gstart("PROCEDURE: Simple Effects estimated")
  variable<-obj$options$simpleVariable
  variable64<-tob64(variable)
  term<-obj$options$simpleModerators
  ### we need to reverse the term because emmeans order levels in a strange way
  term64<-tob64(term)
  varobj<-obj$datamatic$variables[[variable64]]
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
    ### and then add the CI with a specific method declared in options$cimethod


    if (varobj$type=="factor") {
      alist[["specs"]]=c(variable64,term64)
      ### at the moment (2021) with custom contrast function (not string), infer=c() does not work ####
      referenceGrid<-do.call(emmeans::emmeans,alist)
      grid<-emmeans::contrast(referenceGrid,
                                   by=term64,
                                   method =.local.emmc,
                                   datamatic=varobj
                                   )
    } else {
      alist[["specs"]]<-term64
      alist[["var"]]<-variable64
      grid <- do.call(emmeans::emtrends, alist)
    }
    grid
    }
    ##
    
    opts_list<-list(object=model,
                    at=conditions,
                    nesting = NULL,
                    infer=c(TRUE,TRUE),
                    estName="estimate"
    )
    
    if (obj$option("dfmethod"))
      opts_list[["lmer.df"]]<-tolower(obj$options$dfmethod)
    
    if (obj$option("semethod","robust"))
      opts_list[["vcov."]]<-function(x,...) sandwich::vcovHC(x,type="HC3",...)
    
    
    
    grid<-.get_se(opts_list)
    estimates<-as.data.frame(grid)

    ## deal with  CI
    if (!obj$option("cimethod","wald")) {
      
      opts_list$object<-obj$boot_model
      cidata<-.get_se(opts_list)
      cidata<-as.data.frame(parameters::parameters(cidata,ci_method=obj$options$cimethod))
      estimates$est.ci.lower<-cidata$CI_low
      estimates$est.ci.upper<-cidata$CI_high
      
    } else {
      
      cidata<-stats::confint(grid,level = obj$ciwidth)    
      estimates$est.ci.lower<-cidata$lower.CL
      estimates$est.ci.upper<-cidata$upper.CL
    }

    .transnames<-list(test="t.ratio",p="p.value",se="SE")
    names(estimates)<-transnames(names(estimates),.transnames)    

    if (!("contrast" %in% names(estimates)))
          estimates$contrast<-variable

    
    for (.name in term64) {
            estimates[[.name]]<-factor(estimates[[.name]])
            levels(estimates[[.name]])<-obj$datamatic$variables[[.name]]$levels_labels
            estimates[[.name]]<-as.character(estimates[[.name]])
    }
    
    estimates$contrast<-as.character(estimates$contrast)
    ### add effect sizes depending on the model and table
    class(estimates)<-c(paste0("simple_params_",obj$options$modelSelection),class(estimates))
    .params<-add_effect_size(estimates,model,variable64)
    
    
    ### now we build the anova table ###
    .anova<-as.data.frame(emmeans::test(grid, join=TRUE, by = term64))
    names(.anova)<-c(term64,"df1","df2","test","p")
   
    
    ### fix labels and make sure they are not factors or stuff    
    for (.name in term64) {
      .anova[[.name]]<-factor(.anova[[.name]])
      levels(.anova[[.name]])<-obj$datamatic$variables[[.name]]$levels_labels
      .anova[[.name]]<-as.character(.anova[[.name]])
    }
    #### fix names ####
    
    
    ### make fix depending of the type of model ###    
    class(.anova)<-c(paste0("simple_anova_",obj$options$modelSelection),class(.anova))
    .anova<-add_effect_size(.anova,model)
    ### check some stuff 
    .all <- c(term64,variable64)
    test <- unlist(sapply(.all,function(x) !(.is.scaleDependent(obj$model,x))))
    .issues <- .all[test]

    if (is.something(.issues))
      obj$dispatcher$warnings<-list(topic="simpleEffects_anova",message=paste("Variable",paste(fromb64(.issues),collapse = ","),"is included in the simple effects analysis but it does not appear in any interaction"))

  gend()    
  return(list(.anova,.params))

}  

.local.emmc<-function(levs, datamatic=NULL) {

  # get the contrast weights
  codes <- datamatic$contrast_values
  if (datamatic$method=="dummy")
        codes<-datamatic$contrast_codes("simple")

  # transform the model matrix into the contrast matrix
  M <- as.data.frame(MASS::ginv(t(codes)))
  # set some labels
  names(M)<-datamatic$contrast_labels
  M
}




.simpleEffects.multinom<-function(model,obj) {


        levels <-lapply(obj$options$simpleModerators, function(x) {
    
                if (obj$datamatic$variables[[tob64(x)]]$type=="factor")
                        seq_along(obj$datamatic$variables[[tob64(x)]]$levels)
                else 
                        obj$datamatic$variables[[tob64(x)]]$levels
            })
  
  
        vars  <- lapply(obj$options$simpleModerators, function(x) obj$datamatic$variables[[tob64(x)]])
  
        rows  <- expand.grid(levels)
        names(rows)  <-  tob64(obj$options$simpleModerators)
        variable64   <-  tob64(obj$options$simpleVariable)
        varobj       <-  obj$datamatic$variables[[variable64]]
  
        .names       <-  names(rows)
         parameters  <-  data.frame()
         anovas  <-  data.frame()
         
         data64      <-  mf.getModelData(model)
         
         for (i in 1:nrow(rows)) {
                .data1<-data64
                 for (.name in .names) {
                        if (is.factor(.data1[[.name]]))
                               contrasts(.data1[[.name]])<-contr.treatment(nlevels(.data1[[.name]]),base = rows[i,.name])
                        else
                               .data1[[.name]]<-.data1[[.name]]-rows[i,.name]
                 }
                 .model  <-  update(model,data=.data1)
                 params  <-  as.data.frame(parameters::parameters(.model))
                 params  <-  params[params$Parameter %in% varobj$paramsnames64,]
                 params[,.names]  <-  rows[i,]         
                 parameters  <-  rbind(parameters,params)
                 oneanova <- car::Anova(.model,test="LR",type=3,singular.ok=T)
                 oneanova <- oneanova[rownames(oneanova) %in% varobj$name64,]
                 oneanova[,.names]  <-  rows[i,]         
                 anovas   <- rbind(anovas,oneanova)
    
         }
         names(parameters)[1:8]<-c("contrast","estimate","se","nothing","ci.lower","ci.upper","test","df","p")
         
         for (.name in .names) {
           parameters[[.name]]<-factor(parameters[[.name]])
           levels(parameters[[.name]])<-obj$datamatic$variables[[.name]]$levels_labels
           parameters[[.name]]<-as.character(parameters[[.name]])
         }
         ### fix labels for  the response contrasts
         parameters$Response<-factor(parameters$Response)
         levels(parameters$Response)<-unlist(obj$datamatic$dep$contrast_labels)
         parameters$Response<-as.character(parameters$Response)
         
         ### fix labels for the contrast column ###
         parameters$contrast<-factor(parameters$contrast)
         levels(parameters$contrast)<-unlist(varobj$contrast_labels)
         parameters$contrast<-as.character(parameters$contrast)
         
         names(anovas)[1:3]<-c("test","df1","p")
         
         for (.name in .names) {
           anovas[[.name]]<-factor(anovas[[.name]])
           levels(anovas[[.name]])<-obj$datamatic$variables[[.name]]$levels_labels
           anovas[[.name]]<-as.character(anovas[[.name]])
         }
         

         return(list(anovas,parameters))
}
## this is humbly supercool ###
## we want the simple interactions nested in the highest interaction required
procedure.simpleInteractions<-function(obj) {
  
     gstart("PROCEDURE: simple Interactions")
  
      variable<-obj$options$simpleVariable
      variable64<-tob64(variable)
      term<-obj$options$simpleModerators
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
            test<-any(jmvcore::composeTerm(sort(.key)) %in% jmvcore::composeTerms(lapply(obj$options$modelTerms,sort)))
            if (!test) {
              msg<-paste("Interaction",jmvcore::composeTerm(.key),"is not in the model, results may be misleading.")
              obj$dispatcher$warnings<-list(topic="simpleInteractions_anova",message=msg)
              obj$dispatcher$warnings<-list(topic="simpleInteractions_coefficients",message=msg)
              
            }
            
            grid_list<-list(object=obj$model,
                            at = conditions,
                            estName="estimate"
                            )
            
            if (obj$option("dfmethod"))
              grid_list[["lmer.df"]]<-tolower(obj$options$dfmethod)
            
            if (obj$option("semethod","robust"))
              grid_list[["vcov."]]<-function(x,...) sandwich::vcovHC(x,type="HC3",...)
            
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
            
            opts_list<-list(object=emgrid,by=mods,interaction=list(obj$interaction_contrast),datamatic=.datamatic)
            resgrid<-do.call(emmeans::contrast,opts_list)
            res<-as.data.frame(resgrid)
            
            ### deal with confidence intervals ###

            
            if (!obj$option("cimethod","wald")) {
              
              grid_list$object<-obj$boot_model
              
              if (varobj$type=="numeric") 
                emgrid<-do.call(emmeans::emtrends,grid_list)
              else  
                emgrid<-do.call(emmeans::emmeans,grid_list)
              
              opts_list[["object"]]<-emgrid
              resgrid<-do.call(emmeans::contrast,opts_list)
              cidata<-as.data.frame(parameters::model_parameters(resgrid,ci_method=obj$options$cimethod))
              res$est.ci.lower<-cidata$CI_low
              res$est.ci.upper<-cidata$CI_high
              
            } else {
              
              ci<-as.data.frame(stats::confint(resgrid,level=obj$ciwidth))
              res<-cbind(res,ci[,c(ncol(ci)-1,ncol(ci))])
              
            }
            
            
            ##
            
            names(res)[(ncol(res)-6):ncol(res)]<-c("estimate","se","df","t","p","est.ci.lower","est.ci.upper")
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
            names(res)[names(res)%in% mods]<-make.names(paste0("var_",fromb64(mods)))
            res
      })
        
      key<-smartTableName("simpleInteractions",.key,"coefficients")
        
      if (!isFALSE(results$error)) 
          obj$dispatcher$error<-list(topic=key,message=results$error)
      if (!isFALSE(results$warning)) 
          obj$dispatcher$warnings<-list(topic=key,message=results$warning)
            
      params<-results$obj            

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
              names(res)[1:length(mods)]<-make.names(paste0("var_",fromb64(mods)))
              class(res)<-c(paste0("simple_anova_",obj$options$modelSelection),class(res))
              res<-add_effect_size(res,obj$model)
              res
        })
      
      key<-smartTableName("simpleInteractions",.key,"anova")
      
            
      if (!isFALSE(results$error)) 
              obj$dispatcher$error<-list(topic=key,message=results$error)
      if (!isFALSE(results$warning)) 
              obj$dispatcher$warnings<-list(topic=key,message=results$warning)
      anova<-results$obj      
                          
      resultsList[[length(resultsList)+1]]<-list(anova,params)
      j<-j-1
        }
      
#      key<-"simpleInteractions_.._anova"
      


      gend()
 
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





