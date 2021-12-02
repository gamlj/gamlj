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
  model<-obj$model
  
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
    suppressWarnings({
      none <- .posthoc(model, term, "none",ci=TRUE, vfun=vfun,bootstrap=(obj$options$cimethod=="boot"))
      bonferroni <- .posthoc(model, term, "bonferroni",vfun=vfun)
      holm <- .posthoc(model, term, "holm",vfun=vfun)
      tukey <- .posthoc(model, term, "tukey",vfun=vfun)
      sidak <- .posthoc(model, term, "sidak",vfun=vfun)
      scheffe <- .posthoc(model, term, "scheffe",vfun=vfun)
      
    })  # suppressWarnings
    if (is.character(none)) 
      obj$warnings<-list(topic="posthoc", message=WARNS["ph.nojoy"]) 
    else {
        tableData <- as.data.frame(none, stringAsFactors = FALSE)
        tableData$contrast <- as.character(tableData$contrast)
        if (length(names(tableData))==9)
              colnames(tableData) <- c("contrast", "Response", "estimate", "se","df" ,"est.ci.lower","est.ci.upper", "test", "none")
        else
              colnames(tableData) <- c("contrast", "estimate", "se","df" ,"est.ci.lower","est.ci.upper", "test", "none")
        
        tableData$bonf <- bonferroni$p.value
        tableData$holm <- holm$p.value
        tableData$tukey <- tukey$p.value
        tableData$scheffe <- scheffe$p.value
        tableData$sidak <- sidak$p.value
        
      }
    
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
  if (obj$options$cimethod=="boot")
       obj$warnings<-list(topic="posthoc",message="Bootstrap confidence intervals")
    

    postHocTables
  
}


###### post hoc ##########
.posthoc <- function(x, ...) UseMethod(".posthoc")

.posthoc.default <- function(model, term, adjust,ci=FALSE, bootstrap=FALSE,vfun=NULL) {

    termf <- stats::as.formula(paste("~", term))
    data <- mf.getModelData(model)

    opts_list<-list(object=model,specs=termf, type = "response", data = data)

    if (is.something(vfun))
         opts_list[["vcov."]]<-vfun    
    
    suppressMessages({
      referenceGrid <- do.call(emmeans::emmeans,opts_list)
      terms <- jmvcore::decomposeTerm(term)
      labs <- referenceGrid@grid[terms]
      newlabs <- sapply(labs, function(a) sapply(a, function(b) tob64(as.character(b))))
      referenceGrid@grid[terms] <- newlabs
      results <- summary(graphics::pairs(referenceGrid), adjust = adjust,infer = c(ci,TRUE))

      if (bootstrap & ci) {
        model<-parameters::bootstrap_model(model)
        referenceGrid <- emmeans::emmeans(model, termf, type = "response", data = data)
        ci_results<-summary(graphics::pairs(referenceGrid))
        results$est.lower.CL<-ci_results$lower.HPD
        results$est.upper.CL<-ci_results$upper.HPD
        
      }
      
    })
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

######## end post hoc #############

### Estimated Marginal Means ####

procedure.emmeans<-function(obj) {
  
  ginfo("Estimated Marginal Means")
  terms<-obj$options$emmeans
  mode<-NULL
  if (obj$option("modelSelection","ordinal"))
    mode<-"mean.class"
  
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
    df<-NULL
    if (obj$option("dfmethod"))
        df<-tolower(obj$options$dfmethod)
    
    ### now we get the estimated means #######
    referenceGrid<-emmeans::emmeans(obj$model,
                                    specs=term64,
                                    at=conditions,
                                    type=type,
                                    mode=mode,
                                    nesting = NULL,
                                    lmer.df = df,
                                    options = c(level=obj$ciwidth))
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
  
  ginfo("End of Estimated Marginal Means")
  results
}  
  

procedure.simpleEffects<- function(x,...) UseMethod(".simpleEffects")


.simpleEffects.default<-function(model,obj) {
  
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
    ### now we get the estimated means #######
    opts_list<-list(model,
                    at=conditions,
                    nesting = NULL
                    )
    
    if (obj$option("dfmethod"))
      opts_list[["lmer.df"]]<-tolower(obj$options$dfmethod)
    
    if (obj$option("semethod","robust"))
      opts_list[["vcov."]]<-function(x,...) sandwich::vcovHC(x,type="HC3",...)

    
    if (varobj$type=="factor") {
      ### at the moment (2021) with custom contrast function (not string), infer=c() does not work ####
      opts_list[["specs"]]=c(variable64,term64)
      referenceGrid<-do.call(emmeans::emmeans,opts_list)
      estimates<-emmeans::contrast(referenceGrid,
                                   by=term64,
                                   method =.local.emmc,
                                   datamatic=varobj)

    }
    else {
      
      opts_list[["specs"]]<-term64
      opts_list[["var"]]<-variable64
      opts_list[["infer"]]<-c(T,T)
      
      estimates <- do.call(emmeans::emtrends, opts_list)
    }
    ##
    res<-as.data.frame(estimates)
    if (varobj$type=="factor") {
      
            ci<-as.data.frame(stats::confint(estimates,level=obj$ciwidth))
            res<-cbind(res,ci[,c(ncol(ci)-1,ncol(ci))])
            names(res)<-c("contrast",term64,"estimate","se","df","test","p","est.ci.lower","est.ci.upper")
            
    } else {
            names(res)<-c(term64,"estimate","se","df","est.ci.lower","est.ci.upper","test","p")
            res$contrast<-varobj$name
    }
    
    for (.name in term64) {
            res[[.name]]<-factor(res[[.name]])
            levels(res[[.name]])<-obj$datamatic$variables[[.name]]$levels_labels
            res[[.name]]<-as.character(res[[.name]])
    }
    
    res$contrast<-as.character(res$contrast)
    ### add effect sizes depending on the model and table
    class(res)<-c(paste0("simple_params_",obj$options$modelSelection),class(res))
    params<-add_effect_size(res,model,variable64)

    ### now we build the anova table ###
    res<-as.data.frame(emmeans::test(estimates, join=TRUE, by = term64))
    names(res)<-c(term64,"df1","df2","test","p")

    ### fix labels and make sure they are not factors or stuff    
    for (.name in term64) {
      res[[.name]]<-factor(res[[.name]])
      levels(res[[.name]])<-obj$datamatic$variables[[.name]]$levels_labels
      res[[.name]]<-as.character(res[[.name]])
    }
    #### fix names ####
    

    ### make fix depending of the type of model ###    
    class(res)<-c(paste0("simple_anova_",obj$options$modelSelection),class(res))
    anova<-add_effect_size(res,model)
    ### check some stuff 
    .all <- c(term64,variable64)
    test <- unlist(sapply(.all,function(x) !(.is.scaleDependent(obj$model,x))))
    .issues <- .all[test]
    if (is.something(.issues))
      obj$warnings<-list(topic="simpleEffects_anova",message=paste("Variables",paste(.issues,collapse = ","),"are included in the simple effects analysis but they do not appear in any interaction"))

    ## check issues with the variables ##
    for (.term in term64) {
      if (is.something(obj$datamatic$variables[[.term]]$warnings)) {
            obj$absorbe_issues(obj$datamatic$variables[[.term]])
      }
    }
  ginfo("PROCEDURES: Simple Effects estimated")
    
  return(list(anova,params))

}  

.local.emmc<-function(levs, datamatic=NULL) {
  #  print(datamatic)
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
  
     ginfo("simple Interactions started")
  
      variable<-obj$options$simpleVariable
      variable64<-tob64(variable)
      term<-obj$options$simpleModerators
      term64<-tob64(term)
      varobj<-obj$datamatic$variables[[variable64]]
      termobj<-sapply(term64,function(term) obj$datamatic$variables[[term]])
      n<-length(term64)
      j<-n
      
      df<-NULL
      if (obj$option("dfmethod"))
        df<-tolower(obj$options$dfmethod)
      
      resultsList<-list()
      while(j>1) {
            mods<-term64[j:n]
            conditions<-lapply(seq_along(term64),function(i) {
                              .obj<-obj$datamatic$variables[[term64[i]]]
                              if (.obj$type=="numeric") {
                                    if (i<j) c(0,1) else .obj$levels
                              }  else
                                      NULL
              })
            
            names(conditions)<-term64
            conditions<-conditions[sapply(conditions, function(x) is.something(x))]
            .names<-setdiff(term64,mods)
            opts_list<-list(object=obj$model,
                            at = conditions
                            )
            
            if (obj$option("dfmethod"))
              opts_list[["lmer.df"]]<-tolower(obj$options$dfmethod)
            
            if (obj$option("semethod","robust"))
              opts_list[["vcov."]]<-function(x,...) sandwich::vcovHC(x,type="HC3",...)
            
            if (varobj$type=="numeric") {
              
              opts_list[["specs"]]<-term64
              opts_list[["var"]]<-variable64
              results<-try_hard(do.call(emmeans::emtrends,opts_list))
              
            }
            else  { 
              
                  opts_list[["specs"]]<-c(variable64,term64)
                  results<-try_hard(do.call(emmeans::emmeans,opts_list))
                  .names<-c(variable64,.names)
            }
            
            obj$warnings<-list(topic="simpleInteractions",message=results$warning)
            obj$errors<-list(topic="simpleInteractions",message=results$error)
            emgrid<-results$obj 
            ### we need the interaction contrast to be in obj because it should count the times
            ## it is called to know which variable should be contrasted
            .datamatic<-rev(sapply(.names, function(.name) obj$datamatic$variables[[.name]]))
            
            opts_list<-list(object=emgrid,by=mods,interaction=list(obj$interaction_contrast),datamatic=.datamatic)
            results<-try_hard(do.call(emmeans::contrast,opts_list))
            
            obj$warnings<-list(topic="simpleInteractions",message=results$warning)
            obj$errors<-list(topic="simpleInteractions",message=results$error)
            resgrid<-results$obj
            ci<-as.data.frame(stats::confint(resgrid,level=obj$ciwidth))
            res<-as.data.frame(resgrid)
            res<-cbind(res,ci[,c(ncol(ci)-1,ncol(ci))])
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
            params<-res
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
            anova<-add_effect_size(res,obj$model)
            
            resultsList[[length(resultsList)+1]]<-list(anova,params)
            j<-j-1
        }
      

      ginfo("simple Interactions ended")
 
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





