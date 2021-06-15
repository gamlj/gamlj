######  ##########

procedure.beta<- function(x,...) UseMethod(".beta")

.beta.default<-function(model) {
  
  data<-.getData(model)
  for (var in names(data)) {
    if (!is.factor(data[[var]]))
        data[[var]]<-as.numeric(scale(data[[var]]))
  }
  z<-update(model,data)
  parameters::parameters(z)$Coefficient

}


##### get model data #########

.getData<- function(x,...) UseMethod(".getData")

.getData.default<-function(model) {
  return(model$model)
}

.getData.lmer<-function(model) 
  return(model@frame)



############# post hoc ##############

procedure.posthoc <- function(Obj) {
  
  ginfo("Populate posthoc...")
  terms <- Obj$options$posthoc
  dep <- Obj$options$dep
  model<-Obj$model
  if (length(terms) == 0) 
    return()
  
  postHocTables <- list()
  
  for (ph in terms) {
    
    
    term <- jmvcore::composeTerm(ph)
    ## emmeans list comparison levels with a strange order. So we pass the inverted order of the term
    ## so the final table will look sorted in a more sensible way
    termB64 <- jmvcore::composeTerm(jmvcore::toB64(rev(ph)))
    suppressWarnings({
      none <- .posthoc(model, termB64, "none",ci=TRUE)
      bonferroni <- .posthoc(model, termB64, "bonferroni")
      holm <- .posthoc(model, termB64, "holm")
      tukey <- .posthoc(model, termB64, "tukey")
      
    })  # suppressWarnings
    if (is.character(none)) 
      Obj$warnings<-list(topic="posthoc", WARNS["ph.nojoy"]) 
    else {
        tableData <- as.data.frame(none)
        tableData$contrast <- as.character(tableData$contrast)
        colnames(tableData) <- c("contrast", "estimate", "se","df" ,"ci.lower","ci.upper", "test", "none")
        tableData$bonf <- bonferroni[, 6]
        tableData$holm <- holm[, 6]
        tableData$tukey <- tukey[, 6]
      }
    
    .cont <- as.character(tableData$contrast)
    .cont <- gsub(" - ", "-", .cont, fixed = T)
    .cont <- gsub(" / ", "/", .cont, fixed = T)
    
    .labs64 <- sapply(.cont, function(a) {
      sapply(strsplit(as.character(a), "[- ,/]"), trimws, USE.NAMES = F, simplify = F)
    })
    .labs <- fromb64(.labs64)
    labs <- do.call("rbind", .labs)
    cols <- make.names(c(rev(ph),rev(ph)),unique = T)
    colnames(labs) <- cols
    
    tableData <- cbind(labs, tableData)
#    sortstring <- paste0("order(", paste0("tableData$", cols, collapse = ","), ")")
#    tableData <- tableData[eval(parse(text = sortstring)), ]
    for (col in cols)
      tableData[,col]<-as.character(tableData[,col])
    postHocTables[[length(postHocTables)+1]]<-tableData
  }
  ginfo("Populate posthoc done")

    postHocTables
  
}


###### post hoc ##########
.posthoc <- function(x, ...) UseMethod(".posthoc")

.posthoc.default <- function(model, term, adjust,ci=FALSE) {

    termf <- stats::as.formula(paste("~", term))
    data <- mf.getModelData(model)
    suppressMessages({
      referenceGrid <- emmeans::emmeans(model, termf, type = "response", data = data)
      terms <- jmvcore::decomposeTerm(term)
      labs <- referenceGrid@grid[terms]
      newlabs <- sapply(labs, function(a) sapply(a, function(b) jmvcore::toB64(as.character(b))))
      referenceGrid@grid[terms] <- newlabs
      table <- summary(graphics::pairs(referenceGrid), adjust = adjust,infer = c(ci,TRUE))
      
    })
  table
}

.posthoc.multinom <- function(model, term, adjust) {
  results <- try({
    dep <- names(attr(stats::terms(model), "dataClass"))[1]
    dep <- jmvcore::composeTerm(dep)
    tterm <- stats::as.formula(paste("~", paste(dep, term, sep = "|")))
    data <- mf.getModelData(model)
    suppressMessages({
      
      referenceGrid <- emmeans::emmeans(model, tterm, transform = "response", data = data)
      terms <- jmvcore::decomposeTerm(term)
      labs <- referenceGrid@grid[terms]
#      newlabs <- sapply(labs, function(a) sapply(a, function(b) jmvcore::toB64(as.character(b))))
#      referenceGrid@grid[terms] <- newlabs
      res <- summary(graphics::pairs(referenceGrid), adjust = adjust,infer = c(TRUE,TRUE))
      
      
    })
    res <- as.data.frame(res)
    res[, dep] <- NULL
    res
  })
  
  return(results)
}

######## end post hoc #############

### Estimated Marginal Means ####

procedure.emmeans<-function(obj) {
  
  ginfo("Estimated Marginal Means")
  terms<-obj$options$emmeans

  results<-list()
  for (term in terms) {
    ### we need to reverse the term because emmeans order levels in a strange way
    term64<-tob64(rev(term))
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
    referenceGrid<-emmeans::emmeans(obj$model,specs=term64,at=conditions,nesting = NULL,lmer.df = "Satterthwaite")
    tableData<-as.data.frame(referenceGrid)
    ### rename the columns ####
    names(tableData)<-c(term64,"estimate","se","df","ci.lower","ci.upper")
   
    ### change the labels for continuous variables ###

    for (.name in names(labels)) {
      vardata<-tableData[[.name]]
      labs<-labels[[.name]]
      values<-unique(vardata)
      for (i in seq_along(values))
                vardata[vardata==values[i]]<-labs[i]
      tableData[[.name]]<-vardata
    }
    
    ### make sure they are not factors or stuff    
    for (.name in term64) 
        tableData[[.name]]<-as.character(tableData[[.name]])
    
    results[[length(results)+1]]<-tableData
  }
  
  ginfo("End of Estimated Marginal Means")
  results
}  
  


procedure.simpleEffects<-function(obj) {
  
  ginfo("Simple Effects")
  variable<-obj$options$simpleVariable
  variable64<-tob64(variable)
  term<-obj$options$simpleModerators
  ### we need to reverse the term because emmeans order levels in a strange way
  term64<-tob64(rev(term))
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
    referenceGrid<-emmeans::emmeans(obj$model,specs=c(variable64,term64),at=conditions,nesting = NULL,lmer.df = "Satterthwaite")
    if (varobj$type=="factor") {
            ### at the moment (2021) with custom contrast function (not string), infer=c() does not work ####
            estimates<-emmeans::contrast(referenceGrid,
                                         by=term64,
                                         method =.local.emmc,datamatic=varobj)
            params<-as.data.frame(estimates)
            ci<-as.data.frame(stats::confint(estimates,level=obj$ciwidth))
            params<-cbind(params,ci[,c(ncol(ci)-1,ncol(ci))])
            names(params)<-c("contrast",term64,"estimate","se","df","test","p","ci.lower","ci.upper")

    }
    else {
            args<-list(obj$model,specs = term64, var = variable64, at = conditions,infer=c(T,T))
            estimates <- do.call(emmeans::emtrends, args)
            params <- as.data.frame(estimates)
            names(params)<-c(term64,"estimate","se","df","ci.lower","ci.upper","test","p")
            params$contrast<-varobj$name

    }
    ## then we fix the covs labels
    for (.name in names(labels)) {
      vardata<-params[[.name]]
      labs<-labels[[.name]]
      values<-unique(vardata)
      for (i in seq_along(values))
        vardata[vardata==values[i]]<-labs[i]
      params[[.name]]<-vardata
    }
    ### make sure they are not factors or stuff    
    for (.name in vars) 
      params[[.name]]<-as.character(params[[.name]])
    
    ### now we build the anova table ###à
    anova<-as.data.frame(emmeans::test(estimates, join=TRUE, by = term64))
    names(anova)<-c(term64,"df1","df2","test","p")

    ## than we fix the covs labels
    for (.name in names(labels)) {
      vardata<-anova[[.name]]
      labs<-labels[[.name]]
      values<-unique(vardata)
      for (i in seq_along(values))
        vardata[vardata==values[i]]<-labs[i]
      anova[[.name]]<-vardata
    }
    ### make fix dependending of the type of model ###    
    class(anova)<-c(paste0("simple_",obj$options$modelSelection),class(anova))
    anova<-mf.fixTable(anova)
    
    ### make sure they are not factors or stuff    
    for (.name in term64) 
         anova[[.name]]<-as.character(anova[[.name]])

  ginfo("End of Simple Effects")
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


## this is hubly supercool ###
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
      params<-list()
      anovas<-list()
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
            
            results<-try_hard(emmeans::emtrends(obj$model,specs = term64, var = variable64, at = conditions))
            obj$warnings<-list(topic="simpleInteractions",message=results$warning)
            obj$errors<-results$error
            emgrid<-results$obj 
            .names<-setdiff(term64,mods)
            ### we need the interaction contrast to be in obj because it should count the times
            ## it is called to know which variable should be contrasted
            .datamatic<-rev(sapply(.names, function(.name) obj$datamatic$variables[[.name]]))
            opts<-list(object=emgrid,by=mods,interaction=list(obj$interaction_contrast),datamatic=.datamatic)
            results<-try_hard(do.call(emmeans::contrast,opts))
            obj$warnings<-list(topic="simpleInteractions",message=results$warning)
            obj$errors<-results$error
            resgrid<-results$obj 
            
            res<-as.data.frame(resgrid)
            names(res)[(ncol(res)-4):ncol(res)]<-c("estimate","se","df","t","p")
            res$focal<-variable
            names(res)[1:length(.names)]<-.names
            labnames<-c("focal",.names)
            res$effect<-apply(res[,labnames],1,jmvcore::stringifyTerm)
            
            for (.name in mods) {
               res[[.name]]<-as.character(res[[.name]])
            }
            
            params[[length(params)+1]]<-res
            res<-emmeans::test(resgrid,by=mods,join=T)
            names(res)[(ncol(res)-3):ncol(res)]<-c("df1","df2","f","p")
            for (.name in mods) {
              res[[.name]]<-as.character(res[[.name]])
            }
            
            anovas[[length(anovas)+1]]<-res
            j<-j-1
        }
      ginfo("simple Interactions ended")
      
      return(list(params,anovas))
}

  

  


