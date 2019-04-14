gsimple.init<-function(data,options,tables, n64=NULL, cov_condition=NULL) {

  ginfo("Init simple effects")
  dep<-options$dep
  variable<-options$simpleVariable
  moderator<-options$simpleModerator
  threeway<-options$simple3way
  interval<-options$paramCIWidth
  
  if (is.null(variable) | is.null(moderator)) 
     return()
    
    modelType<-"linear"
    if ("modelSelection" %in% names(options))
      modelType<-options$modelSelection

    ###### encode the names    
    dep64<-jmvcore::toB64(dep)
    variable64<-jmvcore::toB64(variable)
    moderator64<-jmvcore::toB64(moderator)
    threeway64<-jmvcore::toB64(threeway)
    
    #### collect the tables
    simpleEffectsAnova<-tables$Anova
    simpleEffectsParams<-tables$Params
    
        
    # determine dimensions of the table  
    xlevels<-length(levels(data[[variable64]]))
    xlevels<-ifelse(xlevels>1,(xlevels-1),1)
    
    modlevels<-length(levels(data[[moderator64]]))
    modlevels<-ifelse(modlevels>1,modlevels,3)
    
    ##### psteps decides when to separate the lines with extra space
    psteps<-ifelse(xlevels>1,xlevels,modlevels)
 
    ## check for special formats    
    rep<-1
    contrastlabels<-NULL
    labels<-NULL
    if (is.factor(data[[variable64]])) {
      labels<-lf.contrastLabels(levels(data[[variable64]]),attr(data[[variable64]],"jcontrast"))
    } 
    if (!is.null(threeway)) {
      threelevels<-length(levels(data[[threeway64]]))
      threelevels<-ifelse(threelevels>1,threelevels,3)
    } else 
      threelevels<-1
    
    moderatorLabels<-NULL
    
    if (modelType=="multinomial") {
      simpleEffectsParams$getColumn('dep')$setTitle(dep)
      rep<-length(levels(data[[dep64]]))-1
      psteps<-rep
    }

    arows<-modlevels*threelevels
    prows<-xlevels*modlevels*threelevels*rep
    
    # create Anova Table with right titles and rows
    title<-paste( "Simple effects of",variable,": Omnibus Tests")
    simpleEffectsAnova$setTitle(title)
    simpleEffectsAnova$getColumn('moderator')$setTitle(moderator)
    simpleEffectsAnova$getColumn('moderator')$setSuperTitle("Moderator levels")
    
    if (!is.null(threeway)) {
      simpleEffectsAnova$getColumn('threeway')$setTitle(threeway)
      simpleEffectsAnova$getColumn('threeway')$setSuperTitle("Moderator levels")
    }
    


    for (i in seq_len(arows)) {
      simpleEffectsAnova$addRow(rowKey=i)
      if ((i %% modlevels)==1) 
        simpleEffectsAnova$addFormat(rowKey=i, col=1, jmvcore::Cell.BEGIN_GROUP)
    }
    simpleEffectsAnova$setVisible(visible=TRUE)
    
    # create Params Table with right titles and rows
    title<-paste("Simple effects of",variable,": Parameter estimates")
    simpleEffectsParams$setTitle(title)
    
    simpleEffectsParams$getColumn('upper.CL')$setSuperTitle(jmvcore::format('{}% Confidence Interval', interval))
    simpleEffectsParams$getColumn('lower.CL')$setSuperTitle(jmvcore::format('{}% Confidence Interval', interval))
    simpleEffectsParams$getColumn('moderator')$setTitle(moderator)
    simpleEffectsParams$getColumn('moderator')$setSuperTitle("Moderator levels")

    if (!is.null(threeway)) {
      simpleEffectsParams$getColumn('threeway')$setTitle(threeway)
      simpleEffectsParams$getColumn('threeway')$setSuperTitle("Moderator levels")
    }
    if (is.factor(data[[variable64]])) 
          contrastlabels<-rep(labels,modlevels*threelevels*rep)

    for (i in seq_len(prows)) {
      simpleEffectsParams$addRow(rowKey=i,list(contrast=contrastlabels[[i]]))
      if ((i %% psteps)==1) {
        simpleEffectsParams$addFormat(rowKey=i,col=1, jmvcore::Cell.BEGIN_GROUP)
        simpleEffectsParams$addFormat(rowKey=i,col=2, jmvcore::Cell.BEGIN_GROUP)
      }
    }
    if (!is.factor(data[[variable64]]) ) {
     simpleEffectsParams$getColumn('contrast')$setVisible(FALSE)
    }
    
  
    simpleEffectsParams$setVisible(visible=TRUE)
  
}  # end of simple effects tables


gsimple.populate<-function(model,options,tables,cov_conditioning) {

        ginfo("Populate simple effects")
  
        variable<-options$simpleVariable
        moderator<-options$simpleModerator
        threeway<-options$simple3way
        interval<-options$paramCIWidth
  
        if (is.null(variable) | is.null(moderator)) 
           return()
  
  ### collect the tables
  
       anovaTable<-tables$Anova
       parametersTable<-tables$Params
  
  #### check if estimation is needed
       if (!is.null(anovaTable$state)) {
            ginfo("simple effects have been recycled")
            anovaTableData<-anovaTable$state
            parametersTableData<-parametersTable$state
       } else {
             simple_test <- try({
                 resultsTables<-pred.simpleEstimates(model,
                                   jmvcore::toB64(variable),jmvcore::toB64(moderator),jmvcore::toB64(threeway),
                                   cov_conditioning=cov_conditioning,
                                   interval=interval) 
             })
             if (jmvcore::isError(simple_test)) {
                 anovaTable$setNote("se.noluck",WARNS[["se.noluck"]])
                 return()
             }
    
             anovaTableData<-resultsTables[[2]]
             anovaTable$setState(anovaTableData)
             parametersTableData<-resultsTables[[1]]  
             parametersTable$setState(parametersTableData)
             ginfo("simple effects have been computed")

       }

   ### fill the Anova Table ###
        for(r in seq_len(nrow(anovaTableData))) {
              anovaTable$setRow(rowNo=r,anovaTableData[r,])
        }
        parametersTableData$contrast<-NULL
        for(r in seq_len(nrow(parametersTableData))) {
             parametersTable$setRow(rowNo=r,parametersTableData[r,])
        }
       
       ### add some note if necessary
       term<-mi.interaction.term(model,c(variable,moderator,threeway))
       if (!is.null(term)) {
         if (mi.is.scaleDependent(model,term))
           parametersTable$setNote("inter",WARNS["se.interactions"])
         else if (mi.term.develop(term)<length(options$modelTerms))
           parametersTable$setNote("covs",WARNS["se.covariates"])
       } else 
         parametersTable$setNote("noint",WARNS["se.noint"])
}

