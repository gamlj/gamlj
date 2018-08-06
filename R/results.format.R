rf.initPostHoc=function(data,options, tables,modelType="linear") {
  
  bs <- options$factors
  phTerms <- options$postHoc
  
  bsLevels <- list()
  for (i in seq_along(bs))
    bsLevels[[bs[i]]] <- levels(data[[jmvcore::toB64(bs[i])]])
  
  nDepLevels<-1
  off<-0
  if (modelType=='multinomial') {
    dep<-options$dep
    depLevels<-levels(data[[jmvcore::toB64(dep)]])
    nDepLevels<-length(depLevels)
    off<-1
  }
  for (j in seq_len(nDepLevels))
    for (ph in phTerms) {
      table <- tables$get(key=ph)
      table$setTitle(paste0('Post Hoc Comparisons - ',  jmvcore::stringifyTerm(ph)))
      test<-ifelse(modelType=="linear","t","z")
      table$getColumn('test')$setTitle(test)
      
      nc<-0 
      ##### set the columns ###########
      for (i in seq_along(ph)) {
        nc<-nc+1
        table$addColumn(name=paste0("c",nc), title=ph[i], type='text', superTitle='Comparison', combineBelow=TRUE,index=nc+off)
      }
      nc<-nc+1
      table$addColumn(name='sep', title='', type='text', content='-', superTitle='Comparison', format='narrow',index=nc+off)
      
      for (i in seq_along(ph)) {
        nc<-nc+1
        table$addColumn(name=paste0("c",nc-1), title=ph[i], type='text', superTitle='Comparison',index=nc+off)
      }
      ##### set the rows ###########
      
      eg<-nrow(expand.grid(bsLevels[ph]))
      nRows<-(eg*(eg-1))/2
      for (i in seq_len(nRows)) {
        table$addRow(rowKey=((j*10)+i),list(sep="-"))
        if (modelType=='multinomial')
          table$setRow(rowKey=((j*10)+i),list(dep=depLevels[j]))
        #### make rows look nicer  ###
        if (i==1)
          table$addFormat(rowKey=((j*10)+i), col=1, jmvcore::Cell.BEGIN_GROUP)
        if (i==nRows)
          table$addFormat(rowKey=((j*10)+i), col=1, jmvcore::Cell.END_GROUP)
      }
    }
#  return(tables)
}


rf.initContrastCode<-function(data,options,results,n64) {

  if (!options$showContrastCode) 
     return()
  
  factorsAvailable <- options$factors
  if (length(factorsAvailable)==0)
    return()
  tables<-results$contrastCodeTables
  for (fac in factorsAvailable) {
    rnames<-n64$nicenames(n64$contrasts(fac))
    clabs<-n64$contrastsLabels(fac)
    aTable<-tables$addItem(key=fac)
    codes<-round(t(contrasts(data[[jmvcore::toB64(fac)]])),digit=3)
    cnames<-colnames(codes)
    colnames(codes)<-paste0("c",1:length(cnames))
    codes<-cbind(rnames,clabs,codes)
    for (i in seq_along(cnames)) {
       aTable$addColumn(name=paste0("c",i), title=paste0("level=",cnames[i]), type='text')
    }
    for (i in 1:nrow(codes)) {
      aTable$addRow(rowKey=i, values=codes[i,])
    }
  }  
  
}
rf.initEMeans<-function(data,options,theTables) {
  
  interval<-options$paramCIWidth
  if (options$eDesc) {
    factorsAvailable <- options$factors
    modelTerms<- options$modelTerms
    if (length(factorsAvailable) == 0)
      return()
    for (term in modelTerms)
      if (all(term %in% factorsAvailable)) {
        aTable<-theTables$addItem(key=.nicifyTerms(jmvcore::composeTerm(term)))
        aTable$getColumn('upper.CL')$setSuperTitle(jmvcore::format('{}% Confidence Interval', interval))
        aTable$getColumn('lower.CL')$setSuperTitle(jmvcore::format('{}% Confidence Interval', interval))
        
        ll <- sapply(jmvcore::toB64(term), function(a) base::levels(data[[a]]), simplify=F)
        ll$stringsAsFactors <- FALSE
        grid <- do.call(base::expand.grid, ll)
        grid <- as.data.frame(grid,stringsAsFactors=F)
        for (i in seq_len(ncol(grid))) {
          colName <- jmvcore::fromB64(colnames(grid)[[i]])
          aTable$addColumn(name=colName, title=term[i], index=i)
        }
        for (rowNo in seq_len(nrow(grid))) {
          row <- as.data.frame(grid[rowNo,],stringsAsFactors=F)
          colnames(row)<-term
          aTable$addRow(rowKey=row, values=row)
        }
      }
    return(theTables)
  } # end of  means
  
  
  
}

rf.initSimpleEffects<-function(data,options,results) {

variable<-options$simpleVariable
moderator<-options$simpleModerator
threeway<-options$simple3way
interval<-options$paramCIWidth

if (!is.null(variable) & !is.null(moderator)) {
  
  variable64<-jmvcore::toB64(variable)
  moderator64<-jmvcore::toB64(moderator)
  threeway64<-jmvcore::toB64(threeway)

  # determine dimensions of the table  
  xlevels<-length(levels(data[[variable64]]))
  xlevels<-ifelse(xlevels>1,(xlevels-1),1)
  
  modlevels<-length(levels(data[[moderator64]]))
  modlevels<-ifelse(modlevels>1,modlevels,3)
  
  psteps<-ifelse(xlevels>1,xlevels,modlevels)
  
  if (!is.null(threeway)) {
       threelevels<-length(levels(data[[threeway64]]))
       threelevels<-ifelse(threelevels>1,threelevels,3)
  } else 
    threelevels<-1
  
  arows<-modlevels*threelevels
  prows<-xlevels*modlevels*threelevels

  # create Anova Table with right titles and rows
  simpleEffectsAnova<-results$simpleEffects$Anova
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
simpleEffectsParams<-results$simpleEffects$Params
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
for (i in seq_len(prows)) {
  simpleEffectsParams$addRow(rowKey=i)
  if ((i %% psteps)==1) {
     simpleEffectsParams$addFormat(rowKey=i,col=1, jmvcore::Cell.BEGIN_GROUP)
     simpleEffectsParams$addFormat(rowKey=i,col=2, jmvcore::Cell.BEGIN_GROUP)
  }
}
if (!is.factor(data[[variable64]]))
   simpleEffectsParams$getColumn('contrast')$setVisible(FALSE)


simpleEffectsParams$setVisible(visible=TRUE)
} 
}  # end of simple effects tables