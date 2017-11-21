rf.initPostHoc=function(data,options, tables,modelType="linear") {
  
  bs <- options$factors
  phTerms <- options$postHoc
  
  bsLevels <- list()
  for (i in seq_along(bs))
    bsLevels[[bs[i]]] <- levels(data[[bs[i]]])
  
  nDepLevels<-1
  off<-0
  if (modelType=='multinomial') {
    dep<-options$dep
    depLevels<-levels(data[[dep]])
    nDepLevels<-length(depLevels)
    off<-1
  }
  for (j in seq_len(nDepLevels))
    for (ph in phTerms) {
      table <- tables$get(key=ph)
      table$setTitle(paste0('Post Hoc Comparisons - ', stringifyTerm(ph)))
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
          table$addFormat(rowKey=((j*10)+i), col=1, Cell.BEGIN_GROUP)
        if (i==nRows)
          table$addFormat(rowKey=((j*10)+i), col=1, Cell.END_GROUP)
      }
    }
  return(tables)
}
