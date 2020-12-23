

gposthoc.init=function(data,options,tables) {

  ginfo("Init posthoc...")
  bs <- options$factors
  phTerms <- options$postHoc
  modelType<-"linear"
  if ("modelSelection" %in% names(options))
    modelType<-options$modelSelection
  
  
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
  diff_label="Difference"
  
  if (modelType %in% c('logistic','poisson','poiover','nb' )) {
    diff_label="exp(B)"
  }

  
  
  for (j in seq_len(nDepLevels))
    for (ph in phTerms) {
      table <- tables$get(key=ph)
      table$setTitle(paste0('Post Hoc Comparisons - ',  jmvcore::stringifyTerm(ph)))
      nc<-0 
      ##### set the columns ###########
      for (i in seq_along(ph)) {
        nc<-nc+1
        table$addColumn(name=paste0("c",nc), title=ph[i], type='text', superTitle='Comparison', combineBelow=FALSE,index=nc+off)
      }
      nc<-nc+1
      table$addColumn(name='sep', title='', type='text', content='-', superTitle='Comparison', format='narrow',index=nc+off)
      
      for (i in seq_along(ph)) {
        nc<-nc+1
        table$addColumn(name=paste0("c",nc-1), title=ph[i], type='text', superTitle='Comparison',index=nc+off)
      }
      ##### fix the difference column label
      table$getColumn('estimate')$setTitle(diff_label)
      ##### set the rows ###########
      
      eg<-nrow(expand.grid(bsLevels[ph]))
      nRows<-(eg*(eg-1))/2
      for (i in seq_len(nRows)) {
        table$addRow(rowKey=((j*100)+i),list(sep="-"))
        if (modelType=='multinomial')
          table$setRow(rowKey=((j*100)+i),list(dep=depLevels[j]))
        #### make rows look nicer  ###
        if (i==1)
          table$addFormat(rowKey=((j*100)+i), col=1, jmvcore::Cell.BEGIN_GROUP)
        if (i==nRows)
          table$addFormat(rowKey=((j*100)+i), col=1, jmvcore::Cell.END_GROUP)
      }
    }
  ginfo("Init done...")
  
}


gposthoc.populate<-function(model,options,tables) {

  ginfo("Populate posthoc...")
  
  terms <- options$postHoc
  dep<-options$dep
  if (length(terms) == 0)
    return()
  
  postHocRows <- list()

  for (ph in terms) {
    
    table <- tables$get(key=ph)
    table$setState(list(1:10))
    term <- jmvcore::composeTerm(ph)
    termB64 <- jmvcore::composeTerm(jmvcore::toB64(ph))
    suppressWarnings({
      none <- .posthoc(model,termB64,"none")
      bonferroni <- .posthoc(model,termB64,"bonferroni")
      holm <-.posthoc(model,termB64,"holm")
      tukey <-.posthoc(model,termB64,"tukey")
      
    }) # suppressWarnings
    if (is.character(none))
      table$setNote("nojoy",WARNS["ph.nojoy"])
    else {        
      table$setState("there is work done")
      tableData<-as.data.frame(none)
      tableData$contrast<-as.character(tableData$contrast)
      colnames(tableData)<-c("contrast","estimate","se","df","test","p")
      tableData$pbonf<-bonferroni[,6]
      tableData$pholm<-holm[,6]
      tableData$ptukey<-tukey[,6]
    }

    .cont<-as.character(tableData$contrast)
    .cont<-gsub(" - ","-",.cont,fixed = T)
    .cont<-gsub(" / ","/",.cont,fixed = T)
    
    .labs64<-sapply(.cont, function(a) {
      sapply(strsplit(as.character(a),"[- ,/]"),trimws,USE.NAMES = F,simplify = F)
    })
    labs64<-do.call("rbind",.labs64)
    colnames(labs64)<-paste0("c",1:ncol(labs64))
    if ("df" %in% names(tableData))
        if (tableData$df[1]==Inf) {
           table$getColumn("test")$setTitle("z")
           table$getColumn("df")$setVisible(FALSE)
        }
    for (i in 1:nrow(tableData)) {
      row<-tableData[i,]
      l<-jmvcore::fromB64(labs64[i,])
      table$setRow(rowNo=i, values=c(row,l))
    }
  }
  ginfo("Populate posthoc done")
  
}


###### post hoc ##########
.posthoc<- function(x,...) UseMethod(".posthoc")

.posthoc.default<-function(model,term,adjust) {
  #  term<-jmvcore::composeTerm(term)
  termf<-stats::as.formula(paste("~",term))
  data<-mf.getModelData(model)
  referenceGrid<-emmeans::emmeans(model, termf,type = "response",data=data)
  terms<-jmvcore::decomposeTerm(term)
  labs<-referenceGrid@grid[terms]
  newlabs<-sapply(labs, function(a) sapply(a, function(b) jmvcore::toB64(as.character(b))))
  referenceGrid@grid[terms]<-newlabs  
  table<-summary(graphics::pairs(referenceGrid),adjust=adjust)
  table[order(table$contrast),]
}

.posthoc.multinom<-function(model,term,adjust) {
  results<-try({
    dep<-names(attr(stats::terms(model),"dataClass"))[1]
    dep<-jmvcore::composeTerm(dep)
    tterm<-stats::as.formula(paste("~",paste(dep,term,sep = "|")))  
    data<-mf.getModelData(model)
    referenceGrid<-emmeans::emmeans(model,tterm,transform = "response",data=data)
    terms<-jmvcore::decomposeTerm(term)
    labs<-referenceGrid@grid[terms]
    newlabs<-sapply(labs, function(a) sapply(a, function(b) jmvcore::toB64(as.character(b))))
    referenceGrid@grid[terms]<-newlabs  
    res<-summary(graphics::pairs(referenceGrid, by=dep, adjust=adjust))
    res<-as.data.frame(res)
    res[,dep]<-NULL
    res
  })
  
  return(results)
}


