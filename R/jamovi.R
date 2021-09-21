tob64<- function(x,...) UseMethod(".tob64")

.tob64.default<-function(obj,ref=NULL) {


  if (is.null(obj))
      return()


  if (is.null(ref)) {
    obj<-jmvcore::toB64(obj)
  } else {
    for (r in ref) {
      reg<-paste0("(?<=[\\s*~=]|^)",r,"(?=[\\s*~=]|$)")
      obj<-stringr::str_replace_all(obj,reg,jmvcore::toB64(r))
    }
  }
  obj
}

.tob64.list<-function(obj,ref=NULL) {
  lapply(obj,bogustob64,ref)
}

bogustob64<-function(obj,ref) tob64(obj,ref)

fromb64<- function(x,...) UseMethod(".fromb64")


.fromb64.default<-function(obj,ref=NULL) {
  
  if (!is.something(obj))
      return(obj)
  
  if (length(obj)>1)
      return(unlist(sapply(obj, bogusfromb64,ref=ref,USE.NAMES = F)))
  
   int<-strsplit(obj,INTERACTION_SYMBOL,fixed=T)[[1]]
   if (length(int)>1)
         return(paste0(unlist(sapply(int,bogusfromb64,ref=ref)),collapse = ":"))

    if (is.null(ref)) {
      fac<-strsplit(obj,FACTOR_SYMBOL,fixed=T)[[1]]
      obj<-fac[[1]]  
      obj<-jmvcore::fromB64(obj)
      if (length(fac)>1) obj<-paste0(obj,fac[[2]])
  } else {
    for (r in ref) 
          obj<-stringr::str_replace_all(obj,paste0("\\b",jmvcore::toB64(r),"\\b"),r)

  }
   obj<-gsub(LEVEL_SYMBOL,"",obj,fixed = T)
   obj<-gsub(FACTOR_SYMBOL,"",obj,fixed = T)
   obj<-gsub(INTERACTION_SYMBOL,":",obj,fixed = T)
   
  obj
}

.fromb64.list<-function(obj,ref=NULL) {
  lapply(obj,bogusfromb64,ref=ref)
}

bogusfromb64<-function(obj,ref=NULL) fromb64(obj,ref=ref)

######### tables #########

j.expand_table<-function(table,alist,type="text", keys=alist,superTitle=NULL,append=FALSE,names64=TRUE,startAt=1) {
 
  if (append) {
    startAt<-length(table$columns)
  }
  if (names64)    keys<-tob64(keys) else  keys<-make.names(keys,unique = T)
  labels<-alist
  
  startAt<-startAt-1
  for (i in seq_along(keys)) {
    j<-startAt+i
    cb <- (i<length(keys)  | length(keys)==1) 
    if (keys[[i]]=="vs") cb=FALSE
    table$addColumn(name = keys[[i]], title = labels[[i]], index = j, superTitle = superTitle, type=type, combineBelow=cb)
  }


  
}
  


j.init_table<-function(table,
                       obj,
                       ci=FALSE,
                       ciroot="",
                       ciformat="{}% Confidence Intervals",
                       ciwidth,
                       indent=NULL,
                       spaceby=NULL,
                       title=NULL) {

  if (ci) {
    l<-paste0(ciroot,"ci.lower")
    u<-paste0(ciroot,"ci.upper")
    table$getColumn(l)$setSuperTitle(jmvcore::format(ciformat, ciwidth))
    table$getColumn(u)$setSuperTitle(jmvcore::format(ciformat, ciwidth))
  }
  if (is.null(obj))
    return()
  
  square<-length(dim(obj))>1
  
  if (!is.logical(obj))
        j.fill_table(table,obj,append=T,spaceby=spaceby)
  
  if (is.something(title))
    table$setTitle(title)
  
  
  table$setVisible(TRUE)

}

j.init_table_append<-function(table,obj, indent=NULL) {
  
  last<-table$rowCount
  for (i in seq_along(obj)) 
    table$addRow(rowKey=last+i,obj[[i]])
  
  if (!is.null(indent)) {
    trows<-1:i
    rows<-trows[indent]
    for (j in rows)
      table$addFormat(rowKey=last+j,col=1,jmvcore::Cell.INDENTED)
  }
  
}


j.fill_table<-function(table,obj, fixNA=TRUE,append=FALSE,spaceby=NULL,start=1) {

  if (!is.something(obj))
    return()


  last<-start-1
  if (append)  last<-table$rowCount
  
  maxrow<-table$rowCount

  insert<-function(i,w) {
    if (i>maxrow)
        table$addRow(rowKey=i,w)
    else
        table$setRow(rowNo=i,w)
  }

  if (inherits(obj,"data.frame")) 
      obj<-lapply(1:nrow(obj),function(i) obj[i,])
  
   onames<-unlist(unique(lapply(obj,names)))
  

   for (i in seq_along(obj)) {
              t<-obj[[i]]
              if (fixNA) 
                  t[which(is.na(t))]<-""
              insert(i+last,t)
   }
  if (is.numeric(spaceby)) 
             spaceby<-onames[spaceby]
   
  if ("..space.." %in% onames)
              spaceby<-"..space.."
   
  if (is.something(spaceby)) {

    col<-lapply(obj,function(x) x[[spaceby]])
    rows<-unlist(lapply(unlist(unique(col)),function(x) min(which(col==x))))

    for (j in rows)
      table$addFormat(rowNo=j+last,col=1,jmvcore::Cell.BEGIN_GROUP)
    
  }
  table$setState("stuffed")
  table$setVisible(TRUE)
}

j.add_warnings<-function(atable,adispatch,atopic,reset=FALSE) {

  if (reset) {
    for (a in atable$notes) 
       a$note<-NULL
    
  }
  if (!is.something(adispatch$warnings[[atopic]]) & !is.something(adispatch$errors[[atopic]]))
         return()
  
  if (is.something(adispatch$errors[[atopic]])) 
            atable$setError(paste(adispatch$errors[[atopic]],collapse = "; "))
  
  if (inherits(atable,"Html"))
          atable$setContent(paste(adispatch$warnings[[atopic]],collapse = "; "))
  else
      for (i in seq_along(adispatch$warnings[[atopic]])) 
               atable$setNote(i,adispatch$warnings[[atopic]][[i]])

  atable$setVisible(TRUE)
  
}
