library(yaml)
ayaml<-read_yaml("jamovi/gamljglm.a.yaml")
opts<-ayaml$options
opts<-sapply(seq_along(opts),function(x) {opts[[x]]$name})

ryaml<-read_yaml("jamovi/gamljglm.r.yaml")
items<-ryaml$items

print_item<-function(item,name=NULL) {
  
  cw<-item$clearWith
  who<-unlist(c(name,item$name))
  cat("##",who,"\n")
  test<-(cw %in% opts)
  print(paste(cw,test,sep="="))
  if (any(isFALSE(test)))
     warning(who,"probles")
  if (!is.null(item$visible)) {
    vis<-item$visible
    vis<-gsub("(","",vis,fixed = T)
    vis<-gsub(")","",vis,fixed = T)
    print(paste(vis,(vis %in% opts),sep="="))
  }
  
  
  cat("\n\n")
  
  
}

check<-function(item,name=NULL) {

  if (item$type=="Group") {
    print_item(item,name)   
    for (i in seq_along(item$items)) 
           check(item$items[[i]],name=item$name)
    return()
  }
  if (item$type=="Array") {
      check(item$template,name=item$name)
    return()
  }
  print_item(item)
  
  if (hasName(item,"columns")) {
   
    for (col in item$columns) {
        if (!is.null(col$visible))
          cat(col$name,col$visible,"\n")
    }
  }
    
}


for (a in seq_along(items)) {
  check(items[[a]])
}


       