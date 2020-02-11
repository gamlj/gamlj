#########à fix tables notes ############

out.infotable_footnotes<-function(table,info) {
  if (info$conv==FALSE) {
    table$setNote("conv",WARNS["noconv"])
  }
  
  if (info$aliased==TRUE) {
    table$setNote("aliased",WARNS["ano.aliased"])
  }
  if (info$singular==TRUE) {
    table$setNote("singular",WARNS["lmer.singular"])
  }
  
  return(table)
}


out.table_notes<-function(table) {
  warns<-table$state$warning
  for (w in warns)
    table$setNote(w,w)
}

######  fill the table ########
out.fillTable<-function(table,params) {
    if (nrow(params)==0)
        return()
    for (i in 1:nrow(params)) {
      tableRow=params[i,]
      table$setRow(rowNo=i,tableRow)
    }
}
