#########à fix tables notes ############

out.table_notes<-function(table) {
  warns<-table$state$warning
  for (w in warns)
    table$setNote(w,w)
}