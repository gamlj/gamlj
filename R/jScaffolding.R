Scaffold <- R6::R6Class("Scaffold",
                          cloneable=FALSE,
                          class=FALSE,
                          dispatcher=NULL,
                          public=list(
                            name=NULL,
                            initialize=function(dispatcher=NULL) {
                              self$dispactcher<-dispatcher
                            }
                          )
) ## end of class
