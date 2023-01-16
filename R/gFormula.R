## an object to obtain formulas in different contexts from lists of terms
#source("R/functions.R")
gFormula <- R6::R6Class(
  "gFormula",
  class=TRUE, 
  cloneable=FALSE, 
  public=list(
    dep=NULL,
    random=NULL,
    random_corr=NULL,
    offset=NULL,
    clusters=NULL,
    hasIntercept=TRUE,
    isProper=FALSE,
    lhs=function() {
      jmvcore::composeFormula(NULL,private$.fixed)
    },
    lhs64=function() {
      jmvcore::composeFormula(NULL,tob64(private$.fixed))
    },
    fixed_formula=function() {
      jmvcore::composeFormula(self$dep,private$.fixed)
    },
    fixed_formula64=function() {
      jmvcore::composeFormula(tob64(self$dep),tob64(private$.fixed))
    },
    random_formula=function() {
        private$.buildrandom("plain")
    },
    random_formula64=function() {
      private$.buildrandom("b64")
    },
    
    pretty=function(){
      .formula<-attr(terms(as.formula(self$formula())),"term.labels")
      .formulalist<-jmvcore::decomposeTerms(.formula)
      lapply(.formulalist, function(x) list(source=jmvcore::stringifyTerm(x,raise=T)))
    },
    formula=function() {
      paste(self$fixed_formula(),self$random_formula())
    },
    formula64=function() {
      paste(self$fixed_formula64(),self$random_formula64())
    }
    

  ), #end of public
  active=list(
    fixed=function(alist) {
      if (missing(alist))
         return(private$.fixed)
      
      aOne<-which(unlist(alist)=="1")
      
      if (is.something(aOne)) {
        alist[[aOne]]<-NULL
        self$hasIntercept<-TRUE
      }
      private$.fixed<-alist
    }

  ), #end of active
  private = list(
    .fixed=NULL,
    .buildrandom=function(encoding) {
      
      if (is.null(self$random))
         return()
      
        terms<-self$random
        
        ## this is for R. It overrides the re_corr option 
        correl<-self$random_corr
        if (length(terms)>1)
          correl  <-  "block"
        # remove empty sublists
        terms <- terms[sapply(terms, function(a) !is.null(unlist(a)))]
        
        # split in sublists if option re_corr=none
        if (correl=="none") {
          termslist<-terms[[1]]
          terms<-lapply(termslist,list)
        }
        
        rterms<-""
        .intercept<-"Intercept"
        for(i in seq_along(terms)) {
          .one<-terms[[i]]
          if (encoding=="b64") {
             .one<-tob64(.one)
             .intercept<-tob64(.intercept)
          }
          flatterms<-lapply(.one,function(x) c(jmvcore::composeTerm(head(x,-1)),tail(x,1)))
          res<-do.call("rbind",flatterms)
          ### check blocks coherence
          if (length(unique(res[,2]))>1 && correl=="block")
            stop("Correlated random effects by block should have the same cluster variable within each block. Please specify different blocks for random coefficients with different clusters.")
          
          self$clusters<-c(self$clusters,unique(res[,2]))
          
          res<-tapply(res[,1],res[,2],paste)
          res<-sapply(res, function(x) paste(x,collapse = " + "))
          
          ### deal with intercept ###
          for (i in seq_along(res)) {
            test<-grep(.intercept,res[[i]],fixed=TRUE)
            if (is.something(test))
              res[[i]]<-gsub(.intercept,1,res[[i]])
            else 
              res[[i]]<-paste("0 + ",res[[i]])
          }
          ### compose ####
          form<-paste(res,names(res),sep=" | ")
          form<-paste("(",form,")")
          rterms<-paste(rterms,form,sep = "+ ")
        }
        ## paste and return ``
        rterms<-paste(rterms,collapse = "")
        rterms
      
    }
  ) #end of private
) #end of class



