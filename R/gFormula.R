## an object to obtain formulas in different contexts from lists of terms
#source("R/functions.R")
gFormula <- R6::R6Class(
  "gFormula",
  class=TRUE, 
  cloneable=FALSE, 
  public=list(
    dep=NULL,
    offset=NULL,
    clusters=NULL,
    nested_intercept=TRUE,
    nested_random=NULL,
    fixed_intercept=TRUE,
    hasTerms=FALSE,
    isProper=FALSE,
    anova_terms=NULL,
    params_terms=NULL,
    lhs=function() {
      private$.buildfixed(NULL,private$.fixed)
    },
    lhs64=function() {
      private$.buildfixed(tob64(private$.fixed))
    },
    fixed_formula=function() {
      private$.buildfixed(self$dep,self$fixed)
    },
    fixed_formula64=function() {
      private$.buildfixed(tob64(self$dep),tob64(self$fixed))
    },
    random_formula=function() {
        private$.buildrandom(self$random,self$random_corr,"plain")
    },
    random_formula64=function() {
      private$.buildrandom(self$random,self$random_corr,"b64")
    },
    formula=function() {
      paste(self$fixed_formula(),self$random_formula())
    },
    formula64=function() {
      paste(self$fixed_formula64(),self$random_formula64())
    },
    nested_fixed_formula=function() {
      
      if (is.null(self$nested_fixed))
         return(NULL)
      
      private$.buildfixed(self$dep,self$nested_fixed)
    },    
    nested_fixed_formula64=function() {
      private$.buildfixed(tob64(self$dep),tob64(self$nested_fixed))
    },    
    
    nested_random_formula=function() {
      private$.buildrandom(self$nested_random,"block","plain")
    },    
    nested_random_formula64=function() {
      private$.buildrandom(self$nested_random,"block","b64")
    },    
    nested_formula=function() {
      if (is.null(self$nested_fixed))
        return(NULL)
      paste(self$nested_fixed_formula(),self$nested_random_formula())
    },
    nested_formula64=function() {
      paste(self$nested_fixed_formula(),self$nested_random_formula())
    },
    nested_tested_fixed=function() {
       if (is.something(private$.nested_fixed))
        return(private$.buildfixed(self$dep,setdiff(private$.fixed,private$.nested_fixed)))
    },
    nested_tested_random=function() {
      if (is.something(private$.nested_fixed))
        return(private$.buildfixed(NULL,setdiff(unlist(self$random),unlist(self$nested_random))))
    },
    keep=function(term) {
      
      w<-unlist(lapply(private$.fixed,function(x) (all(term==x) || is.numeric(x))))
      fixed=private$.fixed[w]
      f<-private$.buildfixed(tob64(self$dep),tob64(fixed))
      r<-NULL
      if (is.something(self$random)) {
        alist<-lapply(self$random,function(z) lapply(z,function(x) {
          w<-all(term==x[-length(x)] ) || x[-length(x)]=="Intercept"
          if (isFALSE(w)) return(NULL) else return(x)
        } ))
      r<-private$.buildrandom(clean_lol(alist),self$random_corr,"b64")
      }
      paste(f,r)
    },
    update_terms=function(data) {
      .formulalist<-self$fixed
      ## we want to be sure that the order of terms is the same used by the estimator
      ## because in R it may arrive a formula like y~x:z+z+x, which would processed by
      ## lm() (or other estimator) in different order
      .formula<-jmvcore::composeFormula(NULL,.formulalist)
      .formula<-attr(terms(as.formula(.formula)),"term.labels")
      .formulalist<-jmvcore::decomposeTerms(.formula)
       self$anova_terms<-fromb64(.formulalist)
       self$params_terms<-fromb64(colnames(model.matrix(as.formula(self$fixed_formula64()),data)))

       
      
      
    }

  ), #end of public
  active=list(
    fixed=function(alist) {
      if (missing(alist))
         return(private$.fixed)
      
      aOne<-which(unlist(alist)=="1")
      if (is.something(aOne)) {
        alist[[aOne]]<-NULL
        self$fixed_intercept<-TRUE
      } 
      aZero<-which(unlist(alist)=="0")
      if (is.something(aZero)) {
        alist[[aZero]]<-NULL
        self$fixed_intercept<-FALSE
      } 
      alist<-c(as.numeric(self$fixed_intercept),alist)
      self$hasTerms<-length(alist)>1
      self$isProper <-(self$fixed_intercept | self$hasTerms)
      private$.fixed<-alist
    },
    nested_fixed=function(alist) {

      if (missing(alist))
        return(private$.nested_fixed)
      
      aOne<-which(unlist(alist)=="1")
      if (is.something(aOne)) {
        alist[[aOne]]<-NULL
        self$nested_intercept<-TRUE
      } 
      aZero<-which(unlist(alist)=="0")
      if (is.something(aZero)) {
        alist[[aZero]]<-NULL
        self$nested_intercept<-FALSE
      } 
      private$.nested_fixed<-c(as.numeric(self$nested_intercept),alist)
    },
    random=function(alist) {
      
      if (missing(alist))
        return(private$.random)
      
      self$clusters<-unique(unlist(lapply(alist,function(z) lapply(z,function(x) x[length(x)]))))
      private$.random<-alist
    },
    
    random_corr=function(avalue) {
      if (missing(avalue))
         return(private$.random_corr)
      if (is.null(avalue))
         return(avalue)
      private$.random_corr=avalue
    }

  ), #end of active
  private = list(
    .fixed=NULL,
    .random=NULL,
    .nested_fixed=NULL,
    .random_corr="all",
    .buildfixed=function(dep,terms) {
      gsub("`0`",0,gsub("`1`",1,jmvcore::composeFormula(dep,terms)))
    },
    .buildrandom=function(terms,correl,encoding) {
      
      if (!is.something(terms))
         return()
      
        ## this is for R. It overrides the re_corr option 
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
          
  #        self$clusters<-c(self$clusters,unique(res[,2]))
          
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
        rterms<-trimws(paste(rterms,collapse = ""))
        return(rterms)
      
    }
  ) #end of private
) #end of class



