.merformula.getRandom<-function(lformula) {
  fb<-findbars(form)
  .terms<-lapply(fb,function(f) 
    trimws(strsplit(as.character(unlist(tt))[[2]],"+",fixed = T)[[1]])
  )  
  .clusters<-lapply(fb,function(f) 
    trimws(strsplit(as.character(unlist(tt))[[3]],"+",fixed = T)[[1]])
  )  
  lapply(1:length(fb),function(i){
   structure(list(terms=.terms[[i]],cluster=.clusters[[i]])) 
  })  
}

merFormula<-function(formula) {
  print(lme4::nobars(formula)[[3]])
structure(list(formula=formula,
                      dep=all.vars(form)[[1]],
                      random=.merformula.getRandom(formula),
                      fixed=lme4::nobars(formula)[[3]]),
                 class="merFormula")
}
formula(form,rhs=F)
form<-y~(1+x1+x2+x2:x2+x3+x3:x2|cluster)+(0+x1|cluster)+x1+x2

(mformula<-merFormula(form))
fixterms<- function(x,...) UseMethod(".fixterms")

.fixterms.default<-function(formula) {
  warning('no applicable method for "fixterms" applied to an object of class "$s"',class(formula))
}
.fixterms.merFormula<-function(mformula) {
  lapply(mformula$fixed,identity)
}

ranterms.merFormula<-function(mformula) {
  lapply(mformula$random,identity)
}

ranterms<- function(x,...) UseMethod(".ranterms")

.ranterms.default<-function(formula) {
  warning('no applicable method for "fixterms" applied to an object of class "$s"',class(formula))
}
.ranterms.merFormula<-function(mformula) {
  lapply(mformula$random,function(a) lapply(a,function(b) strsplit(b,":",fixed=T)))
}

term="x1"
drop.terms.merFormula<-function(term,which="all",type="variable") {
}


fixterms(mformula)

ranterms(mformula)

data("wicksell")
data<-wicksell
data$group<-factor(data$group)
data$time<-factor(data$time)
mod<-gamlj::gamljMixed(
  formula = dv ~ 1 + group + time + group:time+( 1+group+group:time | subj )+( 0+time | subj ),
  data = data
 )


car::Anova(mod1)

a<-list(
  list(
    c("Intercept","sub"),
    c("x","sub"),
    c("x","x","sub"),
    c("x","z","sub")),
  list(
  c("Intercept","sub2"),
  c("x","sub2"),
  c("x","z","sub2")
  )
)

clean<-lapply(a,function(one) {
  .terms<-lapply(one,function(x) paste(head(x,-1),collapse = ":"))
})
remove<-lapply(clean,function(one) {
  .terms<-.terms[.terms!="Intercept"]
  .terms<-lapply(.terms,paste,collapse = ":")
  res<-lapply(.terms,function(t) length(grep(t,.terms,fixed=T))==1)
  .terms[unlist(res)]
})
remove
for (i in seq_along(remove)) {
  keep<-sapply(remove[[i]], function(.term){
    clean[[i]]==.term
  })
  print(keep)
  a[[i]][keep]<-NULL
  print(a)
}
