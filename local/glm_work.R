data("wicksell")
data<-wicksell
oform<-y~x:z+x+q+x:q
formula64<-terms(as.formula(oform))
mynames64<-attr(terms(as.formula(formula64)),"term.labels")
mynames64

a<-letters[1:5]
a
c(paste("1",a,sep=":"),paste("2",a,sep=":"))


data$group<-factor(data$group)
contrasts(data$group)<-contr.sum(2)/2
data$time<-factor(data$time)
contrasts(data$time)<-contr.sum(4)
head(data)
model1<-lm(dv~1+time*group,data=data)
model<-model1
model0<-lm(dv~1+time+group,data=data)
model00<-lm(dv~1,data=data)
logLik(model00)
a<-anova(model00)
sigma(model1)^2*model1$df.residual
car::Anova(model1)
n<-dim(data)[1]
(SSE<-a$`Sum Sq`)

anova(model0,model1,test="LRT")

### this is how stata does, and you get the same as in r logLik()
-0.5*n*(log(2*pi) + log( SSE/n ) + 1)
sigma(model0)
an<-car::Anova(model1,type=3)
car::Anova(model0,type=3)
(l<-anova(model0,model1,test="LRT"))
(a<-anova(model0,model1,test="F"))

n
-0.5*n*(log(2*pi) + log( SSE/n ) + 1)
sss<-sigma(model1)^2*model1$df.residual
ll<--0.5*n*(log(2*pi) + log( sss/n ) + 1)
sumr<-summary(model1)
ssres<-sigma(model1)^2*model1$df.residual
dfres<-sumr$df[2]
sstot<-(sumr$fstatistic[[1]]*sumr$fstatistic[[2]]*ssres/dfres)+sstot
ss<-sstot+ssres
-0.5*n*(log(2*pi) + log( ss/n ) + 1)

logLik(model00)

2*(logLik(model1)-ll)
2*(logLik(model1)-logLik(model00))

drop1(model1,scope = ~1)
ll0<-logLik(model0)
ll1<-logLik(model1)
qchisq(round(l$`Pr(>Chi)`[2],digits = 14),3,lower.tail = F)


lmtest::lrtest(model0,model1)
2*(ll1-ll0)

f<-a$F[2]
chi<-b$Chisq[2]
chi/f
n<-dim(data)[1]
(n/2)*log(f)
chi/3
f
logLik(model1)
lmtest::lrtest(model1)
q<-as.numeric(2*(logLik(model1)-logLik(model0)))
pchisq(q,3,lower.tail = F)


parallelly::

model<-model0
(aa$`Sum of Sq`[2]/7)/(aa$RSS[2]/88)
summary(model)
as.data.frame(vcov(model))
R.version
parallel::detectCores()
if (.Platform$OS.type=="windows") parallel<-"snow" else parallel<-"multicore"
system.time(
.model<-parameters::bootstrap_model(model,parallel=parallel,n_cpus=8,iterations = 10000)
)
.ci_method<-"bcai"
packageVersion("parameters")
parameters::bootstrap_parameters(model)
amodel<-model

parameters::parameters(amodel,
                       standardize=NULL,
                       bootstrap=T,
                       ci_method=.ci_method,
                       ci=.95,
                       iterations=1000)


boot_fun<-function(data,indices,model=NULL) {
  .data<-data[indices,]
  .model<-stats::update(model,data=.data)
  .anova<-car::Anova(.model,type="III",singular.ok=T)
   atable<-as.data.frame(.anova[c(-1,-dim(.anova)[1]),])
   names(atable)<-c("ss","df","test","p")
   df<-atable$df
   dfres<-model$df.residual
   sumr<-summary(model)
   ssres<-sigma(model)^2*dfres
   ssmod<-sumr$fstatistic[[1]]*sumr$fstatistic[[2]]*ssres/dfres
   ss<-atable$ss
   unlist(c(ss,ssmod,ssres))

}


####
get_boot_ci<-function(effsize,terms,bres,type,df,dfres,N) {

fun<-switch(effsize,
       eta=function(ss,ssmod,ssres,df,dfe,N) ss/(ssmod+ssres),
       etap=function(ss,ssmod,ssres,df,dfe,N) ss/(ss+ssres),
       omega=function(ss,ssmod,ssres,df,dfe,N) (ss-(ssres*df/dfres))/(ssmod+(ssres*(dfres+1)/dfres)),
       omegap=function(ss,ssmod,ssres,df,dfe,N) (ss-(ssres*df/dfres))/(ss+(ssres*(N-df)/dfres)),
       epsilon=function(ss,ssmod,ssres,df,dfe,N) (ss-(ssres*df/dfres))/(ssmod+ssres),
       epsilonp=function(ss,ssmod,ssres,df,dfe,N) (ss-(ssres*df/dfres))/(ss+ssres),
       )


sterms<-seq_along(terms)
l<-length(bres$t0)
ss<-bres$t0[sterms]
ssmod<-bres$t0[l-1]
ssres<-bres$t0[l]
es  <- fun(ss,ssmod,ssres,df,dfe,N)
bres$t0[sterms]<-es

for (i in 1:nrow(bres$t)) {

  ss<-bres$t[i,sterms]
  ssmod<-bres$t[i,l-1]
  ssres<-bres$t[i,l]
  es  <- fun(ss,ssmod,ssres,df,dfe,N)
  bres$t[i,sterms]<-es
  
}

alist<-lapply(sterms, function(i) {
      r<-boot::boot.ci(bres,type=type,index=i)
      c(r$t0,r[[length(r)]][c(4:5)])
  })
res<-as.data.frame(do.call(rbind,alist))
names(res)<-c("es","est.ci.lower","est.ci.upper")
res$type<-effsize
res$effect<-terms
res
}
############
terms<-c("x","y","z")
N<-nrow(data)
one<-car::Anova(model)
df<-one$Df[-length(one$Df)]
dfe<-one$Df[length(one$Df)]

bres<-boot::boot(data,statistic = boot_fun,R = 1000,model=model)

get_boot_ci("eta",terms,bres,type="bca",df = df,dfres=dfe,N=N)
get_boot_ci("etap",terms,bres,type="perc",df = df,dfres=dfe,N=N)
get_boot_ci("omega",terms,bres,type="perc",df = df,dfres=dfe,N=N)
get_boot_ci("omegap",terms,bres,type="perc",df = df,dfres=dfe,N=N)
get_boot_ci("epsilon",terms,bres,type="perc",df = df,dfres=dfe,N=N)
get_boot_ci("epsilonp",terms,bres,type="perc",df = df,dfres=dfe,N=N)


.anova<-as.data.frame(car::Anova(model,type="III"))
.anova<-car::Anova(model,type="III")
.anova[!(rownames(.anova) %in% c("(Intercept)","Residuals")),]

obj<-list(ciwidth=.95)
ci_effectsize(b$es,df,dfres = dfe,obj)
do.call(rbind,res)

etaSqP <- SS/(SS+ssres)
omegaSq <- (SS-(ssres*df/dfres))/(ssmod+(ssres*(dfres+1)/dfres))
omegaSqP <- (SS-(ssres*df/dfres))/(SS+(ssres*(N-df)/dfres))
epsilonSq<-(SS-(ssres*df/dfres))/(ssmod+ssres)
epsilonSqP<-(SS-(ssres*df/dfres))/(SS+ssres)

colnames(data[,1])

a<-list("a","b",c("a","b"))
jmvcore::composeFormula(NULL,c(as.numeric(TRUE),a))
form1<-~1+x+z+x:z
form2<-~x+z+z:x
as.formula(form1)
q<-which(form1==form2)
terms(form1)
setdiff(attr(terms(sform1),"term.labels"),attr(terms(form2),"term.labels"))


'%+%' <- function(x,y) UseMethod(".plus")

.plus.default<-function(x,y) x+y

.plus.character <- paste0

.plus.list <- function(x,y)  {
    x[[length(x) + 1]] <- y
    x
}

'%-%' <- function(x,y) UseMethod(".minus")

.minus.default<-function(x,y) x-y

.minus.character <-  function(x,y)  {
  x[!(x==y)]
}

.minus.list <-  function(x,y)  {
  setdiff(x,y)
}


xx<-attr(terms(as.formula(jmvcore::composeFormula(a))),"term.labels")
yy<-attr(terms(as.formula(jmvcore::composeFormula(b))),"term.labels")
xx
yy


a<-list("a","b", c("a","b")) 
b<-list("d","a","b", c("a","b"),c("b","a"),c("a","a","a"))
setdiff(b,a)

c("a","b","c","b") %-% c("b")

"ciao" %-% "a"
class("ciao")
x<-c("a","b","c","b")


names(hsbdemo)
contrasts(hsbdemo$schtyp)<-contr.sum(2)/2
model<-lm(data = hsbdemo,
  formula=science~math+prog+math:prog
)
terms(model)
model
data<-model$model
types<-attr(terms(model),"dataClasses")

for (name in names(types)) {
  if (types[[name]]=="numeric") 
     data[[name]] as.numeric(scale(data[[name]]))
}
str(data)
str(model$model)
update(model,data=newdata)
