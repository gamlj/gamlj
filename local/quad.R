n<-100
x<-rnorm(n,10)
cx<-x-mean(x)
cond<-rep(c(0,1),n/2)
y<-4*x-.5*cx*cx-2.5*cx*cx*cond+rnorm(n,0,4)

plot(y~x)
cond<-factor(cond)
contrasts(cond)<-contr.sum(2)


mod<-lm(y~cond+x+cond:x+I(x^2))
summary(mod)
mod<-lm(y~cond+x+x:cond+I(x^2)+cond*I(x^2)+I(x^3))
summary(mod)
data<-data.frame(y=y,x=x,cond=cond)          
data$y<-round(round(data$y*100)/100)
data$x<-((round(data$x)-10)*3)+8
hist(data$y)
hist(data$x)

table(data$cond)
names(data)
data$pp<-NULL
names(data)<-c("performance","hours","cond")
data$type<-ifelse(data$cond==1,"P","A")
data$cond<-NULL
#data$hours<-data$hours-mean(data$hours)
names(data)
data$type<-factor(data$type)
contrasts(data$type)<-contr.sum(2)
mod<-lm(performance~type+hours+hours:type+I(hours^2)+type*I(hours^2),data=data)
summary(mod)
if (min(data$hours<0))
   data$hours<-data$hours-min(data$hours)
#write.csv(as.data.frame(data),"data/qsport.csv")
data$q<-NULL
mod<-lm(performance~type+hours+type:hours+I(hours^2)+I(hours^2):type,data=data)
summary(mod)
car::Anova(mod)

library(gamlj)
gmod<-gamlj::gamljGLM(
  data = data,
  dep = "performance",
  factors = "type",
  covs = "hours",
  modelTerms = list(
    "hours",
    "type",
    c("type", "hours"),
    c("hours", "hours")),
  contrasts = list(
    list(
      var="type",
      type="deviation")),
  plotHAxis = "hours",
  plotSepLines = "type",
  plotRaw = TRUE,
  simpleVariable = "hours",
  simpleModerator = "type",
  scaling = list(
    list(
      var="hours",
      type="centered")))

gmod
mt = list(
  "hours",
  "type",
  c("type", "ho urs"),
  c("hours", "hours"))

ffh<-lf.higherTerms(mt,model = T)
ffh<-ffh[order(sapply(ffh,length))]
ffh
fo<-lf.constructFormula(dep="y",ffh)
data$type<-factor(data$type)
qq<-data
qq<-qq[0,]

tterms<-terms(as.formula(fo),data = qq)
attr(tterms,"term.labels")
a<-gmod$descPlot
str(a)
b<-a$print()
ff<-list(
  "hours",
  "type",
  c("type", "hours"),
  c("hours", "hours","type"),
  c("hours", "hours","type","type"),
  c("hours", "hours","hours","type"))
higherTerms<-function(aList) {
  lapply(aList, function(a) {
  ho<-unique(a[duplicated(a)])
  if (length(ho))
    unlist(lapply(ho, function(b) {
    apex<-length(a[a==b])
    paste0("I(",b,"^",apex,")")
    }))
  else
    a
  })
}
ffh<-higherTerms(ff)
ff2<-list("hours","type")
higherTerms(ff2)


lf.higherTerms(ff,model=F)
