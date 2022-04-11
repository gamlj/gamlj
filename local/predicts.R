

a<-as.data.frame(cbind(a=1,x=1:25,cl=rep(1:5,each=5)))

b<-cbind(2,1:4)

lapply(1:nrow(b),function(x) cbind(p=rowSums(b[x,]*a[a$cl==x,c("a","x")]),cl=x))

rowSums(cbind(2,1:4))


data("wicksell")
data<-wicksell
data$group<-factor(data$group)
#data$time<-factor(data$time)

gobj<-gamlj::gamljMixed(
    formula = dv ~ 1 + group + time + group:time+( 1 | subj ),
    data = data)



library(lme4)    

mod<-lme4::lmer(formula = dv ~ 1 + group + time + group:time+( 1 +time| subj ),data = data)
mod<-lme4::lmer(formula = dv ~ 1 +  time +( 1 +time| subj ),data = data)
fixef(mod)[[1]]
ndata<-data
ndata$group<-factor(0)

emmeans::emmeans(mod,specs="time",at=list(time=c(0:10)))
model<-mod
pp1<-stats::predict(mod,newdata=ndata,random.only=T,allow.new.levels=TRUE)
cc<-lme4::fixef(model)[c("(Intercept)","time")]
.data<-cbind(1,c(0:10))
pp2<-cbind(1,ndata$time) %*% cc
pp3<-pp1+pp2
plot(pp3~data$time)
cc<-fixef(mod)
int<-cc[[1]]
b<-ranef(mod)$subj
ccc<-cc[c(1,2)]
.c<-b+ccc
x<-cbind(1,pretty(c(min(data$time),max(data$time)),n=20))
mm<-lapply(1:nrow(b),function(x) cbind(p=rowSums(.c[x,]*x),cl=x))
mm<-as.data.frame(do.call("rbind",mm))
r1<-predict(mod,random.only=T)
r2<-predict(mod,re.form=~(1+time|subj))
r<-r1+r2
rr<-predict(mod)

fitted.lmer
mkNewReTrms
