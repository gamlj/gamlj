data("wicksell")
dat<-wicksell
dat$group<-factor(dat$group)
contrasts(dat$group)<-contr.sum(2)/2

dat$subj<-factor(dat$subj)
dat$time<-factor(dat$time)
contrasts(dat$time)<-contr.treatment(4)-(1/4)

crt<-lmerControl(optimizer = "nloptwrap")
crt<-lmerControl(optimizer = "bobyqa")

mod<-lmer(dv~group*time+(1|subj),data=dat,control = crt)
x<-rep(1,length(dat$subj))
mod<-lm(dv~group*time*x*I(x^2),data=dat)
alias(mod)
mod<-lm(dv~group+x,data=dat)
coef(mod)
attributes(mod)
attr(mod,"gmalj")<-list(ciao=1,ciao2="dsfsafsd")


