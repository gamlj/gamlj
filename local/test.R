library(yaml)

data<-read_yaml("jamovisource/gamljglm.a.yaml")

str(data)

write_yaml(data,file="jamovisource/example.a.yaml",indent=4)
a<-c(.333,-.667,.333)
sum(a*a)/(3)
aa<-rep(a,each=1000)
b<-c(-.5,.5)
bb<-rep(b,each=1000)
sd(bb)


a<-c(-1,0,1)
sum(a*a)/(3)
aa<-rep(a,each=1000)
vv<-var(aa)
a/vv
-1.376*vv
-1.153*vv

m1<--1
m2<-1

mod<-gamljGlm(formula=Sepal.Length~Sepal.Width*Petal.Length*Petal.Width, 
              data=iris,
              plotHAxis = "Sepal.Width"
)
mod
plot(mod,formula = ~Sepal.Width:Petal.Length, simpleScale="percent")



jmv::descriptives(
  data = data,
  vars = vars(exer, endu, sptype))
