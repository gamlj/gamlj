B0 <-  runif(50,0,2)                # intercept
B1 <-  1.5                # slope for x1
B2 <- -0.5                # slope for x2
data<-data.frame()
i<-2
for (i in seq_along(B0)) {
b00<-B0[i]
y <- rpois(35, 6.5)+1
x2 <- seq(-0.5, 0.5,,length(y))
x1 <- (log(y) - b00 - B2 * x2) / B1
my.model <- glm(y ~ x1 + x2, family = poisson(link = log))
print(i)
summary(my.model)
data<-rbind(data,cbind(y,x1,x2,i))
}
data$i<-factor(data$i)
library(lme4)
mod<-glmer(y~x1+x2+(1|i),data=data,family = poisson())
summary(mod)
#write.csv(data,"/home/marcello/Skinner/Forge/jamovi/gamlj_docsource/data/mixpois.csv",row.names = F)
