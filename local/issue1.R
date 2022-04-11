dat<-read.csv("../exdata/Jamovi_test_data.csv")
dim(dat)

library(lmerTest)
dat$Emotion
model<-lmer(TFD_Emotional~Emotion*Type+(1|Participant)+(1|Trail_Number),data=dat)
summary(model)
library(car)
a<-Anova(model,type="III",test.statistic="F")
library(emmeans)
emmeans(model,c(""))


