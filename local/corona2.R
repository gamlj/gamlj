folder<-"/home/marcello/Skinner/Stat/data/COVID-19/csse_covid_19_data/csse_covid_19_daily_reports/"
ff<-list.files(folder)
ff<-ff[grep("20",ff,fixed = T)]
data<-read.csv(paste0(folder,ff[1]))
nn<-names(data)

data$Date<-"ok"
data$Day<-0
data<-data[0,]

d<-0
f<-"ok"
for (f in ff) {
  d<-d+1
  data0<-read.csv(paste0(folder,f))
  data0<-data0[,nn]
  date<-gsub(".csv","",f,fixed = T)
  data0$Date<-date
  data0$Day<-d
  data<-rbind(data,data0)
}

data$Confirmed<-ifelse(is.na(data$Confirmed),0,data$Confirmed)
data$Deaths<-ifelse(is.na(data$Deaths),0,data$Deaths)
data$Recovered<-ifelse(is.na(data$Recovered),0,data$Recovered)

data<-data[data$Confirmed>0,]
data<-data[data$Country.Region!="Others"]
tdata<-aggregate(data$Confirmed,list(data$Country),sum)

adata<-aggregate(data$Confirmed,list(data$Country,data$Day),sum)
names(adata)<-c("Country","Day","Confirmed")
it<-adata[adata$Country=="Italy",]
ni<-length(it$Confirmed)
it$Confirmed[ni]/it$Confirmed[ni-1]


library(gamlj)
form<-Confirmed ~ Day + I(Day^2) + I(Day^3)+I(Day^4)
tt<-tapply(data$Confirmed,data$Country.Region,sum)
u1000<-names(which(tt>1000))
u1000<-u1000[order(u1000)]
last<-max(data$Day)
library(lmerTest)
ddata<-adata[adata$Country==u1000[[8]],]
mod<-glm(form, data=ddata,family=Gamma("log"))  
p<-predict(mod,type = "response")
Day<--9:(last-9)
q<-data.frame(i=1:length(Day))
q$Day<-Day
q$p2<-predict(mod,type = "response",q)
q$p2<-round(q$p2,digits = 0)
pre<-ddata[1:11,]
adata$Day[adata$Country=="Mainland China"]<-adata$Day[adata$Country=="Mainland China"]+11
adata<-rbind(adata,pre)
xdata<-data.frame()
for (n in unique(adata$Country)) {
   if (n %in% u1000) {
     idata<-adata[adata$Country==n,]
     idata$Day<-1:length(idata$Day)
     xdata<-rbind(xdata,idata)
   }
}

p <- ggplot2::ggplot()
p <- p + ggplot2::geom_point(data=ddata,aes_string(x="Day", y="Confirmed"),show.legend=FALSE, alpha=.5,shape=16, size=2)
p <- p+ ggplot2::geom_line(data=q,aes_string(x="Day", y="p2"),size=1.2) 
p<-p+geom_vline(xintercept = 50, color="red")
p<-p+jmvcore::theme_default()

#xdata$Day<-xdata$Day/100
xdata$Day<-xdata$Day-mean(xdata$Day)

xdata$Country<-factor(as.character(xdata$Country))
table(xdata$Country)
form<-Confirmed ~ Day + I(Day^2) + I(Day^3)+I(Day^4)+(1+Day+I(Day^2)|Country)
mmod2<-glmer(form,family = Gamma("log"),data=xdata)
summary(mmod2)

write.csv(xdata,"xdata.csv",row.names = F)


