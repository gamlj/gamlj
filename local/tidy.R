library(formatR)

lf<-list.files("R/")
aa<-sapply(lf,strsplit,".",fixed=T)
bb<-sapply(a, length)
cc<-bb<3
lf<-names(cc[cc])
lf<-lf[!(lf %in% c("constants.R","00jmv.R"))]

for (l in lf)
       try(
      tidy_file(paste0("R/",l)),
      silent = T
     )

