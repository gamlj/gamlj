library(gh)
apitoken<-"5e7ee0b535200a84b81743e305f3290cc0e05e42"
gh_whoami(apitoken)
API_TOKEN<-"5e7ee0b535200a84b81743e305f3290cc0e05e42"

query<-paste0("/repos/:owner/:repo/branches")
vers<-gh(query, owner = "gamlj", repo = "gamlj",.limit=Inf,.token=API_TOKEN)
rvers<-rev(vers)
vernames<-sapply(rvers,function(a) a$name)
nvers<-1:(which(vernames==FIRST_VERSION)+1)
rvers<-rvers[nvers]
vers<-rev(rvers)
vernames<-sapply(vers,function(a) a$name)
r<-vers[[1]]
query<-paste0("/repos/:owner/:repo/commits")
coms<-gh(query,sha=r$name, owner = "gamlj", repo = "gamlj",.limit=Inf,.token=API_TOKEN)
date<-coms[[1]]$commit$author$date
vers<-vers[2:length(vernames)]
j<-1
results<-list()
for (r in vers) {
  query<-paste0("/repos/:owner/:repo/commits")
  coms<-gh(query, sha=r$name, since=date,owner = "gamlj", repo = "gamlj",.limit=Inf,.token=API_TOKEN)
  print(paste("##########",r$name,"#########"))
  for (com in coms) {
    results[[j]]<-c(sha=com$sha,msg=com$commit$message,version=r$name)
    j<-j+1
  }
  date<-coms[[1]]$commit$author$date
}
data<-as.data.frame(do.call("rbind",results))
head(data)
length(data$sha)
data[!duplicated(data$sha),]
