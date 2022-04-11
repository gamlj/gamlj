library(lme4)
library(nnet)
funs<-ls(pos = "package:lme4" )
funs<-ls(pos = "package:nnet" )

for (f in funs) {
  cmd<-paste("grep",paste0("'",f,"('"," R/*"))
  res<-system(cmd,intern = T)
  if (length(res)) {
    print(f)
    print(res)
    print("#########")
  }
}
