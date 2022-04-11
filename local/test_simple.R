data <- as.data.frame(matrix(data = c(1,3,1,
                                      1,4,2,
                                      1,5,3,
                                      1,3,4,
                                      1,2,5,
                                      1,3,6,
                                      1,4,7,
                                      1,3,7,
                                      1,5,6,
                                      1,3,5,
                                      0,6,4,
                                      0,3,3,
                                      0,2,2,
                                      0,4,1,
                                      0,3,1,
                                      0,2,2,
                                      0,1,3,
                                      0,2,4,
                                      0,3,5,
                                      0,4,6), ncol=3, byrow = T))

write.csv(data,"data/simpleex.csv")
analysis1 <- gamlj::gamljGLM(
  data = data,
  dep = "V3",
  factors = "V1",
  covs = "V2",
  modelTerms = list(
    "V2",
    "V1",
    c("V1", "V2")),
  contrasts = list(
    list(
      var="V1",
      type="dummy")), #This is the offending line, change this to "simple" and you get different results.
  simpleVariable = "V1",
  simpleModerator = "V2",
  scaling = list(
    list(
      var="V2",
      type="centered")))
analysis1

mm<-tapply(data$V3, data$V1, mean)
mm[1]-mm[2]

analysis2 <- gamlj::gamljGLM(
  data = data,
  dep = "V3",
  factors = "V1",
  covs = "V2",
  modelTerms = list(
    "V2",
    "V1",
    c("V1", "V2")),
  contrasts = list(
    list(
      var="V1",
      type="simple")),
  simpleVariable = "V1",
  simpleModerator = "V2",
  scaling = list(
    list(
      var="V2",
      type="centered")))
analysis2



x<-c("b","a","a","b","b","b")
x<-factor(x,levels=c("b","a"))

levels(x)<-levels(x)[c(2,1)]
table(x)
