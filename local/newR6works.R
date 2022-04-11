library(jamm)
data(coopmedmod)
data$prime<-factor(data$prime)

model<-lm(BEH~prime*SVO*EXP,data=data)

options<-list("dep"="BEH",covs=c("SVO","EXP"),
              simpleScale="mean_sd",
              scaling=list(list(var="SVO",type="centered"),list(var="EXP",type="centered")),
              dep_scale="centered")
  

dm<-Datamatic$new(options,data)
data64<-dm$cleandata(data)
dm$variables[[tob64("SVO")]]
terms<-c("SVO","EXP")
terms64<-tob64(terms)
conditions<-sapply(terms64,function(term) {
  dm$variables[[term]]$levels 
},simplify = FALSE)

form<-"BEH~prime*SVO*EXP"
form64<-tob64(form,names(data))
model<-lm(form64,data=data64)
x<-tob64("prime")
referenceGrid<-emmeans::emmeans(model,x,by=terms64,at=conditions)
referenceGrid@grid[[x]]<-tob64(as.character(referenceGrid@grid[[x]]))

emm<-emmeans::contrast(referenceGrid,method="pairwise",infer=c(TRUE))

emm<-as.data.frame(emm)
emm$contrast
emmeans::test(emm,join=T)


source("R/jamovi.R")
source("R/functions.R")
source("R/constants.R")

data<-hsbdemo




options<-list("dep"=c("prog"),covs=c("math","write"), factors="ses",
              simpleScale="mean_sd",
              scaling=list(list(var="math",type="centered")),
              simpleVariable="math",
              simpleModerators=c("ses","write"),test=1:100000000000000)


datamatic<-Datamatic$new(options,data)

object.size(datamatic)
object.size(options)

data64<-datamatic$cleandata(data)

form<-"prog~ses*math*write"
form<-as.formula(tob64(form,dm$vars))
model<-nnet::multinom(form,data=data64)
summary(model)


levels <-lapply(options$simpleModerators, function(x) {
  
       if (datamatic$variables[[tob64(x)]]$type=="factor")
                   seq_along(datamatic$variables[[tob64(x)]]$levels)
       else 
                 datamatic$variables[[tob64(x)]]$levels
      })


vars  <-lapply(options$simpleModerators, function(x) datamatic$variables[[tob64(x)]])

rows<-expand.grid(levels)
names(rows)<-tob64(options$simpleModerators)
variable64<-tob64(options$simpleVariable)
varobj<-datamatic$variables[[variable64]]
focusparams<-varobj$paramsnames64

.names<-names(rows)
parameters<-data.frame()
for (i in 1:nrow(rows)) {
  .data1<-data64
   for (.name in .names) {
        if (is.factor(.data1[[.name]]))
            contrasts(.data1[[.name]])<-contr.treatment(nlevels(.data1[[.name]]),base = rows[i,.name])
        else
           .data1[[.name]]<-.data1[[.name]]-rows[i,.name]
         }
         params<-update(model,data=.data1)
         params<-as.data.frame(parameters::parameters(params))
         params<-params[params$Parameter %in% varobj$paramsnames64,]
         params[,.names]<-rows[i,]         
         parameters<-rbind(parameters,params)

  }
parameters         

         
         
         
Queue <-  R6::R6Class("Queue",
                 public = list(
                   initialize = function(...) {
                     for (item in list(...)) {
                       self$add(item)
                     }
                   },
                   add = function(x) {
                     xx<-x+1
                     private$queue <- c(private$queue, list(xx))
                     invisible(self)
                   },
                   remove = function() {
                     if (private$length() == 0) return(NULL)
                     # Can use private$queue for explicit access
                     head <- private$queue[[1]]
                     private$queue <- private$queue[-1]
                     head
                   }
                 ),
                 private = list(
                   queue = list(),
                   length = function() base::length(private$queue)
                 )
)
data<-1:1000000
object.size(data)
q <- Queue$new(data)
object.size(q)
