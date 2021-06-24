
es.relativerisk<-function(obj) {

      model          <-  obj$model
      data           <-  model$data
      data$id_id_id  <- seq_len(dim(data)[1])
      depobj         <- obj$datamatic$variables[[tob64(obj$options$dep)]]
      levs           <- depobj$levels
      ciWidth        <- obj$ciwidth
      
      data[,depobj$name64]  <-  as.numeric(data[[depobj$name64]]==levs[2])

      results<-try_hard(geepack::geeglm(as.formula(obj$formula64),
                                        family = poisson(link = "log"),
                                        id = id_id_id, 
                                        corstr = "exchangeable", data = data)
      )
      obj$warnings   <-  list(topic="tab_relativerisk",message=results$warning)
      obj$errors     <-  list(topic="tab_relativerisk",message=results$error)
      if (!isFALSE(results$error))
         return()
      
      params<-as.data.frame(parameters::parameters(results$obj,exponentiate=TRUE))
      names(params)<-c("source","estimate","se","nothing", "ci.lower","ci.upper","test","df","p")
      
      if (params$source[1]=="(Intercept)")
             params <- params[-1,] 
      
    return(params)

}      

