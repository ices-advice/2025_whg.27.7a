# JIT with fixed parameters
jitfix<-function (fit, nojit = 10, par = defpar(fit$data, fit$conf), sd = 0.25, ncores = detectCores()) 
{
  require(parallel)
  parv <- unlist(par)
  pars <- lapply(1:nojit, function(i) relist(parv + rnorm(length(parv), sd = sd), par))
  pars <- lapply(pars, function(p){p$logSdLogN[2]<-log(0.05); p})
  if (ncores > 1) {
    cl <- makeCluster(ncores)
    on.exit(stopCluster(cl))
    lib.ver <- dirname(path.package("stockassessment"))
    clusterExport(cl, varlist = "lib.ver", envir = environment())
    clusterEvalQ(cl, {
      library(stockassessment, lib.loc = lib.ver)
    })
    fits <- parLapply(cl, pars, function(p) sam.fit(fit$data, 
                                                    fit$conf, p, silent = FALSE , map=list(logSdLogN=as.factor(c(1,NA)))))
  }
  else {
    fits <- lapply(pars, function(p) sam.fit(fit$data, fit$conf, 
                                             p, silent = TRUE, map=list(logSdLogN=as.factor(c(1,NA)))))
  }
  attr(fits, "fit") <- fit
  attr(fits, "jitflag") <- 1
  class(fits) <- c("samset")
  fits
}


# CROSS-VALIDATION helper function  
xval <- function(fit, year=NULL, fleet=NULL, age=NULL, ...){
  data <- fit$data
  nam <- c("year", "fleet", "age")[c(length(year)>0,length(fleet)>0,length(age)>0)]
  if((length(year)==0) & (length(fleet)==0) & (length(age)==0)){
    idx <- rep(TRUE,nrow(data$aux))
  }else{
    idx <- !do.call(paste, as.data.frame(data$aux[,nam,drop=FALSE])) %in% do.call(paste, as.data.frame(cbind(year=year, fleet=fleet, age=age)))
  }
  idx <- !idx
  data$logobs[idx] <- NA
  idx2 <- which(is.na(data$logobs))
  conf <- fit$conf
  par <- defpar(data,conf)
  thisfit <- sam.fit(data, conf, par, rm.unidentified = TRUE, newtonsteps=0, silent=TRUE,...)
  ret <- as.data.frame(cbind(data$aux[idx2,], obs=fit$data$logobs[idx2], pred=thisfit$pl$missing, predSd=thisfit$plsd$missing))
  ret <- ret[complete.cases(ret),]
  attr(ret, "fit") <- thisfit
  return(ret)
}
