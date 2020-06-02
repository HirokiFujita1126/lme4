lme4_stats<-function(model=NULL,backtransformation=NULL,coding=NULL){
  p_values<-rep(0,length(model@beta))
  params<-matrix(nrow=nrow(summary(model)$coefficients),ncol=ncol(summary(model)$coefficients))
  bt<-rep(0,length(model@beta))
  for (p in 1:length(model@beta)){
    p_values[p]<-2*(1-pt(abs(summary(model)$coefficients[p,3]),model@devcomp$dims[1][[1]]-length(model@beta)))
    }
    for (j in 1:length(model@beta)){
      if(backtransformation==T&coding==abs(.5)){bt[j]<-exp(summary(model)$coefficients[1]+(summary(model)$coefficients[j])/2)-exp(summary(model)$coefficients[1]-(summary(model)$coefficients[j])/2)
      } else if(backtransformation==T&coding==abs(1)){bt[j]<-exp(summary(model)$coefficients[1]+(summary(model)$coefficients[j]))-exp(summary(model)$coefficients[1]-(summary(model)$coefficients[j]))
      } else if(backtransformation==T){stop("!!!WARNING: coding shoud be either .5 or 1")}
    }
  if(backtransformation==T){bt[1]<-NA}
  if(backtransformation==T)bt<-round(as.numeric(as.character(bt)))
  params<-summary(model)$coefficients[1:nrow(summary(model)$coefficients),]
  stats<-cbind(params,p_values)
  stats[,1]<-round(stats[,1],digits=3)
  stats[,2]<-round(stats[,2],digits=2)
  stats[,3]<-round(stats[,3],digits=2)
  stats[,4]<-round(stats[,4],digits=4)
  colnames(stats)[4]<-"p value"
  if(backtransformation==T)stats<-cbind(stats,bt)
  if(backtransformation==T)colnames(stats)[5]<-"backtransformed estimate"
  return(stats)
}
