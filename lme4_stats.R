if(any(grepl("package:lmerTest",search()))) detach("package:lmerTest") else message("")

numf<-function(val){sub("^(-?)0.","\\1.",sprintf("%.3f",val))}
numf2<-function(val){sub("^(-?)0.","\\1.",sprintf("%.4f",val))}

lme4_stats<-function(model=NULL,backtransformation=NULL,coding=NULL){
  
  p_values<-rep(0,length(model@beta))
  t_values<-rep(0,length(model@beta))
  params<-matrix(nrow=nrow(summary(model)$coefficients),ncol=ncol(summary(model)$coefficients))
  bt<-rep(0,length(model@beta))
  lower<-rep(0,length(model@beta))
  upper<-rep(0,length(model@beta))
  
  for (p in 1:length(model@beta)){
    p_values[p]<-2*(1-pt(abs(summary(model)$coefficients[p,3]),model@devcomp$dims[1][[1]]-length(model@beta)))
  }
  
  for (j in 1:length(model@beta)){
    if(backtransformation==T&coding==abs(.5)){bt[j]<-exp(summary(model)$coefficients[1]+(summary(model)$coefficients[j])/2)-exp(summary(model)$coefficients[1]-(summary(model)$coefficients[j])/2)
    } else if(backtransformation==T&coding==abs(1)){bt[j]<-exp(summary(model)$coefficients[1]+(summary(model)$coefficients[j]))-exp(summary(model)$coefficients[1]-(summary(model)$coefficients[j]))
    } else if(backtransformation==T){stop("!!!WARNING: coding shoud be either .5 or 1")}
  }
  
  for (j in 1:length(model@beta)){
    if(backtransformation==T&coding==abs(.5)){lower[j]<-exp(summary(model)$coefficients[1]+(summary(model)$coefficients[j]-summary(model)$coefficients[j,2]*2)/2)-exp(summary(model)$coefficients[1]-(summary(model)$coefficients[j]-summary(model)$coefficients[j,2]*2)/2)
    } else if(backtransformation==T&coding==abs(1)){lower[j]<-exp(summary(model)$coefficients[1]+(summary(model)$coefficients[j]-summary(model)$coefficients[j,2]*2))-exp(summary(model)$coefficients[1]-(summary(model)$coefficients[j]-summary(model)$coefficients[j,2]*2))
    } else if(backtransformation==T){stop("!!!WARNING: coding shoud be either .5 or 1")}
  }
  
  for (j in 1:length(model@beta)){
    if(backtransformation==T&coding==abs(.5)){upper[j]<-exp(summary(model)$coefficients[1]+(summary(model)$coefficients[j]+summary(model)$coefficients[j,2]*2)/2)-exp(summary(model)$coefficients[1]-(summary(model)$coefficients[j]+summary(model)$coefficients[j,2]*2)/2)
    } else if(backtransformation==T&coding==abs(1)){upper[j]<-exp(summary(model)$coefficients[1]+(summary(model)$coefficients[j]+summary(model)$coefficients[j,2]*2))-exp(summary(model)$coefficients[1]-(summary(model)$coefficients[j]+summary(model)$coefficients[j,2]*2))
    } else if(backtransformation==T){stop("!!!WARNING: coding shoud be either .5 or 1")}
  }
  
  spp<-ifelse(p_values<0.001,"<.001",ifelse(p_values==0.001,"=.001",numf2(p_values)))
  t_values<-summary(model)$coefficients[,3]
  if(backtransformation==T){bt[1]<-NA}
  if(backtransformation==T){lower[1]<-NA}
  if(backtransformation==T){upper[1]<-NA}
  if(backtransformation==T)bt<-as.numeric(as.character(bt))
  if(backtransformation==T)lower<-as.numeric(as.character(lower))
  if(backtransformation==T)upper<-as.numeric(as.character(upper))
  params<-summary(model)$coefficients[1:nrow(summary(model)$coefficients),]
  stats<-cbind(params,p_values)
  stats[,1]<-round(stats[,1],digits=3)
  stats[,2]<-round(stats[,2],digits=2)
  stats[,3]<-round(stats[,3],digits=2)
  stats<-as.data.frame(stats)
  stats$p_values<-numf(p_values)
  stats$p_values<-ifelse(stats$p_values==".000",".001",stats$p_values)
  if(backtransformation==T)stats<-cbind(stats,spp,bt,lower,upper)
  if(backtransformation==T){colnames(stats)<-c("Estimate","SE","t value","p value","*","Raw estimate","lower-95%","upper-95%")}
  if(backtransformation==F)stats<-cbind(stats,spp)
  if(backtransformation==F){colnames(stats)<-c("Estimate","SE","t value","p value","*")}
  rownames(stats)[1]<-"Intercept"
  return(stats)
}
