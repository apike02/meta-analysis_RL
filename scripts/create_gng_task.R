create_gng_task <- function (ntrials, probability, taskname){
  
  probabilities<-list()

  gtw<-ifelse(runif(150,0,1)<probability,1,0)
  gta<-ifelse(runif(150,0,1)<probability,0,-1)
  ngtw<-ifelse(runif(150,0,1)<probability,0,1)
  ngta<-ifelse(runif(150,0,1)<probability,-1,0)
  stim<-sample.int(4,150,replace=TRUE)
  
  probabilities[[1]]<-probability
  probabilities[[2]]<-1-probability
  
  #save task probabilities
  save(probabilities,file=paste('N:/Alex/metaRL/task/probabilities_',taskname,sep=''))
  
  go_outcome<-NULL
  nogo_outcome<-NULL
  
  for (t in 1:ntrials){
    if(stim[t]==1){
      go_outcome[t]<-gtw[t]
    } else if (stim[t]==2){
      go_outcome[t]<-gta[t]
    } else if (stim[t]==3){
      go_outcome[t]<-ngtw[t]
    } else if (stim[t]==4){
      go_outcome[t]<-ngta[t]
    }
    
    if(stim[t]==1){
      nogo_outcome[t]<-1-gtw[t]
    } else if (stim[t]==2){
      nogo_outcome[t]<--1-gta[t]
    } else if (stim[t]==3){
      nogo_outcome[t]<-1-ngtw[t]
    } else if (stim[t]==4){
      nogo_outcome[t]<--1-ngta[t]
    }
  }
  

  
  #return task
  task<-cbind(stim,go_outcome,nogo_outcome)
  return(task)
}
