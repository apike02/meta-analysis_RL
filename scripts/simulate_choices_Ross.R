simulate_choices_Ross <- function(parameters,task,gng){
  
  list2env(parameters,environment())
  
  if(gng==1){
    nstim=4
    stim=task$stim
    go_outcome=task$go_outcome
    nogo_outcome=task$nogo_outcome
  } else {
    outcome=t(cbind(task[,1]-task[,2],(1-task[,1])-(1-task[,2])))
    nstim=2
  }
  
  Q <- matrix(NA, nstim, nrow(task)+1) 
  Q_go <- matrix(NA,nstim, nrow(task)+1)
  Q_nogo <- matrix(NA,nstim, nrow(task)+1)
  
  Q[,1]<-0
  Q_go[,1]<-0
  Q_nogo[,1]<-0
  
  ProbGo <-rep(NA,nrow(task))
  ProbA <- rep(NA,nrow(task))
  choices <- rep(NA, nrow(task))
  
  randoms<- runif(nrow(task))
  task<-data.frame(task)
  
  
  for (t in 1:nrow(task)){
    #here '1' is choosing shape A (i.e. the only one we have simulated, really)
    
    if (gng==1){
      
      ProbGo[t] = 1/(1+exp(-1*(Q_go[stim[t]]-Q_nogo[stim[t]])*beta))
      choices[t]<- ifelse(ProbGo[t]>randoms[t], 1, 2)
      
      Q_go[,t+1]=Q_go[,t]
      Q_nogo[,t+1]=Q_nogo[,t]
      
      if (choices[t]==1){
        predictionerror=go_outcome[t]-Q_go[stim[t],t]
        if (predictionerror>0){
          learning=alphawin
        } else {
          learning=alphaloss
        }
        Q_go[stim[t],t+1]=Q_go[stim[t],t]+learning*(go_outcome[t]-Q_go[stim[t],t])
      } else {
        predictionerror=nogo_outcome[t]-Q_nogo[stim[t],t]
        if (predictionerror>0){
          learning=alphawin
        } else {
          learning=alphaloss
        }
        Q_nogo[stim[t],t+1]=Q_nogo[stim[t],t]+learning*(go_outcome[t]-Q_nogo[stim[t],t])
      }
      
      #barplot(c(QA_go,QA_nogo,QB_go,QB_nogo,QC_go,QC_nogo,QD_go,QD_nogo))
      
    } else {
      
        ProbA[t] = 1/(1+exp(-1*(Q[1,t]-Q[2,t])*beta))
        choices[t]<- ifelse(ProbA[t]>randoms[t], 1, 2)
        stim=choices[t]
        predictionerror=outcome[stim,t]-Q[stim,t]
    
        
        if (predictionerror>0){
          learning=alphawin
        } else {
          learning=alphaloss
        }
        
        Q[,t+1]=Q[,t]
        
        Q[stim,t+1]=Q[stim,t]+learning*(outcome[stim,t]-Q[stim,t])

    }
  }
  return (choices)
}
