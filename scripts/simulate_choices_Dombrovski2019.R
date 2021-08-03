simulate_choices_Dombrovski2019 <- function(alphawin,alphaloss,decay,tau,task,gng){
  
  
  if (gng==0){
    outcomeA<-task[,1]-task[,2]
    outcomeB<-(1-task[,1]-(1-task[,2])) 
    QA <- rep(0, nrow(task)+1) 
    QB <- rep(0, nrow(task)+1) 
    ProbA <- rep(100,nrow(task))
  } else {
    Q_go<-matrix(data=NA,nrow=nrow(task)+1,ncol=4)
    Q_nogo<-matrix(data=NA,nrow=nrow(task)+1,ncol=4)
    Q_go[1,]<-rep(0,4)
    Q_nogo[1,]<-rep(0,4)
    ProbGo <-rep(NA,nrow(task))
    stim<-task$stim
    
  }
  
  choices <- rep(NA, nrow(task))
  
  rewardA <- task[,1] #reward (the m terms are essentially binary indicators here)
  punishA <- task[,2] #punishment 
  
  randoms<- runif(nrow(task))
  task<-data.frame(task)
  
  for (t in 1:nrow(task)){
    #here '1' is choosing shape A (i.e. the only one we have simulated, really)
    
    if (gng==1){
      if((Q_go[t,stim[t]]/tau) >709){ProbGo[t]=1
      } else if((Q_go[t,stim[t]]/tau)< -709){ProbGo[t]=0
      } else if((Q_nogo[t,stim[t]]/tau)> 709){ProbGo[t]=0
      } else if((Q_nogo[t,stim[t]]/tau)< -709){ProbGo[t]=1
      } else {
        ProbGo[t] = exp(Q_go[t,stim[t]]/tau)/(exp(Q_go[t,stim[t]]/tau)+exp(Q_nogo[t,stim[t]]/(tau)))
      }

      choices[t]<- ifelse(ProbGo[t]>randoms[t], 1, 2)
      
      Q_go[t+1,] = Q_go[t,] #update all unseen stimuli
      Q_nogo[t+1,] = Q_nogo[t,] #update all unseen stimuli
      
      
      
      if (choices[t]==1){
        if (task$go_outcome[t]==1) {
          lr=alphawin
        } else if (task$go_outcome[t]==0&(stim[t]==2|stim[t]==4)){
          lr=alphawin #counts as a win for avoid conditions
        } else {
          lr=alphaloss
        }
      } else {
        if (task$nogo_outcome[t]==1) {
          lr=alphawin
        } else if (task$nogo_outcome[t]==0&(stim[t]==2|stim[t]==4)){
          lr=alphawin #counts as a win for avoid conditions
        } else {
          lr=alphaloss
        }
      } 
      if (choices[t]==1){
        Q_go[t+1,stim[t]] =  Q_go[t,stim[t]] + lr*(task$go_outcome[t]-Q_go[t,stim[t]])
        Q_nogo[t+1,stim[t]] = decay*Q_nogo[t,stim[t]]
      } else { #if nogo
        Q_nogo[t+1,stim[t]] = Q_nogo[t,stim[t]] + lr *(task$nogo_outcome[t]-Q_nogo[t,stim[t]])
        Q_go[t+1,stim[t]] = decay*Q_go[t,stim[t]]
        
      }
      
      
    } else {
      
      if((QA[t]/tau) >709){ProbA[t]==1
      } else if((QA[t]/tau)< -709){ProbA[t]==0
      } else if((QB[t]/tau)> 709){ProbA[t]==0
      } else if((QB[t]/tau)< -709){ProbA[t]==1
      } else {
        
        ProbA[t] = exp(QA[t]/tau)/(exp(QA[t]/tau)+exp(QB[t]/(tau)))
        
      }
      
      
      choices[t]<- ifelse(ProbA[t]>randoms[t], 1, 2)
      
      if (choices[t]==1){
        if (outcomeA[t]==1){
          lr = alphawin
        } else {
          lr = alphaloss
        } 
        QA[t+1] = QA[t] + lr *(outcomeA[t]-QA[t]) 
        QB[t+1] = decay*QB[t]
        
      } else {
        if (outcomeB[t]==1){
          lr=alphawin
        } else {
          lr=alphaloss
        } 
        QB[t+1] = QB[t] + lr*(outcomeB[t]-QB[t])
        QA[t+1] = decay*QA[t]
      }
    }
  }
  return (choices)
}
