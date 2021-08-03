simulate_choices_unvalenced <- function(parameters,task,gng){
  
  list2env(parameters,environment())
  
  if (!exists('alpha',inherits=FALSE) & !exists('alphawin',inherits=FALSE)){#no learning rate equivalent to setting it to 1
    alpha<-1
  }
  
  if (!exists('alphawin',inherits=FALSE)){#only one learning rate
    alphawin<-alpha
    alphaloss<-alpha
  } 
  
  if (!exists('beta',inherits=FALSE) & exists('tau', inherits=FALSE)){
    beta=1/tau #if non-inv temp
  }
  
  if (!exists('betawin',inherits=FALSE) & exists('tauwin',inherits=FALSE)){#is temp but not inv temp
    beta=1/((tauwin+tauloss)/2)
  }
  
  if (!exists('beta',inherits=FALSE) & !exists('betawin',inherits=FALSE)){#no temperature equivalent to setting it to 1
    beta<-1
    
  }
  
  if (!exists('betawin', inherits=FALSE)){#only one temperature
    betawin<-beta
    betaloss<-beta
  }
  
  if (!exists('sensitivity', inherits=FALSE) & !exists('rewsens',inherits=FALSE)){#no sens equivalent to setting it to 1
    sensitivity<-1
  }
  
  
  if (!exists('lapse',inherits=FALSE)){
    lapse<-0
  }  
  
  if (!exists('r0', inherits=FALSE)){
    r0<-0
  }
  
  
  ProbA <- rep(0.5, nrow(task)) #initialise probability of choosing shape A at 0.5
  choices <- rep(NA, nrow(task))
  randoms<- runif(nrow(task))
  
  if (gng==0){
    nstim=2
    QA<-rep(0,nrow(task)+1)
    QB<-rep(0,nrow(task)+1)
    outcomeA<-task[,1]-task[,2]
    outcomeB<-(1-task[,1])-(1-task[,2])
    ifelse(outcomeA==0,r0,outcomeA) #recodes midpoint (neutral) as r0 if myers model
    ifelse(outcomeB==0,r0,outcomeB)
  } else {
    nstim=4
    stim=task$stim
    Q_go<-rep(0,nstim)
    Q_nogo<-rep(0,nstim)
    ifelse(task$go_outcome==0,r0,task$go_outcome)
    ifelse(task$nogo_outcome==0,r0,task$nogo_outcome)
  }
  
  ProbGo <-rep(NA,nrow(task))
  
  rewardA <- task[,1] #reward (the m terms are essentially binary indicators here)
  punishA <- task[,2] #punishment 
  
  
  for (t in 1:nrow(task)){
    #here '1' is choosing shape A (i.e. the only one we have simulated, really)
    
    if (gng==1){
      if((Q_go[stim[t]]*beta) >709){ProbGo[t]=1
      } else if((Q_go[stim[t]]*beta)< -709){ProbGo[t]=0
      } else if((Q_nogo[stim[t]]*beta)> 709){ProbGo[t]=0
      } else if((Q_nogo[stim[t]]*beta)< -709){ProbGo[t]=1
      } else {
        ProbGo[t] = (1-lapse)*exp(Q_go[stim[t]]*beta)/(exp(Q_go[stim[t]]*beta)+exp(Q_nogo[stim[t]]*beta))+lapse/2
      }
      choices[t]<- ifelse(ProbGo[t]>randoms[t], 1, 2)
      
      if (choices[t]==1){
        if (task$go_outcome[t]==1) {
          lr=alphawin
        } else if (task$go_outcome[t]==0&(stim[t]==2|stim[t]==4)){
          lr=alphawin #counts as a win for avoid conditions
        } else {
          lr=alphaloss
        }
        Q_go[stim[t]] = Q_go[stim[t]] + lr * (sensitivity*task$go_outcome[t]-Q_go[stim[t]])
      } else {
        if (task$nogo_outcome[t]==1) {
          lr=alphawin
        } else if (task$nogo_outcome[t]==0&(stim[t]==2|stim[t]==4)){
          lr=alphawin #counts as a win for avoid conditions
        } else {
          lr=alphaloss
        }
        Q_nogo[stim[t]] = Q_nogo[stim[t]] + lr * (sensitivity*task$nogo_outcome[t]-Q_nogo[stim[t]])
      }
      
    } else {
        
      if((QA[t]*beta) >709){ProbA[t]=1
      } else if((QA[t]*beta)< -709){ProbA[t]=0
      } else if((QB[t]*beta)> 709){ProbA[t]=0
      } else if((QB[t]*beta)< -709){ProbA[t]=1
      } else {
        
        ProbA[t] = (1-lapse)*(exp(QA[t]*beta)/(exp(QA[t]*beta) + exp(QB[t]*beta)))+lapse/2
        
      }

      
      choices[t]<- ifelse(ProbA[t]>randoms[t], 1, 2)
      
      
      if (choices[t]==1){
        if (outcomeA[t]==1){
          lr = alphawin
        } else {
          lr = alphaloss
        }
        
        QA[t+1] = QA[t] + lr * (sensitivity * outcomeA[t] - QA[t]) # value learning for rewards for A
        QB[t+1] = QB[t] #don't update unchosen
        
      } else {
        if (outcomeB[t]==1){
          lr = alphawin
        } else {
          lr = alphaloss
        }
        
        QB[t+1] = QB[t] + lr * (sensitivity * outcomeB[t] - QB[t]) # value learning for rewards for B
        QA[t+1] = QA[t] #don't update unchosen

        
      }
    }
  }
  
  
  return (choices)
}
