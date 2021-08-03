simulate_choices_predictionerror <- function(parameters,task,gng){
  
  list2env(parameters,environment())
  
  if (!exists('alpha',inherits=FALSE) & !exists('alphawin',inherits=FALSE)){#no learning rate equivalent to setting it to 1
    alpha<-1
  }
  
  if (!exists('alphawin',inherits=FALSE)){#only one learning rate
    alphawin<-alpha
    alphaloss<-alpha
  } 
  if (!exists('alphaomit',inherits=FALSE)){#no separate learning rate for omitting outcomes
    alphaomit<-0
  }
  
  if (exists('tau',inherits=FALSE)){
    beta<-1/tau #inverse temp
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
  
  if (!exists('rewsens',inherits=FALSE)){#if only one sensitivity param
    rewsens<-sensitivity
    punsens<-sensitivity
  }
  
  if (!exists('lapse',inherits=FALSE)){
    lapse<-0
  }  
  
  if (!exists('r0',inherits=FALSE)){#no r0 equivalent to setting to 0
    r0<-0
  }
  
  if (!exists('memory',inherits=FALSE)){#no memory equiv to setting to 1
    memory<-1
  }
  
  if (gng==0){
    QA <- rep(0, nrow(task)+1) 
    QB <- rep(0, nrow(task)+1) 
    ProbA <- rep(0,nrow(task))
    rewardA <- task[,1] #reward (the m terms are essentially binary indicators here)
    rewardB <- 1-rewardA
    punishA <- task[,2] #punishment 
    punishB <- 1-punishA
    outcomeA <- rewardA-punishA
    outcomeB <- rewardB-punishB
  } else {
    ProbGo <-rep(0,nrow(task))
    Q_go <- rep(0,4)
    Q_nogo <- rep(0,4)
    stim<-task$stim
  }
  
  



  choices <- rep(100, nrow(task))

  

  
  randoms<- runif(nrow(task))
  task<-data.frame(task)
  
  for (t in 1:nrow(task)){
    #here '1' is choosing shape A (i.e. the only one we have simulated, really)
    
    if (gng==1){
      if((Q_go[stim[t]]*beta) >709){ProbGo[t]=1
      } else if((Q_go[stim[t]]*beta)< -709){ProbGo[t]=0
      } else if((Q_nogo[stim[t]]*beta)> 709){ProbGo[t]=0
      } else if((Q_nogo[stim[t]]*beta)< -709){ProbGo[t]=1
      } else {
        ProbGo[t] = (1-lapse)*(exp(Q_go[stim[t]]*beta))/(exp(Q_go[stim[t]]*beta) + exp(Q_nogo[stim[t]]*beta))+lapse/2
      }
      choices[t]<- ifelse(ProbGo[t]>randoms[t], 1, 2)
      if (choices[t]==1){
        if ((task$go_outcome[t]-Q_go[stim[t]])>0){
          lr=alphawin
        } else {
          lr=alphaloss
        }
        Q_go[stim[t]] = Q_go[stim[t]] + lr * (task$go_outcome[t]-Q_go[stim[t]]) 
      } else if (choices[t]!=1) {
        if ((task$nogo_outcome[t]-Q_nogo[stim[t]])>0){
          lr=alphawin
        } else {
          lr=alphaloss
        }
        Q_nogo[stim[t]] = Q_nogo[stim[t]] + lr*(task$nogo_outcome[t]-Q_nogo[stim[t]]) 
      } 
  
    } else {
      
      if(QA[t]*beta> 709) {ProbA[t]=1
      } else if(QA[t]*beta< -709) {ProbA[t]=0
      } else if(QB[t]*beta> 709) {ProbA[t]=0
      } else if(QB[t]*beta< -709) {ProbA[t]=1
      } else {
        ProbA[t] = (1-lapse)*exp(QA[t]*beta)/(exp(QA[t]*beta) + exp(QB[t]*beta))+lapse/2
      }

      choices[t]<- ifelse(ProbA[t]>randoms[t], 1, 2)
      
      if (choices[t]==1){
        if ((outcomeA[t]-QA[t])<0){
          lr=alphawin
        } else {
          lr=alphaloss
        }
        QA[t+1] = QA[t] + lr*(outcomeA[t]-QA[t])
        QB[t+1] = QB[t]
        
      } else {
        if((outcomeB[t]-QB[t])<0){
          lr=alphawin
        } else {
          lr=alphaloss
        }
        QB[t+1] = QB[t] + lr*(outcomeB[t]-QB[t])
        QA[t+1] = QA[t]
      }
    }
  }
  return (choices)
}
