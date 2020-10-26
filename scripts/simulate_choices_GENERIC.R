simulate_choices <- function(parameters,task,gng){
  
  list2env(parameters,environment())
  
  if (!exists('alpha',inherits=FALSE) & !exists('alphawin',inherits=FALSE)){#no learning rate equivalent to setting it to 1
    alpha<-1
  }
  
  if (!exists('alphawin',inherits=FALSE)){#only one learning rate
    alphawin<-alpha
    alphaloss<-alpha
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


  ProbA <- rep(0.5, nrow(task)) #initialise probability of choosing shape A at 0.5
  choices <- rep(NA, nrow(task))
  randoms<- runif(nrow(task))
  
  QA_reward<-rep(NA,nrow(task))
  QA_punish<-rep(NA,nrow(task))
  QB_reward<-rep(NA,nrow(task))
  QB_punish<-rep(NA,nrow(task))
  
  QA_reward[1]<-0
  QA_punish[1]<-0
  QB_reward[1]<-0
  QB_punish[1]<-0
  
  QA_reward_go <- 0
  QA_reward_nogo<- 0
  QB_reward_go <- 0
  QB_reward_nogo <- 0
  QC_reward_go <- 0
  QC_reward_nogo <- 0
  QD_reward_go <- 0
  QD_reward_nogo <- 0
  QA_punish_go <- 0
  QA_punish_nogo<- 0
  QB_punish_go <- 0
  QB_punish_nogo <- 0
  QC_punish_go <- 0
  QC_punish_nogo <- 0
  QD_punish_go <- 0
  QD_punish_nogo <- 0
  ProbGo <-rep(NA,nrow(task))
  
  rewardA <- task[,1] #reward (the m terms are essentially binary indicators here)
  punishA <- task[,2] #punishment 
  

  for (t in 1:nrow(task)){
    #here '1' is choosing shape A (i.e. the only one we have simulated, really)
    
    if (gng==1){
      
      if (task$stim[t]==1){ #stimulus A
        QA_go = QA_reward_go*betawin + QA_punish_go*betaloss
        QA_nogo = QA_reward_nogo*betawin + QA_punish_nogo*betaloss
        ProbGo[t] = (1-lapse)*exp(QA_go)/(exp(QA_go)+exp(QA_nogo))+lapse/2
        if (QA_go*betawin>709){ProbGo[t] = 1}
        if (QA_go*betawin<(-709)){ProbGo[t] = 0}
        if (QA_nogo*betawin>709){ProbGo[t] = 0}
        if (QA_nogo*betawin<(-709)){ProbGo[t] = 1}
        choices[t]<- ifelse(ProbGo[t]>randoms[t], 1, 2)
        if (choices[t]==1){
          QA_reward_go = QA_reward_go + alphawin * (rewsens * task$go_outcome[t] - QA_reward_go) 
          QA_punish_go = QA_punish_go + alphaloss * (punsens * 0 - QA_punish_go) # punish always 0 for stima as win/omit
        } else { #if nogo
          QA_reward_nogo = QA_reward_nogo + alphawin * (rewsens * task$nogo_outcome[t] - QA_reward_nogo) 
          QA_punish_nogo = QA_punish_nogo + alphaloss * (punsens * 0 - QA_punish_nogo) # punish always 0 for stima as win/omit
          
        }
        
        
      } else if (task$stim[t]==2){ #stimulus B
        QB_go = QB_reward_go*betawin + QB_punish_go*betaloss
        QB_nogo = QB_reward_nogo*betawin + QB_punish_nogo*betaloss
        ProbGo[t] = (1-lapse)*(exp(QB_go)/(exp(QB_go) + exp(QB_nogo)))+lapse/2
        if (QB_go*betawin>709){ProbGo[t] = 1}
        if (QB_go*betawin<(-709)){ProbGo[t] = 0}
        if (QB_nogo*betawin>709){ProbGo[t] = 0}
        if (QB_nogo*betawin<(-709)){ProbGo[t] = 1}
        choices[t]<- ifelse(ProbGo[t]>randoms[t], 1, 2)
        if (choices[t]==1){
          QB_reward_go = QB_reward_go + alphawin * (rewsens * 0 - QB_reward_go) #reward always 0 as lose/omit
          QB_punish_go = QB_punish_go + alphaloss * (punsens * -1*task$go_outcome[t] - QB_punish_go) # 
        } else { #if nogo
          QB_reward_nogo = QB_reward_nogo + alphawin * (rewsens * 0 - QB_reward_nogo) 
          QB_punish_nogo = QB_punish_nogo + alphaloss * (punsens * -1*task$nogo_outcome[t] - QB_punish_nogo) # 
          
        }
        
        
      } else if (task$stim[t]==3){
        QC_go = QC_reward_go*betawin + QC_punish_go*betaloss
        QC_nogo = QC_reward_nogo*betawin + QC_punish_nogo*betaloss
        ProbGo[t] = (1-lapse)*(exp(QC_go)/(exp(QC_go) + exp(QC_nogo)))+lapse/2
        if (QC_go*betawin>709){ProbGo[t] = 1}
        if (QC_go*betawin<(-709)){ProbGo[t] = 0}
        if (QC_nogo*betawin>709){ProbGo[t] = 0}
        if (QC_nogo*betawin<(-709)){ProbGo[t] = 1}
        choices[t]<- ifelse(ProbGo[t]>randoms[t], 1, 2)
        if (choices[t]==1){
          QC_reward_go = QC_reward_go + alphawin * (rewsens * task$go_outcome[t] - QA_reward_go) 
          QC_punish_go = QC_punish_go + alphaloss * (punsens * 0 - QC_punish_go) # punish always 0 for stim c as win/omit
        } else { #if nogo
          QC_reward_nogo = QC_reward_nogo + alphawin * (rewsens * task$nogo_outcome[t] - QC_reward_nogo) 
          QC_punish_nogo = QC_punish_nogo + alphaloss * (punsens * 0 - QC_punish_nogo) # punish always 0 for stim c as win/omit
          
        }
        
        
      } else if (task$stim[t]==4){
        QD_go = QD_reward_go*betawin + QD_punish_go*betaloss
        QD_nogo = QD_reward_nogo*betawin + QD_punish_nogo*betaloss
        ProbGo[t] = (1-lapse)*(exp(QD_go)/(exp(QD_go) + exp(QD_nogo)))+lapse/2
        if (QD_go*betawin>709){ProbGo[t] = 1}
        if (QD_go*betawin<(-709)){ProbGo[t] = 0}
        if (QD_nogo*betawin>709){ProbGo[t] = 0}
        if (QD_nogo*betawin<(-709)){ProbGo[t] = 1}
        choices[t]<- ifelse(ProbGo[t]>randoms[t], 1, 2)
        if (choices[t]==1){
          QD_reward_go = QD_reward_go + alphawin * (rewsens * 0 - QD_reward_go) #reward always 0 as lose/omit
          QD_punish_go = QD_punish_go + alphaloss * (punsens * -1*task$go_outcome[t] - QD_punish_go) # 
        } else { #if nogo
          QD_reward_nogo = QD_reward_nogo + alphawin * (rewsens * 0 - QD_reward_nogo) 
          QD_punish_nogo = QD_punish_nogo + alphaloss * (punsens * -1*task$nogo_outcome[t] - QD_punish_nogo) # 
          
        }
        
      }
      
      #barplot(c(QA_reward_go,QA_reward_nogo,QB_punish_go,QB_punish_nogo,QC_reward_go,QC_reward_nogo,QD_punish_go,QD_punish_nogo))
    } else {
    
      #temporary fix for the fact can't do exp(700+)
      if (QA_reward[t]*betawin - QA_punish[t]*betaloss > 709){ProbA[t]=1
      } else if (QA_reward[t]*betawin - QA_punish[t]*betaloss < - 709){ProbA[t]=0
      } else {
        ProbA[t] = (1-lapse)*(exp(QA_reward[t]*betawin - QA_punish[t]*betaloss)/
          (exp(QA_reward[t]*betawin - QA_punish[t]*betaloss) + exp(QB_reward[t]*betawin - QB_punish[t]*betaloss)))+lapse/2
      }
      
      
      choices[t]<- ifelse(ProbA[t]>randoms[t], 1, 2)
      
      
      if (choices[t]==1){
        
        QA_reward[t+1] = QA_reward[t] + alphawin * (rewsens * rewardA[t] - QA_reward[t]) # value learning for rewards for A
        QA_punish[t+1] = QA_punish[t] + alphaloss * (punsens * punishA[t] - QA_punish[t]) # value learning for punishments for A
        QB_reward[t+1] = QB_reward[t] #don't update unchosen
        QB_punish[t+1] = QB_punish[t] #don't update unchosen
        
      } else {  
        
        QB_reward[t+1] = QB_reward[t] + alphawin * (rewsens*(1-rewardA[t]) - QB_reward[t]) # value learning for rewards for B
        QB_punish[t+1] = QB_punish[t] + alphaloss * (punsens*(1-punishA[t]) - QB_punish[t]) 
        QA_reward[t+1] = QA_reward[t] #don't update unchosen
        QA_punish[t+1] = QA_punish[t] #don't update unchosen
        
      }
    }
  }
  
    
  return (choices)
}
