simulate_choices_Frey <- function(alpha,tau,choice_bias,outcome_valuation,task,gng){
  
  choices <- rep(NA, nrow(task))
  randoms<- runif(nrow(task))
  
  rewards<-1-outcome_valuation
  losses<-(-1)*outcome_valuation
  neutral<-(1-outcome_valuation)-((1-outcome_valuation)-(-outcome_valuation))/2
  
  if (gng==0){
    QA<-rep(NA,nrow(task))
    QB<-rep(NA,nrow(task))
    QA[1]<-0
    QB[1]<-0
    
    rewardA <- task[,1] #reward (the m terms are essentially binary indicators here)
    rewardB <- 1-rewardA
    punishA <- task[,2] #punishment 
    punishB <- 1-punishA
    
    outcomeA<- rewardA - punishA
    outcomeB<- rewardB - punishB
    
    outcomeA <- ifelse(outcomeA==1,rewards,
                       ifelse(outcomeA==-1,losses,neutral)) #recodes using outcome valuation
    outcomeB <- ifelse(outcomeB==1,rewards,
                       ifelse(outcomeB==-1,losses,neutral))
    
    ProbA <- rep(0.5, nrow(task)) #initialise probability of choosing shape A at 0.5
    
    
  } else {
    Q_go<-matrix(data=NA,nrow=nrow(task)+1,ncol=4)
    Q_nogo<-matrix(data=NA,nrow=nrow(task)+1,ncol=4)
    
    Q_go[1,]<-rep(0,4)
    Q_nogo[1,]<-rep(0,4)
    
    task$go_outcome<-ifelse(task$go_outcome==1,rewards,
                            ifelse(task$go_outcome==-1,losses,neutral)) #revalues using outcome valuation
    task$nogo_outcome<-ifelse(task$nogo_outcome==1,rewards,
                              ifelse(task$nogo_outcome==-1,losses,neutral))
    ProbGo <-rep(NA,nrow(task))
  }
  


  
  for (t in 1:nrow(task)){
    #here '1' is choosing shape A (i.e. the only one we have simulated, really)
    if (gng==1){
      stim <- task$stim[t]
      
      if(length(which(task$stim[1:(t-1)]==stim))!=0&t>1){
        sticky<-c(choices[max(which(task$stim[1:t-1]==stim))]==1,choices[max(which(task$stim[1:t-1]==stim))]==2)
      } else {
        sticky<-c(0,0)
      }

      
      if((Q_go[t,stim]+sticky[1]*choice_bias)/tau >709){ProbGo[t]=1
      } else if((Q_go[t,stim]+sticky[1]*choice_bias)/tau < -709){ProbGo[t]=0
      } else if((Q_nogo[t,stim]+sticky[2]*choice_bias)/tau > 709){ProbGo[t]=0
      } else if((Q_nogo[t,stim]+sticky[2]*choice_bias)/tau < - 709){ProbGo[t]=1
      } else {
        
        ProbGo[t] = exp((Q_go[t,stim]+sticky[1]*choice_bias)/tau)/
          (exp((Q_go[t,stim]+sticky[1]*choice_bias)/tau) + exp((Q_nogo[t,stim]+sticky[2]*choice_bias)/tau))
        
      }
      
      choices[t]<- ifelse(ProbGo[t]>randoms[t], 1, 2)
      Q_go[t+1,]<-Q_go[t,]#updates all to previous value - just copies over
      Q_nogo[t+1,]<-Q_nogo[t,]#updates all to previous value - copies over
      if (choices[t]==1){
        pe = task$go_outcome[t]  - Q_go[t,stim]
        Q_go[t+1,stim] = Q_go[t,stim] + alpha * max(0,pe) + alpha * min(0,pe) 
      } else { #if nogo
        pe = task$nogo_outcome[t]  - Q_nogo[t,stim]
        Q_nogo[t+1,stim] = Q_nogo[t,stim] + alpha * max(0,pe) + alpha * min(0,pe)
        
      }
      
      #barplot(c(QA_reward_go,QA_reward_nogo,QB_punish_go,QB_punish_nogo,QC_reward_go,QC_reward_nogo,QD_punish_go,QD_punish_nogo))
    } else {
      
      sticky=ifelse(t=1,0,ifelse(choices[t-1]==1,1,0))
      
      if((QA[t]+sticky*choice_bias)/tau >709){ProbA[t]=1
      } else if((QA[t]+sticky*choice_bias)/tau< -709){ProbA[t]=0
      } else if((QB[t]+(1-sticky)*choice_bias)/tau > 709){ProbA[t]=0
      } else if((QB[t]+(1-sticky)*choice_bias)/tau < - 709){ProbA[t]=1
      } else {
        
        ProbA[t] = exp((QA[t]+sticky*choice_bias)/tau)/
          (exp((QA[t]+sticky*choice_bias)/tau) + exp((QB[t]+(1-sticky)*choice_bias)/tau))
        
      }
      
      
      choices[t]<- ifelse(ProbA[t]>randoms[t], 1, 2)
      outcomeA<-rewardA+punishA
      outcomeB<-rewardB+punishB
      
      
      if (choices[t]==1){
        pe = outcomeA[t] - QA[t]
        QA[t+1] = QA[t] + alpha * max(0,pe) + alpha * min(0,pe) # value alpha for rewards for A
        QB[t+1] = QB[t] #don't update unchosen
        
      } else {  
        pe = outcomeB[t] - QB[t]
        QB[t+1] = QB[t] + alpha * max(0,pe) + alpha * min(0,pe) # value alpha for rewards for B
        QA[t+1] = QA[t] #don't update unchosen
      }
    }
  }
  
  
  return (choices)
}
