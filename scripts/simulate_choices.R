simulate_choices <- function(win_alpha,loss_alpha,betawin,betaloss,task){
  if(loss_alpha=='empty'){alphaloss<-alphaawin} #when no betaloss parameter is used
  if(betaloss=='empty'){betaloss<-betawin} #when no betaloss parameter is used
  QA_reward <- rep(0, nrow(task)+1) 
  QA_punish <- rep(0, nrow(task)+1) 
  QB_reward <- rep(0, nrow(task)+1)
  QB_punish <- rep(0, nrow(task)+1)
  ProbA <- rep(0.5, nrow(task)) #initialise probability of choosing shape A at 0.5
  choices <- rep(100, nrow(task))
  randoms <- rep(NA, nrow(task))
  
  rewardA <- task[,1] #reward (the m terms are essentially binary indicators here)
  punishA <- task[,2] #punishment 
  
  for (t in 1:nrow(task)){
    #here '1' is choosing shape A (i.e. the only one we have simulated, really)
    
    #temporary fix for the fact can't do exp(700+)
    if (QA_reward[t]/betawin - QA_punish[t]/betaloss > 709){ProbA[t]=1
    } else if (QA_reward[t]/betawin - QA_punish[t]/betaloss < - 709){ProbA[t]=0
    } else {
      ProbA[t] = exp(QA_reward[t]/betawin - QA_punish[t]/betaloss)/
        (exp(QA_reward[t]/betawin - QA_punish[t]/betaloss) + exp(QB_reward[t]/betawin - QB_punish[t]/betaloss))
    }
    
    
    randoms[t]<- runif(1)
    choices[t]<- ifelse(ProbA[t]>randoms[t], 1, 2)
    
   
    if (choices[t]==1){
      
      QA_reward[t+1] = QA_reward[t] + win_alpha * (rewardA[t] - QA_reward[t]) # value learning for rewards for A
      QA_punish[t+1] = QA_punish[t] + loss_alpha * (punishA[t] - QA_punish[t]) # value learning for punishments for A
      QB_reward[t+1] = QB_reward[t] #don't update unchosen
      QB_punish[t+1] = QB_punish[t] #don't update unchosen
  
    } else {  
    
      QB_reward[t+1] = QB_reward[t] + win_alpha * ((1-rewardA[t]) - QB_reward[t]) # value learning for rewards for B
      QB_punish[t+1] = QB_punish[t] + loss_alpha * ((1-punishA[t]) - QB_punish[t]) 
      QA_reward[t+1] = QA_reward[t] #don't update unchosen
      QA_punish[t+1] = QA_punish[t] #don't update unchosen
    
    }
  }
    
  return (choices)
}
