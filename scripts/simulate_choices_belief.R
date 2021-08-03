simulate_choices_belief <- function(epsilon,gamma,belief,beta,q0,task,gng){
  
  
  ProbA <- rep(0.5, nrow(task)) #initialise probability of choosing shape A at 0.5
  choices <- rep(NA, nrow(task))
  randoms<- runif(nrow(task))
  
  weight<-matrix(NA,2) #weights for go and nogo
  
  QA<-rep(NA,nrow(task))
  QB<-rep(NA,nrow(task))
  
  QA[1]<-q0
  QB[1]<-q0
  
  Q_go<-matrix(data=NA,nrow=nrow(task)+1,ncol=4)
  Q_nogo<-matrix(data=NA,nrow=nrow(task)+1,ncol=4)
  
  Q_go[1,]<-rep(q0,4)
  Q_nogo[1,]<-rep(q0,4)
  
  ProbGo <-rep(NA,nrow(task))
  
  rewardA <- task[,1] #reward (the m terms are essentially binary indicators here)
  punishA <- task[,2] #punishment 
  
  
  for (t in 1:nrow(task)){
    learning=epsilon
    
    #here '1' is choosing shape A (i.e. the only one we have simulated, really)
    
    if (gng==1){
    stim <- task$stim[t]
      weight[1]=gamma*0 + belief*(Q_go[t,stim])+(1-belief)*(Q_nogo[t,c(stim)]) #weight for go
      weight[2]=gamma*0 + belief*(Q_nogo[t,stim])+(1-belief)*(Q_go[t,c(stim)]) #weight for nogo
      ProbGo[t] = 1/(1+exp(-(weight[1] - weight[2])))
      choices[t]<- ifelse(ProbGo[t]>randoms[t], 1, 2)
      Q_go[t+1,]<-Q_go[t,]#updates all to previous value - just copies over
      Q_nogo[t+1,]<-Q_nogo[t,]#updates all to previous value - copies over
      if (choices[t]==1){
        Q_go[t+1,stim] = Q_go[t,stim] + learning * (beta*task$go_outcome[t] - Q_go[t,stim]) #unless that stimulus
      } else { #if nogo
        Q_nogo[t+1,stim] = Q_nogo[t,stim] + learning * (beta*task$nogo_outcome[t] - Q_nogo[t,stim])
        
      }
      
      #barplot(c(QA_reward_go,QA_reward_nogo,QB_punish_go,QB_punish_nogo,QC_reward_go,QC_reward_nogo,QD_punish_go,QD_punish_nogo))
    } else {
      
      
      weight[1]=gamma*0 + belief*(QA[t])+(1-belief)*(QB[t]) #weight for go
      weight[2]=gamma*0 + belief*(QB[t])+(1-belief)*(QA[t]) #weight for nogo
      ProbA[t] = 1/(1+exp(-(weight[1] - weight[2])))
      
      
      choices[t]<- ifelse(ProbA[t]>randoms[t], 1, 2)
      outcomeA<-rewardA-punishA
      outcomeB<-(1-rewardA)-(1-punishA)
      

      
      if (choices[t]==1){
        
        QA[t+1] = QA[t] + learning * (beta*outcomeA[t]  - QA[t]) # value learning for rewards for A
        QB[t+1] = QB[t] #don't update unchosen
        
      } else {  
        
        QB[t+1] = QB[t] + learning * (beta*outcomeB[t] - QB[t]) # value learning for rewards for B
        QA[t+1] = QA[t] #don't update unchosen
      }
    }
  }
  
  
  return (choices)
}
