simulate_choices_SARSA <- function(alpha,beta,task,gng){
  
  
  ProbA <- rep(0.5, nrow(task)) #initialise probability of choosing shape A at 0.5
  choices <- rep(NA, nrow(task))
  randoms<- runif(nrow(task))
  
  QA<-rep(NA,nrow(task))
  QB<-rep(NA,nrow(task))
  
  QA[1]<-0
  QB[1]<-0
  
  Q_go<-matrix(data=NA,nrow=nrow(task)+1,ncol=4)
  Q_nogo<-matrix(data=NA,nrow=nrow(task)+1,ncol=4)
  
  Q_go[1,]<-rep(0,4)
  Q_nogo[1,]<-rep(0,4)
  
  ProbGo <-rep(NA,nrow(task))
  
  rewardA <- task[,1] #reward (the m terms are essentially binary indicators here)
  punishA <- task[,2] #punishment 
  
  
  for (t in 1:nrow(task)){
    learning=alpha
    
    #here '1' is choosing shape A (i.e. the only one we have simulated, really)
    
    if (gng==1){
      stim <- task$stim[t]
      if (Q_go[t,stim]*beta>700|Q_nogo[t,stim]*beta< -700){
        ProbGo[t]=1
      } else if (Q_go[t,stim]*beta< -700|Q_nogo[t,stim]*beta>700){
        ProbGo[t]=0
      } else {
        ProbGo[t] = exp(Q_go[t,stim]*beta)/(exp(Q_go[t,stim]*beta)+exp(Q_nogo[t,stim]*beta))
      }
      choices[t]<- ifelse(ProbGo[t]>randoms[t], 1, 2)
      Q_go[t+1,]<-Q_go[t,]#updates all to previous value - just copies over
      Q_nogo[t+1,]<-Q_nogo[t,]#updates all to previous value - copies over
      if (choices[t]==1){
        Q_go[t+1,stim] = Q_go[t,stim] + learning * (task$go_outcome[t] + max(c(Q_go[t,stim],Q_nogo[t,stim])) - Q_go[t,stim]) #unless that stimulus
      } else { #if nogo
        Q_nogo[t+1,stim] = Q_nogo[t,stim] + learning * (task$nogo_outcome[t] + max(c(Q_go[t,stim],Q_nogo[t,stim])) - Q_nogo[t,stim])
        
      }
      
      #barplot(c(QA_reward_go,QA_reward_nogo,QB_punish_go,QB_punish_nogo,QC_reward_go,QC_reward_nogo,QD_punish_go,QD_punish_nogo))
    } else {
      
      if(QA[t]*beta>700){
        ProbA[t]=1
      } else if (QA[t]*beta< -700){
        ProbA[t]=0
      } else {
        ProbA[t] = exp(QA[t]*beta)/(exp(QA[t]*beta) + exp(QB[t]*beta))
      }
      
      
      choices[t]<- ifelse(ProbA[t]>randoms[t], 1, 2)
      outcomeA<-rewardA-punishA
      outcomeB<-(1-rewardA)-(1-punishA)
      

      
      if (choices[t]==1){
        
        QA[t+1] = QA[t] + learning * (outcomeA[t] + max(c(QA[t],QB[t])) - QA[t]) # value learning for rewards for A
        QB[t+1] = QB[t] #don't update unchosen
        
      } else {  
        
        QB[t+1] = QB[t] + learning * (outcomeB[t]+ max(c(QA[t],QB[t])) - QB[t]) # value learning for rewards for B
        QA[t+1] = QA[t] #don't update unchosen
      }
    }
  }
  
  
  return (choices)
}
