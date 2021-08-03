simulate_choices_Moutoussis <- function(alpha,win_sens,loss_sens,Pavlovian_bias,lapse,action_bias,task,gng){
  QA <- rep(0, nrow(task)) #initialise at 0 - agnostic towards rewards and losses
  QB <- rep(0, nrow(task)) #initialise at 0 - agnostic towards rewards and losses
  if (gng==0){
    value <- rep(0, nrow(task)) #initialise at 0 - agnostic towards rewards and losses
  }
  ProbA <- rep(0.5, nrow(task)) #initialise probability of choosing shape A at 0.5
  randoms <- runif(nrow(task))
  choices <- rep(100, nrow(task))
  qA <- rep(0, nrow(task))
  qB <- rep(0, nrow(task))
  
  rewardA <- task[,1] #reward
  rewardB <- 1-task[,1]
  punishA <- task[,2] #punishment 
  punishB <- 1-task[,2]
  outcomeA <- rewardA-punishA
  outcomeB <- rewardB-punishB
  
  if (gng==1){
    QA_go<-0
    QA_nogo<-0
    QB_go<-0
    QB_nogo<-0
    QC_go<-0
    QC_nogo<-0
    QD_go<-0
    QD_nogo<-0
    valueA<-0
    valueB<-0
    valueC<-0
    valueD<-0
    ProbGo <- rep(100,nrow(task))
  }

  
  
  for (t in 1:nrow(task)){
    #here '1' is choosing shape A (i.e. the only one we have simulated, really)
    if (gng==1){
      #Both the bias coefficients assume zero values unless Vt(st)>0 (for both) andat =Go≔1(for pav bias)
      if (task$stim[t]==1){
        if (valueA>0){
          qA_go=QA_go + action_bias + Pavlovian_bias*valueA #action bias and pavlovian bias when go and value >0
          qA_nogo=QA_nogo + Pavlovian_bias*valueA #no action bias as not go, but is Pav bias
        } else {
          qA_go=QA_go #neither when value <0 
          qA_nogo=QA_nogo #neither when value <0
        }
        ProbGo[t] = (1-lapse)*exp(qA_go)/(exp(qA_go) + exp(qA_nogo)) + lapse/2
        choices[t]<- ifelse(ProbGo[t]>randoms[t], 1, 2)
        if (choices[t]==1){
          QA_go = QA_go + alpha * (win_sens*task$go_outcome[t] - QA_go) #win sense as always either 1 or 0 for this stim
          # value learning for the state - not specific to action 
          # except in that it is updated based on the actual outcome they get given their choice
          valueA= valueA + alpha * (win_sens*task$go_outcome[t] - valueA)
        } else if(choices[t]==2){
          QA_nogo = QA_nogo + alpha * (win_sens*task$nogo_outcome[t] - QA_nogo)
          # value learning for the state - not specific to action 
          # except in that it is updated based on the actual outcome they get given their choice
          valueA= valueA + alpha * (win_sens*task$nogo_outcome[t] - valueA)
        }
        
      }  else if (task$stim[t]==2){
        if (valueB>0){
          qB_go=QB_go + action_bias + Pavlovian_bias*valueB #action bias and pavlovian bias when go and value >0
          qB_nogo=QB_nogo + Pavlovian_bias*valueB #no action bias as not go, but is Pav bias
        } else {
          qB_go=QB_go #neither when value <0
          qB_nogo=QB_nogo #neither when value <0
        }
        ProbGo[t] = (1-lapse)*exp(qB_go)/(exp(qB_go) + exp(qB_nogo)) + lapse/2
        choices[t]<- ifelse(ProbGo[t]>randoms[t], 1, 2)
        if (choices[t]==1){
          QB_go = QB_go + alpha * (loss_sens*task$go_outcome[t] - QB_go) 
          # value learning for the state - not specific to action 
          # except in that it is updated based on the actual outcome they get given their choice
          valueB= valueB + alpha * (loss_sens*task$go_outcome[t] - valueB)
        } else if(choices[t]==2){
          QB_nogo = QB_nogo + alpha * (loss_sens*task$nogo_outcome[t] - QB_nogo)
          # value learning for the state - not specific to action 
          # except in that it is updated based on the actual outcome they get given their choice
          valueB= valueB + alpha * (loss_sens*task$nogo_outcome[t] - valueB)
        }
        
      } else if (task$stim[t]==3){
        if (valueC>0){
          qC_go=QC_go + action_bias + Pavlovian_bias*valueC #action bias and pavlovian bias when go and value >0
          qC_nogo=QC_nogo + Pavlovian_bias*valueC #no action bias as not go, but is Pav bias
        } else {
          qC_go=QC_go #neither when value <0 
          qC_nogo=QC_nogo #neither when value <0
        }
        ProbGo[t] = (1-lapse)*exp(qC_go)/(exp(qC_go) + exp(qC_nogo)) + lapse/2
        choices[t]<- ifelse(ProbGo[t]>randoms[t], 1, 2)
        if (choices[t]==1){
          QC_go = QC_go + alpha * (win_sens*task$go_outcome[t] - QC_go)
          # value learning for the state - not specific to action 
          # except in that it is updated based on the actual outcome they get given their choice
          valueC= valueC + alpha * (win_sens*task$go_outcome[t] - valueC)
        } else if(choices[t]==2){
          QC_nogo = QC_nogo + alpha * (win_sens*task$nogo_outcome[t] - QC_nogo)
          # value learning for the state - not specific to action 
          # except in that it is updated based on the actual outcome they get given their choice
          valueC= valueC + alpha * (win_sens*task$nogo_outcome[t] - valueC)
        }
        
      } else if (task$stim[t]==4){
        if (valueD>0){
          qD_go=QD_go + action_bias + Pavlovian_bias*valueD #action bias and pavlovian bias when go and value >0
          qD_nogo=QD_nogo + Pavlovian_bias*valueD #no action bias as not go, but is Pav bias
        } else {
          qD_go=QD_go #neither when value <0 
          qD_nogo=QD_nogo #neither when value <0
        }
        ProbGo[t] = (1-lapse)*exp(qD_go)/(exp(qD_go) + exp(qD_nogo)) + lapse/2
        choices[t]<- ifelse(ProbGo[t]>randoms[t], 1, 2)
        if (choices[t]==1){
          QD_go = QD_go + alpha * (loss_sens*task$go_outcome[t] - QD_go)
          # value learning for the state - not specific to action 
          # except in that it is updated based on the actual outcome they get given their choice
          valueD= valueD + alpha * (loss_sens*task$go_outcome[t] - valueD)
        } else if(choices[t]==2){
          QD_nogo = QD_nogo + alpha * (loss_sens*task$nogo_outcome[t] - QD_nogo)
          # value learning for the state - not specific to action 
          # except in that it is updated based on the actual outcome they get given their choice
          valueD= valueD + alpha * (loss_sens*task$nogo_outcome[t] - valueD)
        }
      }
      
      #barplot(c(QA_go,QA_nogo,QB_go,QB_nogo,QC_go,QC_nogo,QD_go,QD_nogo))
    } else {
    
      # uses action bias as all actions are go 
      # q values can't be dependent on choices as this can't be calculated without them
      if (value[t]>0){
        qA[t]=QA[t] + action_bias + Pavlovian_bias*value[t]
        qB[t]=QB[t] + action_bias + Pavlovian_bias*value[t]
      } else {
        qA[t]=QA[t]
        qB[t]=QB[t]
      }
  
      ProbA[t] = (1-lapse)*exp(qA[t])/(exp(qA[t]) + exp(qB[t])) + lapse/2
    
      choices[t]<- ifelse(ProbA[t]>randoms[t], 1, 2)
      
      
      if (choices[t]==1){
        if (outcomeA[t]==1){sens<-win_sens} else if (outcomeA[t]==-1){sens<-loss_sens} else {sens=1}
        
        QA[t+1] = QA[t] + alpha * (sens*outcomeA[t] - QA[t]) # Q learning for A
        value[t+1] = value[t] + alpha * (sens*outcomeA[t] - value[t]) # value learning for 'state'
        QB[t+1] = QB[t] #don't update unchosen
        
      } else if (choices[t]==2){
        if (outcomeB[t]==1){sens<-win_sens} else if (outcomeB[t]==-1){sens<-loss_sens} else {sens=1}
        
        QB[t+1] = QB[t] + alpha * (sens*outcomeB[t] - QB[t])# Q learning for A
        value[t+1] = value[t] + alpha * (sens*outcomeB[t] - value[t]) # value learning for 'state'
        QA[t+1] = QA[t] #don't update unchosen
        
      } 
    }
  }
  return (choices)
}