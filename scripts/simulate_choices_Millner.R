simulate_choices_Millner <- function(parameters, task, gng){
  #joint RL-DDM model
  
  list2env(parameters,environment())
  
  library('RWiener')
  
  QA <- rep(0, nrow(task)+1) #initialise at 0 - agnostic towards rewards and losses
  QB <- rep(0, nrow(task)+1) #initialise at 0 - agnostic towards rewards and losses
  outcomeA <- rep(nrow(task))
  outcomeB <- rep(nrow(task))
  
  ProbA <- rep(0.5, nrow(task)) #initialise probability of choosing shape A at 0.5
  ProbB <- rep(0.5, nrow(task))
  choices <- rep(100, nrow(task))
  randoms <- runif(nrow(task))
  muA <- rep(NA, nrow(task))
  #muB <- rep(NA, nrow(task))
  

  rewardA <- task[,1] #reward (the m terms are essentially binary indicators here)
  punishA <- task[,2] #punishment 
  rewardB <- 1-task[,1]
  punishB <- 1-task[,2]
  
  outcomeA <- rewardA - punishA
  outcomeB <- rewardB - punishB
  
  ProbGo<-rep(100,nrow(task))
  QA_go<-0
  QA_nogo<-0
  QB_go<-0
  QB_nogo<-0
  QC_go<-0
  QC_nogo<-0
  QD_go<-0
  QD_nogo<-0
  
  for (t in 1:nrow(task)){

    if (gng==1){
      if (task$stim[t]==1){
        muA = b0 + b1*(QA_go-QA_nogo)
        ProbGo[t] <-pwiener(10, alpha=omega, tau=tau, beta=w, delta=muA,resp='upper')
        choices[t] <- ifelse(ProbGo[t]>randoms[t],1,2)
        if (choices[t]==1){
          QA_go<-QA_go + alpha * (task$go_outcome[t] - QA_go)
        } else if (choices[t]==2){
          QA_nogo<-QA_nogo + alpha * (task$nogo_outcome[t] - QA_nogo)
        }
        
      } else if (task$stim[t]==2){
        muB = b0 + b1*(QB_go-QB_nogo)
        ProbGo[t] <-pwiener(10, alpha=omega, tau=tau, beta=w, delta=muB,resp='upper')
        choices[t] <- ifelse(ProbGo[t]>randoms[t],1,2)
        if (choices[t]==1){
          QB_go<-QB_go + alpha * (task$go_outcome[t] - QB_go)
        } else if (choices[t]==2){
          QB_nogo<-QB_nogo + alpha * (task$nogo_outcome[t] - QB_nogo)
        }
      } else if (task$stim[t]==3){
        muC = b0 + b1*(QC_go-QC_nogo)
        ProbGo[t] <-pwiener(10, alpha=omega, tau=tau, beta=w, delta=muC,resp='upper')
        choices[t] <- ifelse(ProbGo[t]>randoms[t],1,2)
        if (choices[t]==1){
          QC_go<-QC_go + alpha * (task$go_outcome[t] - QC_go)
        } else if (choices[t]==2){
          QC_nogo<-QC_nogo + alpha * (task$nogo_outcome[t] - QC_nogo)
        }
      } else if (task$stim[t]==4){
        muD = b0 + b1*(QD_go-QD_nogo)
        ProbGo[t] <-pwiener(10, alpha=omega, tau=tau, beta=w, delta=muD,resp='upper')
        choices[t] <- ifelse(ProbGo[t]>randoms[t],1,2)
        if (choices[t]==1){
          QD_go<-QD_go + alpha * (task$go_outcome[t] - QD_go)
        } else if (choices[t]==2){
          QD_nogo<-QD_nogo + alpha * (task$nogo_outcome[t] - QD_nogo)
        }
      }
      
      #barplot(c(QA_go,QA_nogo,QB_go,QB_nogo,QC_go,QC_nogo,QD_go,QD_nogo))
      
    } else if (gng==0){

    
    
      #w=z/omega where omega is boundary, z is starting point, so starting point (beta in RWiener) is w*omega
      #w is escape
      #w2 is avoid
      #hard to figure out which to use, expect that escape is best as they are making a choice 
      #to avoid a negative outcome, nothing negative has yet happened
    
      muA[t] = b0 + b1*(QA[t]-QB[t]) #mu[t] = b0+b1(QA(go) - QA(nogo)) -- this is irrelevant as all are go here
      #muB[t] = b0 + b1*(QB[t]) #mu[t] = b0+b1(QA(go) - QA(nogo)) -- this is irrelevant as all are go here
      
  
      ProbA[t]<-pwiener(10, alpha=omega, tau=tau, beta=w, delta=muA[t],resp='upper')
      ProbB[t]<-pwiener(10, alpha=omega, tau=tau, beta=w, delta=muA[t],resp='lower')
  
      choices[t]<- ifelse(ProbA[t]>randoms[t],1,2)
      
  
      
      
      if (choices[t]==1){
        
        QA[t+1] = QA[t] + alpha * (outcomeA[t]-QA[t]) # value learning for rewards for A
        QB[t+1] = QB[t]
  
      } else if (choices[t]==2){  
        
        QB[t+1] = QB[t] + alpha * (outcomeB[t]-QB[t]) # value learning for rewards for B
        QA[t+1] = QA[t]
        
      }
    }
  }
  
  return (choices)
}
