simulate_choices_Lamba <- function(parameters,task,gng){
  
  list2env(parameters,environment())
  
  bias<-0.5 #set bias to neutral as this will have been task-specific
  
  if (gng==1){
    nstim=4
    stim=task$stim
    go_outcome=task$go_outcome
    nogo_outcome=task$nogo_outcome
  } else {
    nstim=1
  }
  
  mu<-matrix(NA,nstim,nrow(task)+1)
  alpha<-matrix(NA,nstim,nrow(task)+1)
  beta<-matrix(NA,nstim,nrow(task)+1)
  Prob<-matrix(NA,nstim,nrow(task)+1)
  entropy<-rep(0,nrow(task)+1)
  entropydelta<-rep(0,nrow(task)+1)

  
  mu[,1]<-1
  alpha[,1]<-1 #or get infinity dividing by 0, this also makes mean 1 on trial 1
  beta[,1]<-1
  
  
  ProbGo <-rep(NA,nrow(task))
  ProbA <- rep(NA,nrow(task))
  choices <- rep(NA, nrow(task))
  Q<- rep(0,4)
  value<-rep(0,4)
  k<-rep(0.5,4) #midway between choices
  lastoutcome<-rep(0,4) #starts off as 0, results in 'bad' params
  
  rewardA <- task[,1] #reward (the m terms are essentially binary indicators here)
  punishA <- task[,2] #punishment 
  
  randoms<- runif(nrow(task))
  task<-data.frame(task)
  
  
  for (t in 1:nrow(task)){
    
    #here '1' is choosing shape A (i.e. the only one we have simulated, really)
    
    if (gng==1){
      
      ProbGo[t]=exp(invtemp*mu[stim[t],t])/(exp(invtemp*mu[stim[t],t])+exp(invtemp*bias))
      
      choices[t]<- ifelse(ProbGo[t]>randoms[t], 1, 2)
      
      entropy[t]<- -1*(ProbGo[t]*log2(ProbGo[t])-(1-ProbGo[t])*log2(1-ProbGo[t]))
      
      if (t==1){
        entropydelta[t]=entropy[t]
      } else {
        entropydelta[t]=entropy[t]-entropy[t-1]
      }
      
      gammapos<-inv.logit(gamma0pos + gamma1pos*entropydelta[t])
      gammaneg<-inv.logit(gamma0neg + gamma1neg*entropydelta[t])
      
      
      alpha[,t+1]=alpha[,t]#updates all indiscriminately
      beta[,t+1]=beta[,t]
      
      if (stim[t]==1|stim[t]==3){ #positive prediction error is a gain
        if (choices[t]==1&go_outcome[t]==1||choices[t]==2&nogo_outcome[t]==1){
          alpha[stim[t],t]=alpha[stim[t],t]+1
        } else {
          beta[stim[t],t]=beta[stim[t],t]+1
        }
      } else if (stim[t]==2|stim[t]==4){ #positive prediction error is no loss
        if (choices[t]==1&go_outcome[t]==0||choices[t]==2&nogo_outcome[t]==0){
          alpha[stim[t],t]=alpha[stim[t],t]+1
        } else {
          beta[stim[t],t]=beta[stim[t],t]+1
        }
      }
      
      alpha[stim[t],t+1]=alpha[stim[t],t]*gamma0pos
      beta[stim[t],t+1]=beta[stim[t],t]*gamma0neg
      
      mu[,t+1] = alpha[,t+1]/(alpha[,t+1]+beta[,t+1])
      
    } else {

      outcomeA=rewardA-punishA
      outcomeB=(1-rewardA)-(1-punishA)
      

      Prob[,t]=exp(invtemp*mu[,t])/(exp(invtemp*mu[,t])+exp(invtemp*bias))
      
      choices[t]<- ifelse(Prob[,t]>randoms[t], 1, 2)
      
      stim=choices[t]
      
      entropy[t]<- -1*(Prob[,t]*log2(Prob[,t])-(1-Prob[,t])*log2(1-Prob[,t]))
      
      if (t==1){
        entropydelta[t]=entropy[t]
      } else {
        entropydelta[t]=entropy[t]-entropy[t-1]
      }

      gammapos<-inv.logit(gamma0pos + gamma1pos*entropydelta[t])
      gammaneg<-inv.logit(gamma0neg + gamma1neg*entropydelta[t])
      
      
      alpha[,t+1]=alpha[,t]#updates all indiscriminately
      beta[,t+1]=beta[,t]
      
      if (choices[t]==1&outcomeA[t]==1||choices[t]==2&outcomeB[t]==1){
        alpha[,t]=alpha[,t]+1
      } else {
        beta[,t]=beta[,t]+1
      }
      
      
      alpha[,t+1]=alpha[,t]*gamma0pos
      beta[,t+1]=beta[,t]*gamma0neg
      
      mu[,t+1] = alpha[,t+1]/(alpha[,t+1]+beta[,t+1])
    }

  }
  return (choices)
}
