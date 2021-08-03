simulate_choices_Brown <- function(alpha_win,alpha_loss,associability_pun,rewsens,punsens,
                                   decay_rew,decay_pun,beta_rew,beta_pun,task,gng){
  #initially I tried to do this for overall outcome, but in the paper it specifies that they had one parameter for 
  #each domain i.e. pos/neg
  #associability only in negative domain
  
  #some participants don't have the reward condition - copy this over
  
  if (is.na(alpha_win)){
    alpha_win=alpha_loss
    rewsens=punsens
    decay_rew=decay_pun
  }


  ProbA <- rep(0.5, nrow(task)) #initialise probability of choosing shape A at 0.5
  choices <- rep(100, nrow(task))
  randoms <- runif(nrow(task))
  
  rewardA <- task[,1] #reward (the m terms are essentially binary indicators here)
  punishA <- task[,2] #punishment 
  rewardB <- 1-task[,1]
  punishB <- 1-task[,2]
  outcomeA <-rewardA - punishA
  outcomeB <-rewardB - punishB
  
  if (gng==0){
    nstim=2
    QA<-rep(0,ntrials+1)
    QB<-rep(0,ntrials+1)
    kA_pun<-rep(1,ntrials+1)
    kB_pun<-rep(1,ntrials+1)
  } else {
    nstim=4
    Q_go<-rep(0,nstim)
    Q_nogo<-rep(0,nstim)
    k_go<-rep(1,nstim) 
    k_nogo<-rep(1,nstim)
    stim=task$stim
  }
  


  ProbGo<-rep(100,ntrials)

  for (t in 1:nrow(task)){
    
    if (gng==1){
      
      if (stim[t]==1|stim[t]==3){
        beta=beta_rew
        decay=decay_rew
        lr=alpha_win
        associability=0 #essentially means that k should just stay at 1 and be neutral
        sens=rewsens
      } else if (stim[t]==2|stim[t]==4){
        beta=beta_rew
        decay=decay_rew
        lr=alpha_win
        associability=associability_pun
        sens=punsens
      }
        
        ProbGo[t] = exp(beta*Q_go[stim[t]])/
          (exp(beta*Q_go[stim[t]])+exp(beta*Q_nogo[stim[t]]))
        choices[t]<- ifelse(ProbGo[t]>randoms[t], 1, 2)
        if (choices[t]==1){
          Q_go[stim[t]] = Q_go[stim[t]] + lr * k_go[stim[t]] * (sens*task$go_outcome[t]-Q_go[stim[t]]) # value learning for rewards for A
          Q_nogo[stim[t]] = decay*Q_nogo[stim[t]]
          k_go[stim[t]] = (1-associability)*k_go[stim[t]] + associability*abs(sens*task$go_outcome[t]-Q_go[stim[t]])
          if (k_go[stim[t]]<0.05){k_go[stim[t]]=0.05}
        } else {
          Q_nogo[stim[t]] = Q_nogo[stim[t]] + lr * k_nogo[stim[t]] * (sens*task$nogo_outcome[t]-Q_nogo[stim[t]]) # value learning for rewards for A
          Q_go[stim[t]] = decay*Q_go[stim[t]]
          k_nogo[stim[t]] = (1-associability)*k_nogo[stim[t]] + associability*abs(sens*task$nogo_outcome[t]-Q_nogo[stim[t]])
          if (k_go[stim[t]]<0.05){k_go[stim[t]]=0.05}
        }
      } else {
        
      beta<-(beta_rew+beta_pun)/2
    
      #with associability this can become numerically unstable
      #bound decay between 0 and 1!! 
      
      #ProbA[t] = exp(beta*QA[t])/(exp(beta*QA[t])+exp(beta*QB[t]))
      
      ProbA[t] = exp(beta*QA[t])/
        (exp(beta*QA[t])+exp(beta*QB[t]))
      
      randoms[t]<- runif(1)
      choices[t]<- ifelse(ProbA[t]>randoms[t], 1, 2)
      
      if (choices[t]==1){
        
        if (outcomeA[t]==1){
          lr=alpha_win
          sens=rewsens
          kA=1
          decay=decay_rew
        } else {
          lr=alpha_loss
          sens=punsens
          kA=kA_pun[t]
          decay=decay_pun
          kA_pun[t+1] = (1-associability_pun)*kA_pun[t] + associability_pun*abs(punsens*punishA[t]-QA[t])
          if (kA_pun[t+1]<0.05){kA_pun[t+1]=0.05}
          if (kA_pun[t+1]>1){kA_pun[t+1]=1}
          kB_pun[t+1] = kB_pun[t]
        }
        
        QA[t+1] = QA[t] + lr * kA * (sens*outcomeA[t]-QA[t]) # value learning for rewards for A
        QB[t+1] = decay*QB[t]
  
      } else if (choices[t]==2){
        if (outcomeB[t]==1){
          lr=alpha_win
          sens=rewsens
          kB=1
          decay=decay_rew
        } else {
          lr=alpha_loss
          sens=punsens
          kB=kB_pun[t]
          decay=decay_pun
          kB_pun[t+1] = (1-associability_pun)*kB_pun[t] + associability_pun*abs(punsens*punishB[t]-QB[t])
          if (kB_pun[t+1]<0.05){kB_pun[t+1]=0.05}
          if (kB_pun[t+1]>1){kB_pun[t+1]=1}
          kB_pun[t+1] = kB_pun[t]
        }
        
        QB[t+1] = QB[t] + lr * kB * (sens*outcomeB[t]-QB[t]) # value learning for rewards for A
        QA[t+1] = decay*QA[t]
        
      }
    }
  }
  
  return (choices)
}
