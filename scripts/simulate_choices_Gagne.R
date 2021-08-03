simulate_choices_Gagne <- function(parameters,task,gng){
  
  list2env(parameters,environment())
  
  if (gng==1){
    stim=task$stim
    go_outcome=task$go_outcome
    nogo_outcome=task$nogo_outcome
  }
  
  QA <- rep(0, nrow(task)+1) 
  valueA<-0
  kA <- rep(0.5,nrow(task)+1)
  
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
  
  choices[1]<- ifelse(0.5>randoms[1], 1, 2) #first trial drawn at random
  
  for (t in 2:nrow(task)){
    
    #here '1' is choosing shape A (i.e. the only one we have simulated, really)
    
    if (gng==1){
      
      if (task$stim[t]==1|task$stim[t]==3){ #stimulus A or C (rew)
        omegak=rew_omegak
        sensitivity=rew_sensitivity
        if (lastoutcome[stim[t]]==1){
          win_learning=rew_lr_good
          reward=1
          lambda=rew_lambda_good
          omega=rew_omega_good
        } else {
          win_learning=rew_lr_bad
          reward=0
          lambda=rew_lambda_bad
          omega=rew_omega_bad
        }
      } else {
        omegak=loss_omegak
        sensitivity=loss_sensitivity
        if (lastoutcome[stim[t]]==1){
          learning=loss_lr_good
          outcome=0
          lambda=loss_lambda_good
          omega=loss_omega_good
        } else {
          learning=loss_lr_bad
          outcome=-1
          lambda=loss_lambda_bad
          omega=loss_omega_bad
        }
        
      }

        Q[stim[t]] = Q[stim[t]] + learning*(outcome-Q[stim[t]]) #no loss learning
        
        value[stim[t]] = lambda*(Q[stim[t]]-(1-Q[stim[t]]))+(1-lambda)*(1-1)^sensitivity
        
        k[stim[t]] = k[stim[t]] + eta*(choices[t-1]-k[stim[t]])
        
        ProbGo[t] = 1/(1+exp(-1*(omega*value[stim[t]]+omegak*(k[stim[t]]-(1-k[stim[t]])))))
        
        choices[t]<- ifelse(ProbGo[t]>randoms[t], 1, 2)
        
        #this part encodes whether the last outcome for that stimulus was good or bad
        if (stim[t]==1|stim[t]==3){
          if (choices[t]==1&go_outcome[t]==0){
            lastoutcome[stim[t]]==-1
          } else if (choices[t]==2&nogo_outcome[t]==0){
            lastoutcome[stim[t]]==-1
          } else {
            lastoutcome[stim[t]]==1
          }
        } else if (stim[t]==2|stim[t]==4){
          if (choices[t]==1&go_outcome[t]==0){
            lastoutcome[stim[t]]==1
          } else if (choices[t]==2&nogo_outcome[t]==0){
            lastoutcome[stim[t]]==1
          } else {
            lastoutcome[stim[t]]==-1
          }
        }
          
        
    } else {
      if (choices[t-1]==1){
        outcome=rewardA[t-1]-punishA[t-1]
      } else {
        outcome=(1-rewardA[t-1])-(1-punishA[t]-1)
      }
      
      if (outcome==1){
        learning=lr_good
        lambda=lambda_good
        omega=omega_good
      } else {
        learning=lr_bad
        lambda=lambda_bad
        omega=omega_bad
      }
      
        
      QA[t] = QA[t-1] + learning*(outcome-QA[t-1])
    
      valueA = lambda*(QA[t]-(1-QA[t]))+(1-lambda)*(1-1)^sensitivity
      
      choseA=ifelse(choices[t-1]==1,1,0)
      
      kA[t] = kA[t-1] + eta*(choseA-kA[t-1])
      
      ProbA[t] = 1/(1+exp(-1*(omega*valueA+omegak*(kA[t]-(1-kA[t])))))
      
      choices[t]<- ifelse(ProbA[t]>randoms[t], 1, 2)
    } 
  }
  return (choices)
}
