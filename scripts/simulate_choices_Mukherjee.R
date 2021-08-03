simulate_choices_Mukherjee <- function(parameters,task,gng){
  
  library('boot')
  
  list2env(parameters,environment())
  
  ProbA <- rep(100, nrow(task)) #initialise probability of choosing shape A at 0.5
  choices <- rep(NA, nrow(task))
  randoms<- runif(nrow(task))
  
  bias_a<-0
  bias_f<-0
  persev_a<-(persev_a_reward+persev_a_punish)/2
  persev_f<-(persev_f_reward+persev_f_punish)/2
  
  if (gng==0){
    outcomeA<-task[,1]-task[,2]
    outcomeB<-(1-task[,1])-(1-task[,2])
    Q_f<-rep(0,2)
    Q_a<-rep(0,2)
    value<-0
  } else {
    Q_f_go <- rep(0,4)
    Q_f_nogo<- rep(0,4)
    Q_a_go <- rep(0,4)
    Q_a_nogo <- rep(0,4)
    ProbGo <-rep(NA,nrow(task))
    value<-rep(0,4)
    stim<-task$stim
  }
  

  for (t in 1:nrow(task)){
    #here '1' is choosing shape A (i.e. the only one we have simulated, really)
    
    if (gng==1){
      
      if (stim[t]==1|stim[t]==3){ #stimulus A or C are rewarding
        beta_f<-beta_f_reward
        beta_a<-beta_a_reward
        alpha_f<-alpha_f_reward
        persev_a<-persev_a_reward
        persev_f<-persev_f_reward
      } else {
        beta_f<-beta_f_punish
        beta_a<-beta_a_punish
        alpha_f<-alpha_f_punish
        persev_a<-persev_a_punish
        persev_f<-persev_f_punish
      }
      
      if(length(which(task$stim[1:(t-1)]==stim[t]))!=0&t>1){
        sticky<-c(choices[max(which(task$stim[1:t-1]==stim[t]))]==1,choices[max(which(task$stim[1:t-1]==stim[t]))]==2)
      } else {
        sticky<-c(0,0)
      }
      
      
      value[stim[t]] = beta_f*(Q_f_go[stim[t]] - Q_f_nogo[stim[t]]) +
        beta_a*(Q_a_go[stim[t]] - Q_a_nogo[stim[t]]) + 
        persev_f*sticky[1] + persev_a*sticky[1] #only interested on whether they chose go last time 
      
      ProbGo[t] = inv.logit(value[stim[t]])
      choices[t]<- ifelse(ProbGo[t]>randoms[t], 1, 2)
      
      if (choices[t]==1){
        Q_f_go[stim[t]]<-Q_f_go[stim[t]] + alpha_f*(task$go_outcome[t] - Q_f_go[stim[t]])
        Q_a_go[stim[t]]<-Q_a_go[stim[t]] + (task$go_outcome[t] - Q_a_go[stim[t]])
      } else if (choices[t]==2){
        Q_f_nogo[stim[t]]<-Q_f_nogo[stim[t]] + alpha_f*(task$nogo_outcome[t] - Q_f_nogo[stim[t]])
        Q_a_nogo[stim[t]]<-Q_a_nogo[stim[t]] + (task$nogo_outcome[t] - Q_a_nogo[stim[t]])
      }
        
    } else {
      
      sticky=ifelse(t=1,0,ifelse(choices[t-1]==1,1,0))
      
      beta_f<-(beta_f_reward+beta_f_punish)/2
      beta_a<-(beta_a_reward+beta_a_punish)/2
      
      value = beta_f*(Q_f[1] - Q_f[2]) + beta_a*(Q_a[1] - Q_a[2]) +
        persev_f*sticky + persev_a*sticky

      ProbA[t] = inv.logit(value)
      choices[t]<- ifelse(ProbA[t]>randoms[t], 1, 2)
      
      if ((outcomeA[t]>0&choices[t]==1)|(outcomeB[t]>0&choices[t]==2)){ #stimulus A or C are rewarding
        alpha_f<-alpha_f_reward
      } else {
        alpha_f<-alpha_f_punish
      }
      
      if (choices[t]==1){
        outcome=outcomeA[t]
      } else {
        outcome=outcomeB[t]
      }
      
      Q_f[choices[t]]<-Q_f[choices[t]] + alpha_f*(outcome - Q_f[choices[t]])
      Q_a[choices[t]]<-Q_a[choices[t]] + alpha_f*(outcome - Q_a[choices[t]]) 
    }
    
  }
  
  
  return (choices)
}
