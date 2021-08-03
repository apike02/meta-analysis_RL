simulate_choices_Mkrtchian <- function(win_alpha,loss_alpha,win_sens,loss_sens,win_pav,loss_pav,action_bias,lapse,task,gng){

  randoms <- runif(nrow(task))
  choices <- rep(100, nrow(task))
  

  if (gng==0){
    Q<-rep(0,2)
    q<-rep(0,2)
    value<-0
    rewardA <- task[,1] #reward (the m terms are essentially binary indicators here)
    rewardB <- 1-task[,1]
    punishA <- task[,2] #punishment 
    punishB <- 1-task[,2]
    
    outcomeA<-rewardA-rewardB
    outcomeB<-rewardA-rewardB
    
    stim<-vector()
    
    ProbA <- rep(0.5, nrow(task)) #initialise probability of choosing shape A at 0.5
    
    
  } else {
    stim=task$stim
    Q_go<-rep(0,4)
    q_go<-rep(0,4)
    Q_nogo<-rep(0,4)
    q_nogo<-rep(0,4)
    value<-rep(0,4)
    
    ProbGo<-rep(100,nrow(task))
  }
  
  for (t in 1:nrow(task)){
    
    if (gng==1){
      if (stim[t]==1|stim[t]==3){ #win stimuli 
        lr = win_alpha
        sens = win_sens
        pav = win_pav
      } else {
        lr = loss_alpha
        sens = loss_sens
        pav = loss_pav
      }
      q_go[stim[t]] = Q_go[stim[t]] + action_bias + pav*value[stim[t]] 
      q_nogo[stim[t]] = Q_nogo[stim[t]] #no biases for nogo
      ProbGo[t] = (1-lapse)*exp(q_go[stim[t]])/(exp(q_go[stim[t]]) + exp(q_nogo[stim[t]])) + lapse/2
      choices[t]<- ifelse(ProbGo[t]>randoms[t], 1, 2)
      if (choices[t]==1){
        Q_go[stim[t]] = Q_go[stim[t]] + lr * (sens*task$go_outcome[t] - Q_go[stim[t]]) # Q learning
        value[stim[t]] = value[stim[t]] + lr * (sens*task$go_outcome[t] - value[stim[t]])# value learning
      } else if (choices[t]==2){
        Q_nogo[stim[t]] = Q_nogo[stim[t]] + lr * (sens*task$nogo_outcome[t] - Q_nogo[stim[t]]) # Q learning 
        value[stim[t]] = value[stim[t]] + lr * (sens*task$nogo_outcome[t] - value[stim[t]])# value learning 
      }
        

    } else {
      
    
      #here '1' is choosing shape A (i.e. the only one we have simulated, really)
      
      if (value>0){
        pav=win_pav
      } else{
        pav=loss_pav
      }
      
      q=Q + action_bias + pav*value #always action bias as for other tasks can only go #should create a vector
      ProbA[t] = (1-lapse)*exp(q[1])/(exp(q[1]) + exp(q[2])) + lapse/2
      
    
      choices[t]<- ifelse(ProbA[t]>randoms[t], 1, 2)
      stim[t] <- choices[t]
      
      if (choices[t]==1){
        if (outcomeA[t]>0){
          sens<-win_sens
          lr<-win_alpha
        } else if (outcomeA[t]<=0){
          sens<-loss_sens
          lr<-loss_alpha
        } 
        
        Q[stim[t]] = Q[stim[t]] + lr * (sens*outcomeA[t] - Q[stim[t]]) # Q learning for A
        value= value + lr * (sens*outcomeA[t] - value)# value learning for A 
        
      } else {
        if (outcomeB[t]>0){
          sens<-win_sens
          lr<-win_alpha
        } else if (outcomeB[t]<=0){
          sens<-loss_sens
          lr<-loss_alpha
        } 
        
        Q[stim[t]] = Q[stim[t]] + lr * (sens*outcomeA[t] - Q[stim[t]]) # Q learning for B
        value = value+ lr * (sens*outcomeA[t] - value)# value learning for B
      }
    }
  }
  return (choices)
}
