simulate_choices_2lr2s1lapse <- function(win_alpha,loss_alpha,rewsens,punsens,lapse,task,gng){
  QA_reward <- rep(0, nrow(task)+1) #initialise at 0 - agnostic towards rewards and losses
  QA_punish <- rep(0, nrow(task)+1) #initialise at 0 - agnostic towards rewards and losses
  QB_reward <- rep(0, nrow(task)+1) #initialise at 0 - agnostic towards rewards and losses
  QB_punish <- rep(0, nrow(task)+1) #initialise at 0 - agnostic towards rewards and losses
  ProbA <- rep(0.5, nrow(task)) #initialise probability of choosing shape A at 0.5
  choices <- rep(100, nrow(task))
  randoms <- runif(nrow(task))
  
  rewardA <- task[,1] #reward (the m terms are essentially binary indicators here)
  punishA <- task[,2] #punishment 
  
  ProbGo<-rep(100,nrow(task))
  
  QA_go_reward<-0
  QA_nogo_reward<-0
  QA_go_punish<-0
  QA_nogo_punish<-0
  QB_go_reward<-0
  QB_nogo_reward<-0
  QB_go_punish<-0
  QB_nogo_punish<-0
  QC_go_reward<-0
  QC_nogo_reward<-0
  QC_go_punish<-0
  QC_nogo_punish<-0
  QD_go_reward<-0
  QD_nogo_reward<-0
  QD_go_punish<-0
  QD_nogo_punish<-0
  
  for (t in 1:nrow(task)){
    #here '1' is choosing shape A (i.e. the only one we have simulated, really)
    
    if(gng==1){
      if (task$stim[t]==1){
        QA_go <- QA_go_reward + QA_go_punish
        QA_nogo <- QA_nogo_reward + QA_nogo_punish
        ProbGo[t] <- exp(QA_go)/(exp(QA_go)+exp(QA_nogo))*(1-lapse)+lapse/2 #lapse/2 not 4 as 2 options only
        choices[t] <- ifelse(ProbGo[t]>randoms[t],1,2)
        if (choices[t]==1){
          delta_rew <- rewsens * task$go_outcome[t] - QA_go_reward
          delta_loss <- punsens * 0 - QA_go_punish #0 as can't get punish for a
          delta_rew_fic <- -1 * QA_nogo_reward
          delta_pun_fic <- -1 * QA_nogo_punish
          QA_go_reward <- QA_go_reward + win_alpha * delta_rew
          QA_go_punish <- QA_go_punish + loss_alpha * delta_loss
          QA_nogo_reward <- QA_nogo_reward + win_alpha * delta_rew_fic
          QA_nogo_punish <- QA_nogo_punish + loss_alpha * delta_pun_fic
        } else if (choices[t]==2){
          delta_rew <- rewsens * task$nogo_outcome[t] - QA_nogo_reward
          delta_loss <- punsens * 0 - QA_nogo_punish #0 as can't get punish for a
          delta_rew_fic <- -1 * QA_go_reward
          delta_pun_fic <- -1 * QA_go_punish
          QA_nogo_reward <- QA_nogo_reward + win_alpha * delta_rew
          QA_nogo_punish <- QA_nogo_punish + loss_alpha * delta_loss
          QA_go_reward <- QA_go_reward + win_alpha * delta_rew_fic
          QA_go_punish <- QA_go_punish + loss_alpha * delta_pun_fic
        }
        
      } else if (task$stim[t]==2){
        QB_go <- QB_go_reward + QB_go_punish
        QB_nogo <- QB_nogo_reward + QB_nogo_punish
        ProbGo[t] <- exp(QB_go)/(exp(QB_go)+exp(QB_nogo))*(1-lapse)+lapse/2 #lapse/2 not 4 as 2 options only
        choices[t] <- ifelse(ProbGo[t]>randoms[t],1,2)
        if (choices[t]==1){
          delta_rew <- rewsens * 0 - QB_go_reward
          delta_loss <- punsens * task$go_outcome[t] - QB_go_punish 
          delta_rew_fic <- -1 * QB_nogo_reward
          delta_pun_fic <- -1 * QB_nogo_punish
          QB_go_reward <- QB_go_reward + win_alpha * delta_rew
          QB_go_punish <- QB_go_punish + loss_alpha * delta_loss
          QB_nogo_reward <- QB_nogo_reward + win_alpha * delta_rew_fic
          QB_nogo_punish <- QB_nogo_punish + loss_alpha * delta_pun_fic
        } else if (choices[t]==2){
          delta_rew <- rewsens * 0 - QB_nogo_reward
          delta_loss <- punsens * task$nogo_outcome[t] - QB_nogo_punish 
          delta_rew_fic <- -1 * QB_go_reward
          delta_pun_fic <- -1 * QB_go_punish
          QB_nogo_reward <- QB_nogo_reward + win_alpha * delta_rew
          QB_nogo_punish <- QB_nogo_punish + loss_alpha * delta_loss
          QB_go_reward <- QB_go_reward + win_alpha * delta_rew_fic
          QB_go_punish <- QB_go_punish + loss_alpha * delta_pun_fic
        }
        
      } else if (task$stim[t]==3){
        QC_go <- QC_go_reward + QC_go_punish
        QC_nogo <- QC_nogo_reward + QC_nogo_punish
        ProbGo[t] <- exp(QC_go)/(exp(QC_go)+exp(QC_nogo))*(1-lapse)+lapse/2 #lapse/2 not 4 as 2 options only
        choices[t] <- ifelse(ProbGo[t]>randoms[t],1,2)
        if (choices[t]==1){
          delta_rew <- rewsens * task$go_outcome[t] - QC_go_reward
          delta_loss <- punsens * 0 - QC_go_punish #0 as can't get punish for c
          delta_rew_fic <- -1 * QC_nogo_reward
          delta_pun_fic <- -1 * QC_nogo_punish
          QC_go_reward <- QC_go_reward + win_alpha * delta_rew
          QC_go_punish <- QC_go_punish + loss_alpha * delta_loss
          QC_nogo_reward <- QC_nogo_reward + win_alpha * delta_rew_fic
          QC_nogo_punish <- QC_nogo_punish + loss_alpha * delta_pun_fic
        } else if (choices[t]==2){
          delta_rew <- rewsens * task$nogo_outcome[t] - QC_nogo_reward
          delta_loss <- punsens * 0 - QC_nogo_punish #0 as can't get punish for c
          delta_rew_fic <- -1 * QC_go_reward
          delta_pun_fic <- -1 * QC_go_punish
          QC_nogo_reward <- QC_nogo_reward + win_alpha * delta_rew
          QC_nogo_punish <- QC_nogo_punish + loss_alpha * delta_loss
          QC_go_reward <- QC_go_reward + win_alpha * delta_rew_fic
          QC_go_punish <- QC_go_punish + loss_alpha * delta_pun_fic
        }
        
      } else if (task$stim[t]==4){
        QD_go <- QD_go_reward + QD_go_punish
        QD_nogo <- QD_nogo_reward + QD_nogo_punish
        ProbGo[t] <- exp(QD_go)/(exp(QD_go)+exp(QD_nogo))*(1-lapse)+lapse/2 #lapse/2 not 4 as 2 options only
        choices[t] <- ifelse(ProbGo[t]>randoms[t],1,2)
        if (choices[t]==1){
          delta_rew <- rewsens * 0 - QD_go_reward
          delta_loss <- punsens * task$go_outcome[t] - QD_go_punish 
          delta_rew_fic <- -1 * QD_nogo_reward
          delta_pun_fic <- -1 * QD_nogo_punish
          QD_go_reward <- QD_go_reward + win_alpha * delta_rew
          QD_go_punish <- QD_go_punish + loss_alpha * delta_loss
          QD_nogo_reward <- QD_nogo_reward + win_alpha * delta_rew_fic
          QD_nogo_punish <- QD_nogo_punish + loss_alpha * delta_pun_fic
        } else if (choices[t]==2){
          delta_rew <- rewsens * 0 - QD_nogo_reward
          delta_loss <- punsens * task$nogo_outcome[t] - QD_nogo_punish 
          delta_rew_fic <- -1 * QD_go_reward
          delta_pun_fic <- -1 * QD_go_punish
          QD_nogo_reward <- QD_nogo_reward + win_alpha * delta_rew
          QD_nogo_punish <- QD_nogo_punish + loss_alpha * delta_loss
          QD_go_reward <- QD_go_reward + win_alpha * delta_rew_fic
          QD_go_punish <- QD_go_punish + loss_alpha * delta_pun_fic
        }
        
      } 
      #barplot(c(QA_go_reward, QA_nogo_reward, QB_go_punish, QB_nogo_punish,
            #  QC_go_reward, QC_nogo_reward, QD_go_punish, QD_nogo_punish))

    } else if (gng==0){
    
      ProbA[t] = exp(QA_reward[t] - QA_punish[t])/
        (exp(QA_reward[t] - QA_punish[t]) + exp(QB_reward[t] - QB_punish[t]))*(1-lapse)+lapse/2
  
      
      
      randoms[t]<- runif(1)
      choices[t]<- ifelse(ProbA[t]>randoms[t], 1, 2)
      
      
      if (choices[t]==1){
        
        QA_reward[t+1] = QA_reward[t] + win_alpha * (rewsens*rewardA[t] - QA_reward[t]) # value learning for rewards for A
        QA_punish[t+1] = QA_punish[t] + loss_alpha * (punsens*punishA[t] - QA_punish[t]) # value learning for punishments for A
        QB_reward[t+1] = QB_reward[t] + win_alpha * (-QB_reward[t]) #fictive updating
        QB_punish[t+1] = QB_punish[t] + loss_alpha * (-QB_punish[t]) #fictive updating
        
      } else {  
        
        QB_reward[t+1] = QB_reward[t] + win_alpha * (rewsens*(1-rewardA[t]) - QB_reward[t]) # value learning for rewards for B
        QB_punish[t+1] = QB_punish[t] + loss_alpha * (punsens*(1-punishA[t]) - QB_punish[t]) 
        QA_reward[t+1] = QA_reward[t] + win_alpha * (-QA_reward[t]) #fictive updating
        QA_punish[t+1] = QA_punish[t] + loss_alpha * (-QA_punish[t]) #fictive updating
        
      }
    }
  }
  
  return (choices)
}
