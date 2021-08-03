simulate_choices_feedback <- function(parameters,task,gng){
  
  list2env(parameters,environment())
  
  if (!exists('alpha',inherits=FALSE) & !exists('alphawin',inherits=FALSE)){#no learning rate equivalent to setting it to 1
    alpha<-1
  }
  
  if (!exists('alphawin',inherits=FALSE)){#only one learning rate
    alphawin<-alpha
    alphaloss<-alpha
  } 
  if (!exists('alphaomit',inherits=FALSE)){#no separate learning rate for omitting outcomes
    alphaomit<-0
  } 
  
  if (!exists('beta',inherits=FALSE) & !exists('betawin',inherits=FALSE)){#no temperature equivalent to setting it to 1
    beta<-1
  }
  
  if (!exists('betawin', inherits=FALSE)){#only one temperature
    betawin<-beta
    betaloss<-beta
  }
  
  if (!exists('sensitivity', inherits=FALSE) & !exists('rewsens',inherits=FALSE)){#no sens equivalent to setting it to 1
    sensitivity<-1
  }
  
  if (!exists('rewsens',inherits=FALSE)){#if only one sensitivity param
    rewsens<-sensitivity
    punsens<-sensitivity
  }
  
  if (!exists('lapse',inherits=FALSE)){
    lapse<-0
  }  
  
  if (!exists('r0',inherits=FALSE)){#no r0 equivalent to setting to 0
    r0<-0
  }
  
  if (!exists('memory',inherits=FALSE)){#no memory equiv to setting to 1
    memory<-1
  }
  
  
  QA <- rep(0, nrow(task)+1) 
  QB <- rep(0, nrow(task)+1) 

  ProbGo <-rep(NA,nrow(task))
  ProbA <- rep(100,nrow(task))
  choices <- rep(100, nrow(task))
  QA_go <- 0
  QA_nogo<- 0
  QB_go <- 0
  QB_nogo <- 0
  QC_go <- 0
  QC_nogo <- 0
  QD_go <- 0
  QD_nogo <- 0
  
  rewardA <- task[,1] #reward (the m terms are essentially binary indicators here)
  punishA <- task[,2] #punishment 
  
  randoms<- runif(nrow(task))
  task<-data.frame(task)
  
  for (t in 1:nrow(task)){
    #here '1' is choosing shape A (i.e. the only one we have simulated, really)
    
    if (gng==1){
      
      if (task$stim[t]==1){ #stimulus A
        ProbGo[t] = (1-lapse)*(exp(QA_go/betawin))/(exp(QA_go/betawin) + exp(QA_nogo/betawin))+lapse/2
        if (QA_go/betawin>709){ProbGo[t] = 1}
        if (QA_go/betawin<(-709)){ProbGo[t] = 0}
        if (QA_nogo/betawin>709){ProbGo[t] = 0}
        if (QA_nogo/betawin<(-709)){ProbGo[t] = 1}
        choices[t]<- ifelse(ProbGo[t]>randoms[t], 1, 2)
        if (choices[t]==1){
          m1=task$go_outcome[t] #reward 
        } else {
          m1=task$nogo_outcome[t]
        } 
        m2=0 #m2 = 0 because no punishment is possible on these trials
        if (choices[t]==1){
          task$go_outcome[t]<-ifelse(task$go_outcome[t]==0,r0,task$go_outcome[t]) #if 0, replace with r0 instead (ensure defined after m1 and m2)
          QA_go = memory*QA_go + m1*alphawin*(rewsens*task$go_outcome[t]-QA_go) - 
            m2*alphaloss*(punsens*task$go_outcome[t] - QA_go) +
            (1-m1)*alphaomit*(rewsens*task$go_outcome[t]-QA_go) -
            (1-m2)*alphaomit*(punsens*task$go_outcome[t]-QA_go)
        } else { #if nogo
          task$nogo_outcome[t]<-ifelse(task$nogo_outcome[t]==0,r0,task$nogo_outcome[t]) #if 0, replace with r0 instead
          QA_nogo = memory*QA_nogo + m1*alphawin*(rewsens*task$nogo_outcome[t]-QA_nogo) - 
            m2*alphaloss*(punsens*task$nogo_outcome[t] - QA_nogo) + 
            (1-m1)*alphaomit*(rewsens*task$nogo_outcome[t]-QA_nogo) - 
            (1-m2)*alphaomit*(punsens*task$nogo_outcome[t]-QA_nogo)
          
        }

        
      } else if (task$stim[t]==2){
        ProbGo[t] = (1-lapse)*(exp(QB_go/betawin))/(exp(QB_go/betawin) + exp(QB_nogo/betawin))+lapse/2
        if (QB_go/betawin>709){ProbGo[t] = 1}
        if (QB_go/betawin<(-709)){ProbGo[t] = 0}
        if (QB_nogo/betawin>709){ProbGo[t] = 0}
        if (QB_nogo/betawin<(-709)){ProbGo[t] = 1}
        choices[t]<- ifelse(ProbGo[t]>randoms[t], 1, 2)
        if (choices[t]==1){
          m2=task$go_outcome[t]
        } else {
          m2=task$nogo_outcome[t]
        } 
        m1=0 #m1 = 0 because no reward is possible on these trials
        if (choices[t]==1){
          task$go_outcome[t]<-ifelse(task$go_outcome[t]==0,r0,task$go_outcome[t]) #if 0, replace with r0 instead
          QB_go= memory*QB_go + m1*alphawin*(rewsens*task$go_outcome[t]-QB_go) - 
            m2*alphaloss*(punsens*task$go_outcome[t] - QB_go) + #multiplied by -1 as m2 is -1 if punish
            (-1-m1)*alphaomit*(rewsens*task$go_outcome[t]-QB_go) - 
            (-1-m2)*alphaomit*(punsens*task$go_outcome[t]-QB_go) #as above
        } else {
          task$nogo_outcome[t]<-ifelse(task$nogo_outcome[t]==0,r0,task$nogo_outcome[t]) #if 0, replace with r0 instead
          QB_nogo= memory*QB_nogo + m1*alphawin*(rewsens*task$nogo_outcome[t]-QB_nogo) - 
            m2*alphaloss*(punsens*task$nogo_outcome[t] - QB_nogo) + #multiplied by -1 as m2 is -1 if punish
            (-1-m1)*alphaomit*(rewsens*task$nogo_outcome[t]-QB_nogo) - 
            (-1-m2)*alphaomit*(punsens*task$nogo_outcome[t]-QB_nogo) #as above
        }

        
      } else if (task$stim[t]==3){
        ProbGo[t] = (1-lapse)*(exp(QC_go/betawin))/(exp(QC_go/betawin) + exp(QC_nogo/betawin))+lapse/2
        if (QC_go/betawin>709){ProbGo[t] = 1}
        if (QC_go/betawin<(-709)){ProbGo[t] = 0}
        if (QC_nogo/betawin>709){ProbGo[t] = 0}
        if (QC_nogo/betawin<(-709)){ProbGo[t] = 1}
        choices[t]<- ifelse(ProbGo[t]>randoms[t], 1, 2)
        if (choices[t]==1){
          m1=task$go_outcome[t]
        } else {
          m1=task$nogo_outcome[t]
        } 
        m2=0 #m2 = 0 because no punishment is possible on these trials
        if (choices[t]==1){
          task$go_outcome[t]<-ifelse(task$go_outcome[t]==0,r0,task$go_outcome[t]) #if 0, replace with r0 instead
          QC_go = memory*QC_go + m1*alphawin*(rewsens*task$go_outcome[t]-QC_go) - 
            m2*alphaloss*(punsens*task$go_outcome[t] - QC_go) + 
            (1-m1)*alphaomit*(rewsens*task$go_outcome[t]-QC_go) - 
            (1-m2)*alphaomit*(punsens*task$go_outcome[t]-QC_go)
        } else {
          task$nogo_outcome[t]<-ifelse(task$nogo_outcome[t]==0,r0,task$nogo_outcome[t]) #if 0, replace with r0 instead
          QC_nogo = memory*QC_nogo + m1*alphawin*(rewsens*task$nogo_outcome[t]-QC_nogo) - 
            m2*alphaloss*(punsens*task$nogo_outcome[t] - QC_nogo) + 
            (1-m1)*alphaomit*(rewsens*task$nogo_outcome[t]-QC_nogo) - 
            (1-m2)*alphaomit*(punsens*task$nogo_outcome[t]-QC_nogo)
        }

        
      } else if (task$stim[t]==4){
        ProbGo[t] = (1-lapse)*(exp(QD_go/betawin))/(exp(QD_go/betawin) + exp(QD_nogo/betawin))+lapse/2
        if (QD_go/betawin>709){ProbGo[t] = 1}
        if (QD_go/betawin<(-709)){ProbGo[t] = 0}
        if (QD_nogo/betawin>709){ProbGo[t] = 0}
        if (QD_nogo/betawin<(-709)){ProbGo[t] = 1}
        choices[t]<- ifelse(ProbGo[t]>randoms[t], 1, 2)
        if (choices[t]==1){
          m2=task$go_outcome[t]
        } else {
          m2=task$nogo_outcome[t]
        } 
        m1=0 #m1 = 0 because no reward is possible on these trials
        
        if (choices[t]==1){
          task$go_outcome[t]<-ifelse(task$go_outcome[t]==0,r0,task$go_outcome[t]) #if 0, replace with r0 instead
          QD_go = memory*QD_go + m1*alphawin*(rewsens*task$go_outcome[t]-QD_go) - 
            m2*alphaloss*(punsens*task$go_outcome[t] - QD_go) +
            (-1-m1)*alphaomit*(rewsens*task$go_outcome[t]-QD_go) - 
            (-1-m2)*alphaomit*(punsens*task$go_outcome[t]-QD_go) 
        } else {
          task$nogo_outcome[t]<-ifelse(task$nogo_outcome[t]==0,r0,task$nogo_outcome[t]) #if 0, replace with r0 instead
          QD_nogo = memory*QD_nogo + m1*alphawin*(rewsens*task$nogo_outcome[t]-QD_nogo) - 
            m2*alphaloss*(punsens*task$nogo_outcome[t] - QD_nogo) +
            (-1-m1)*alphaomit*(rewsens*task$nogo_outcome[t]-QD_nogo) - 
            (-1-m2)*alphaomit*(punsens*task$nogo_outcome[t]-QD_nogo) #
        }

      }
      
      #barplot(c(QA_go,QA_nogo,QB_go,QB_nogo,QC_go,QC_nogo,QD_go,QD_nogo))

      

      
    } else {
    
      ProbA[t] = (1-lapse)*exp(QA[t]/betawin)/(exp(QA[t]/betawin) + exp(QB[t]/betawin))+lapse/2
      if (QA[t]/betawin > 709){ProbA[t]=1}
      if (QA[t]/betawin < -709){ProbA[t]=0}
      if (QB[t]/betawin > 709){ProbA[t]=0}
      if (QB[t]/betawin < -709){ProbA[t]=1}
      
  
      choices[t]<- ifelse(ProbA[t]>randoms[t], 1, 2)
  
      if (choices[t]==1){
        if (rewardA[t]==1){
          m1=1
        } else {
          m1=0
        } 
        if (punishA[t]==1){
          m2=1
        } else {
          m2=0
        }
        rewardA[t]<-ifelse(rewardA[t]==0,r0,rewardA[t])
        punishA[t]<-ifelse(punishA[t]==0,r0,punishA[t])
        QA[t+1] = memory*QA[t] + m1*alphawin*(rewsens*rewardA[t]-QA[t]) - m2*alphaloss*(punsens*punishA[t] - QA[t]) + 
          (1-m1)*alphaomit*(rewsens*rewardA[t]-QA[t]) - (1-m2)*alphaomit*(punsens*punishA[t]-QA[t])
        QB[t+1] = QB[t]
  
      } else {
        rewardB<-1-rewardA[t]
        punishB<-1-punishA[t]
          if (rewardB==1){
            m1=1
          } else {
            m1=0
          } 
          if (punishB==1){
            m2=1
          } else {
            m2=0
          }
        rewardB<-ifelse(rewardB==0,r0,rewardB)
        punishB<-ifelse(punishB==0,r0,punishB)
        QB[t+1] = memory*QB[t] + m1*alphawin*(rewsens*rewardB-QB[t]) - m2*alphaloss*(punsens*punishB - QB[t]) + 
          (1-m1)*alphaomit*(rewsens*rewardB-QA[t]) - (1-m2)*alphaomit*(punsens*punishB-QA[t])
        QA[t+1] = QA[t]
      }
    }
  }
  return (choices)
}
