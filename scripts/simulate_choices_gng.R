simulate_choices_gng<- function(parameters,task,gng){
  
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
  
  if (!exists('go_bias',inherits=FALSE)){
    go_bias<-0
  }
  
  if (!exists('pav_bias', inherits=FALSE) & !exists('app_bias',inherits=FALSE)){#no app/av bias - set to 0
    app_bias<-0
    av_bias<-0
  }
  
  if (!exists('app_bias',inherits=FALSE)){#if only one pavlovian bias
    app_bias<-pav_bias
    av_bias<-pav_bias
  }
  
  if (!exists('decay',inherits=FALSE)){
    decay<-0
  }
  
  if (!exists('perseverance',inherits=FALSE)){
    perseverance<-1
  } 
  
  randoms <- runif(nrow(task))
  choices <- rep(NA, nrow(task))

  #outcomeA<-rewardA-rewardB
  #outcomeB<-rewardA-rewardB
  
  QA_go<-0
  QB_go<-0
  QC_go<-0
  QD_go<-0
  QA_nogo<-0
  QB_nogo<-0
  QC_nogo<-0
  QD_nogo<-0
  valueA<-0
  valueB<-0
  valueC<-0
  valueD<-0
  ProbGo<-rep(NA,nrow(task))
  
  choice_trace_go <- c(0,0,0,0)
  choice_trace_nogo <-c(0,0,0,0)
  
  
  for (t in 1:nrow(task)){
    
    if (gng==1){
      if (task$stim[t]==1){
        qA_go = (QA_go + go_bias + app_bias*valueA)*betawin - perseverance * choice_trace_go[1]#no loss possible for A
        qA_nogo = QA_nogo*betawin - perseverance * choice_trace_nogo[1] #no biases for nogo 
        if(qA_go > 710|qA_nogo < -710){
          ProbGo[t]<-1
        } else if (qA_go < -710|qA_nogo > 710){
          ProbGo[t]<-0
        } else {
          ProbGo[t] = (1-lapse)*exp(qA_go)/(exp(qA_go) + exp(qA_nogo)) + lapse/2 
        }        
        choices[t]<- ifelse(ProbGo[t]>randoms[t], 1, 2)
        if (choices[t]==1){
          QA_go = QA_go + alphawin * (rewsens*task$go_outcome[t] - QA_go) # Q learning for A (only rewards)
          valueA=valueA + alphawin *(rewsens*task$go_outcome[t] - valueA) + alphaloss*(punsens*0 - valueA) 
          choice_trace_go[1] = choice_trace_go[1] + decay*(1 - choice_trace_go[1])
          choice_trace_nogo[1] = choice_trace_nogo[1] + decay*(0 - choice_trace_nogo[1])
        } else if (choices[t]==2){
          QA_nogo = QA_nogo + alphawin * (rewsens*task$nogo_outcome[t] - QA_nogo) # Q learning for A
          valueA=valueA + alphawin *(rewsens*task$nogo_outcome[t] - valueA) + alphaloss*(punsens*0 - valueA) 
          choice_trace_go[1] = choice_trace_go[1] + decay*(0 - choice_trace_go[1])
          choice_trace_nogo[1] = choice_trace_nogo[1] + decay*(1 - choice_trace_nogo[1])
        }
        
      } else if (task$stim[t]==2){
        qB_go = (QB_go + go_bias + av_bias*valueB)*betaloss - perseverance * choice_trace_go[2]
        qB_nogo = QB_nogo*betaloss - perseverance * choice_trace_nogo[2] #no biases for nogo
        if(qB_go > 710|qB_nogo < -710){
          ProbGo[t]<-1
        } else if (qB_go < -710|qB_nogo > 710){
          ProbGo[t]<-0
        } else {
          ProbGo[t] = (1-lapse)*exp(qB_go)/(exp(qB_go) + exp(qB_nogo)) 
        }
        choices[t]<- ifelse(ProbGo[t]>randoms[t], 1, 2)
        if (choices[t]==1){
          QB_go = QB_go + alphaloss * (punsens*task$go_outcome[t] - QB_go) # Q learning for B
          valueB=valueB + alphawin *(rewsens*0 - valueB) + alphaloss*(punsens*task$go_outcome[t] - valueB) 
          choice_trace_go[2] = choice_trace_go[2] + decay*(1 - choice_trace_go[2])
          choice_trace_nogo[2] = choice_trace_nogo[2] + decay*(0 - choice_trace_nogo[2])
        } else if (choices[t]==2){
          QB_nogo = QB_nogo + alphaloss * (punsens*task$nogo_outcome[t] - QB_nogo) # Q learning for B
          valueB=valueB + alphawin *(rewsens*0 - valueB) + alphaloss*(punsens*task$nogo_outcome[t] - valueB) 
          choice_trace_go[2] = choice_trace_go[2] + decay*(0 - choice_trace_go[2])
          choice_trace_nogo[2] = choice_trace_nogo[2] + decay*(1 - choice_trace_nogo[2])
        }
        
      } else if (task$stim[t]==3){
        qC_go = (QC_go + go_bias + app_bias*valueC)*betawin - perseverance * choice_trace_go[3]
        qC_nogo = QC_nogo*betawin - perseverance * choice_trace_nogo[3] #no biases for nogo
        if(qC_go > 710|qC_nogo < -710){
          ProbGo[t]<-1
        } else if (qC_go < -710|qC_nogo > 710){
          ProbGo[t]<-0
        } else {
          ProbGo[t] = (1-lapse)*exp(qC_go)/(exp(qC_go) + exp(qC_nogo)) + lapse/2
        }
        choices[t]<- ifelse(ProbGo[t]>randoms[t], 1, 2)
        if (choices[t]==1){
          QC_go = QC_go + alphawin * (rewsens*task$go_outcome[t] - QC_go) # Q learning for C
          valueC=valueC + alphawin *(rewsens*task$go_outcome[t] - valueC) + alphaloss*(punsens*0 - valueC) 
          choice_trace_go[3] = choice_trace_go[3] + decay*(1 - choice_trace_go[3])
          choice_trace_nogo[3] = choice_trace_nogo[3] + decay*(0 - choice_trace_nogo[3])
        } else if (choices[t]==2){
          QC_nogo = QC_nogo + alphawin * (rewsens*task$nogo_outcome[t] - QC_nogo) # Q learning for C
          valueC=valueC + alphawin *(rewsens*task$nogo_outcome[t] - valueC) + alphaloss*(punsens*0 - valueC) 
          choice_trace_go[3] = choice_trace_go[3] + decay*(0 - choice_trace_go[3])
          choice_trace_nogo[3] = choice_trace_nogo[3] + decay*(1 - choice_trace_nogo[3])
        }
      } else if (task$stim[t]==4){
        qD_go = (QD_go + go_bias + av_bias*valueD)*betaloss - perseverance * choice_trace_go[4]
        qD_nogo = QD_nogo*betaloss - perseverance * choice_trace_nogo[4] #no biases for nogo
        if(qD_go > 710|qD_nogo < -710){
          ProbGo[t]<-1
        } else if (qD_go < -710|qD_nogo > 710){
          ProbGo[t]<-0
        } else {
          ProbGo[t] = (1-lapse)*exp(qD_go)/(exp(qD_go) + exp(qD_nogo)) + lapse/2
        }        
        choices[t]<- ifelse(ProbGo[t]>randoms[t], 1, 2)
        if (choices[t]==1){
          QD_go = QD_go + alphaloss * (punsens*task$go_outcome[t] - QD_go) # Q learning for D
          valueD=valueD + alphawin *(rewsens*0 - valueD) + alphaloss*(task$go_outcome[t]*0 - valueD) 
          choice_trace_go[4] = choice_trace_go[4] + decay*(1 - choice_trace_go[4])
          choice_trace_nogo[4] = choice_trace_nogo[4] + decay*(0 - choice_trace_nogo[4])
        } else if (choices[t]==2){
          QD_nogo = QD_nogo + alphaloss * (punsens*task$nogo_outcome[t] - QD_nogo) # Q learning for D
          valueD=valueD + alphawin *(rewsens*0 - valueD) + alphaloss*(punsens*task$nogo_outcome[t] - valueD) 
          choice_trace_go[4] = choice_trace_go[4] + decay*(0 - choice_trace_go[4])
          choice_trace_nogo[4] = choice_trace_nogo[4] + decay*(1 - choice_trace_nogo[4])
        }
      }
      
      #barplot(c(QA_go,QA_nogo,QB_go,QB_nogo,QC_go,QC_nogo,QD_go,QD_nogo))
      
    } else {
      
      
      #here '1' is choosing shape A (i.e. the only one we have simulated, really)
      
      if (valueA>0){pavbiasA<-app_bias}
      else {pavbiasA<-av_bias}
      
      if (valueB>0){pavbiasB<-app_bias}
      else {pavbiasB<-av_bias}

      qA[t]=QA[t] + go_bias + pavbiasA*valueA #always action bias as for other tasks can only go
      qB[t]=QB[t] + go_bias + pavbiasB*valueB
      #ProbA[t+1] = exp(QA[t+1]/beta)/(exp(QA[t+1]/beta)+exp(QB[t+1]/beta)) #old version with just one beta
      #this is now a modification using betawin when they won, and betaloss when they lost. Think this works??
      ProbA[t] = (1-lapse)*exp(qA[t]/beta)/(exp(qA[t]/beta) + exp(qB[t]/beta)) + lapse/2
      
      
      choices[t]<- ifelse(ProbA[t]>randoms[t], 1, 2)
      
      if (choices[t]==1){
        if (outcomeA[t]>0){
          sens<-rewsens
          alpha<-alphawin
        } else if (outcomeA[t]<=0){
          sens<-punsens
          alpha<-alphaloss
        } 
        
        QA[t+1] = QA[t] + alpha * (sens*outcomeA[t] - QA[t]) # Q learning for A
        value=value + alpha *(sens*outcomeA[t] - value) 
        QB[t+1] = QB[t] #don't update unchosen
        
      } else {
        if (outcomeB[t]>0){
          sens<-rewsens
          alpha<-alphawin
        } else if (outcomeB[t]<=0){
          sens<-punsens
          alpha<-alphaloss
        } 
        
        QB[t+1] = QB[t] + alpha * (sens*outcomeB[t] - QB[t]) # Q learning for A
        value=value+ alpha * (sens*outcomeB[t] - value)
        QA[t+1] = QA[t] #don't update unchosen
        
      }
    } 
  }
  return (choices)
}
