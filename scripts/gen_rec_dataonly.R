gen_rec_dataonly <- function (n,task,model,taskname,gng,workingdir){
  library('tidyr')
  
  source(paste0(workingdir,'scripts/simulate_choices_GENERIC.R'))
  source(paste0(workingdir,'scripts/simulate_choices_gng.R'))
  #source(paste0(workingdir,'scripts/simulate_choices_unvalenced.R'))
  source(paste0(workingdir,'scripts/create_sim_matrix.R'))
  source(paste0(workingdir,'scripts/get_previous_character.R'))
  
  load(paste0(workingdir,'simulated_data/generated_params'))
  
  ntrials<-nrow(task)
  
  if (gng==1){
    temp1<-matrix(data=NA, nrow <- ntrials*(n), ncol<- 8)  
  } else {
    temp1<-matrix(data=NA, nrow <- ntrials*(n), ncol<- 7)
  }
  
  n_lr<-get_previous_character(model,pattern='lr') #home-brewed function that gets previous character to a pattern
  n_b<-get_previous_character(model,pattern='t')
  n_s<-get_previous_character(model,pattern='s')
  n_lapse<-get_previous_character(model,pattern='lapse')
  n_bias<-get_previous_character(model,pattern='bias')
  
  if(is.na(n_s)){n_s=0} #changes NA to 0 if there's only a lapse term

  for (agent in 1:n){
    parameters=c(
      if (n_lr==1){
        generated_parameters$alpha[agent]
      } else if (n_lr==2){
        c(generated_parameters$alpha[agent],
          generated_parameters$alphaloss[agent])
      },
      if (n_b==1){
        generated_parameters$beta[agent]
      } else if (n_b==2){
        c(generated_parameters$beta[agent],
          generated_parameters$betaloss[agent])
      },
      if (n_s==2){
        c(generated_parameters$rewsens[agent],
          generated_parameters$punsens[agent])
      } else if (n_s==1){
        generated_parameters$rewsens[agent]
      },
      if (n_lapse==1){
        generated_parameters$lapse[agent]
      },
      if (n_bias==1){
        generated_parameters$gobias[agent]
      },
      if (n_bias==2){
        c(generated_parameters$gobias[agent],
          generated_parameters$appbias[agent])
      },
      if (n_bias==3){
        c(generated_parameters$gobias[agent],
          generated_parameters$appbias[agent],
          generated_parameters$avbias[agent])
      }
      
    )
    
    parameters<-as.list(parameters) #stops there being weird blank fields
    
    if (n_lr==2) param_names<-c('alphawin','alphaloss')
    if (n_lr==1) param_names<-c('alpha')
    if (n_b==2) param_names<-c(param_names,'betawin','betaloss')
    if (n_b==1) param_names<-c(param_names,'beta')
    if (n_s==2) param_names<-c(param_names,'rewsens','punsens')
    if (n_s==1) param_names<-c(param_names,'sensitivity')
    if (n_lapse==1) param_names<-c(param_names,'lapse')
    if (n_bias==1) param_names<-c(param_names,'go_bias')
    if (n_bias==2) param_names<-c(param_names,'go_bias','pav_bias')
    if (n_bias==3) param_names<-c(param_names,'go_bias','app_bias','av_bias')
    
    names(parameters)<-param_names
    
    if (gng==1){
      task<-data.frame(task)
      choices<- simulate_choices_gng (parameters, task, gng)
      temp1<-create_sim_matrix(temp1,1,0,agent,task,choices,ntrials, gng)
      temp1<-data.frame(temp1)
      colnames(temp1)<- c('study','pat_con','id','trial','stim','go_outcome','nogo_outcome','choices')
    } else {
      choices<- simulate_choices (parameters, task, gng) #note this uses simulate_choices_GENERIC script
      temp1<-create_sim_matrix(temp1,1,0,agent,task,choices,ntrials, gng)
      temp1<-data.frame(temp1)
      colnames(temp1)<- c('study','pat_con','id','trial','reward','pun','choices')
    }
    rm(choices)
  }
  
  
  
  # rewardA <- select(temp1,c('id','trial','reward')) %>% spread(id, reward, fill = 0)
  # rewardA$trial <- NULL
  # punishA <- select(temp1,c('id','trial','pun')) %>% spread(id, pun, fill = 0)
  # punishA$trial <- NULL
  # choices <- select(temp1,c('id','trial','choices')) %>% spread(id, choices, fill = 0)
  # choices$trial <- NULL
  # pat_con <- subset(temp1,trial==1,drop=TRUE) %>% select(c('id','pat_con'))
  # pat_con$id <- NULL
  # pat_con<- pat_con[,1] #removes the extra singleton dimension
  # rewardB<-ifelse(rewardA==0,1,0)
  # punishB<-ifelse(punishA==0,1,0)
  
  data<-temp1
  
  #data<-list(ntrials=ntrials,nsub=n,includeTrial = rep(1,ntrials),  rewardA=rewardA[,1],punishA=punishA[,1],rewardB=rewardB[,1],punishB=punishB[,1],choices=choices,pat_con=pat_con)
  
  return(data)
}
  