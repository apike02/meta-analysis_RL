generate_choices_fromfit<-function(model,taskname,workingdir,nprior,method='vb'){

  source(paste0(workingdir,'/scripts/simulate_choices_GENERIC.R'))
  source(paste0(workingdir,'/scripts/simulate_choices_gng.R'))
  source(paste0(workingdir,'/scripts/create_sim_matrix.R'))
  source(paste0(workingdir,'scripts/get_previous_character.R'))
  
  task=load(paste0(workingdir,'tasks/',taskname)) 
  task=get(task)
  ntrials<-nrow(task)

  model1=model
  taskname_short<-paste0('t',str_sub(taskname,5,5))
  if (taskname=='task5'){
    gng=1
  } else {
    gng=0
  }
  
  all_data<-load(paste0(workingdir,'simulated_data/data_',taskname))
  bank_choices<-as.data.frame(get(all_data))
  
  n_lr<-get_previous_character(model,pattern='lr') #home-brewed function that gets previous character to a pattern
  n_b<-get_previous_character(model,pattern='t')
  n_s<-get_previous_character(model,pattern='s')
  n_lapse<-get_previous_character(model,pattern='lapse')
  n_bias<-get_previous_character(model,pattern='bias')
  n_decay<-get_previous_character(model,pattern='d')
  n_perseverance<-get_previous_character(model,pattern='p')-n_lapse 
  
  if(is.na(n_s)){n_s=0} #changes NA to 0 if there's only a lapse term
  
  
  if (n_lr==2) param_names<-c('alpha_win','alpha_loss')
  if (n_lr==1) param_names<-c('alpha')
  if (n_b==2) param_names<-c(param_names,'beta_win','beta_loss')
  if (n_b==1) param_names<-c(param_names,'beta')
  if (n_s==2) param_names<-c(param_names,'rewsens','punsens')
  if (n_s==1) param_names<-c(param_names,'sensitivity')
  if (n_lapse==1) param_names<-c(param_names,'lapse')
  if (n_bias==1) param_names<-c(param_names,'go_bias')
  if (n_bias==2) param_names<-c(param_names,'go_bias','pav_bias')
  if (n_bias==3) param_names<-c(param_names,'go_bias','app_bias','av_bias')
  if (n_decay==1) param_names<-c(param_names,'decay')
  if (n_perseverance==1) param_names<-c(param_names,'perseverance')
  
  if (nprior==1 & method=='vb'){
    all_fit<-load(paste0(workingdir,'/stan_outputs/fit_',model1,'_ALL_',taskname_short,'.RData'))
    
    parameters<-list()
    for (param in 1:length(param_names)){
      parameters[[param]]<-summary(get(all_fit),pars=c(param_names[param]))$summary[,1]
    }
    parameters<-bind_cols(parameters)
    names(parameters)<-param_names
    
  } else if (nprior==2 & method=='vb'){
    pa_fit<-load(paste0(workingdir,'/stan_outputs/fit_',model1,'_PA_',taskname_short,'.RData'))
    hc_fit<-load(paste0(workingdir,'/stan_outputs/fit_',model1,'_HC_',taskname_short,'.RData'))
    
    #reorders bank choices appropriately
    bank_choices<-arrange(bank_choices,desc(pat_con))
    
    parameters<-list()
    for (param in 1:length(param_names)){
      parameters[[param]]<-c(summary(get(pa_fit),pars=c(param_names[param]))$summary[,1],
                             summary(get(hc_fit),pars=c(param_names[param]))$summary[,1])
    }
    parameters<-bind_cols(parameters)
    names(parameters)<-param_names
  } else if (method=='map'){
    map<-readMat(paste0(workingdir,'/map/results_map_',taskname_short,'.mat'))$results.map[,,1]
    mname=paste0('map.',model)
    map_data=as.data.frame(map[mname])
    parameters<-map_data[,1:length(param_names)]
    names(parameters)<-param_names

  }
  
  n=nrow(parameters)

  
  if (gng==1){
    temp1<-matrix(data=NA, nrow <- ntrials*(n), ncol<- 8)  
  } else {
    temp1<-matrix(data=NA, nrow <- ntrials*(n), ncol<- 7)
  }
  
  for (agent in 1:n){
    if (gng==1){
      task<-data.frame(task)
      choices<- simulate_choices_gng (parameters[agent,], task, gng)
      temp1<-create_sim_matrix(temp1,1,0,agent,task,choices,ntrials, gng)
      temp1<-data.frame(temp1)
      colnames(temp1)<- c('study','pat_con','id','trial','stim','go_outcome','nogo_outcome','choices')
    } else {
      choices<- simulate_choices (parameters[agent,], task, gng) #note this uses simulate_choices_GENERIC script
      temp1<-create_sim_matrix(temp1,1,0,agent,task,choices,ntrials, gng)
      temp1<-data.frame(temp1)
      colnames(temp1)<- c('study','pat_con','id','trial','rew','pun','choices')
    }
  }
  
  if (gng==1){
    
    join<- c('trial','stim','go_outcome','nogo_outcome','fullid')
    
    synthetic_choices<-temp1%>%
      mutate(id=as.factor(id))%>%
      mutate(fullid=as.factor(id))%>%
      group_by(fullid)
   
    synthetic_choices_summary<-synthetic_choices%>%
      summarize(
        total_rewards=sum(choices==1&go_outcome==1|choices==2&nogo_outcome==1),
        total_punishments=sum(choices==1&go_outcome==-1|choices==2&nogo_outcome==-1))
    
    bank_choices<-bank_choices%>%
      mutate(id=as.factor(id))%>%
      mutate(fullid=as.factor(rep(1:n,each=ntrials)))%>%
      group_by(fullid)%>%
      mutate(study=as.numeric(as.character(study)))
    
    bank_choices_summary<-bank_choices%>%
      summarize(
        total_rewards=sum(choices==1&go_outcome==1|choices==2&nogo_outcome==1),
        total_punishments=sum(choices==1&go_outcome==-1|choices==2&nogo_outcome==-1))
    
    accuracy_learning<-bank_choices%>%
      rowwise%>%
      mutate(correct=which.max(c(go_outcome,nogo_outcome)))%>%
      ungroup%>%
      inner_join(synthetic_choices,by=join)%>%
      mutate(bank_correct=(choices.x==correct))%>%
      mutate(synthetic_correct=(choices.y==correct))%>%
      group_by(trial)%>%
      summarize(bank_accuracy=mean(bank_correct),
                synthetic_accuracy=mean(synthetic_correct))
    
  } else {
    
    join<- c('trial','rew','pun','fullid')
  
    synthetic_choices<-temp1%>%
      mutate(id=as.factor(id))%>%
      mutate(fullid=as.factor(id))%>%
      group_by(fullid)
    
    synthetic_choices_summary<-synthetic_choices%>%
      summarize(
        total_rewards=sum(choices==1&rew==1|choices==2&rew==0),
        total_punishments=sum(choices==1&pun==1|choices==2&pun==0))
    
    bank_choices<-bank_choices%>%
      mutate(id=as.factor(id))%>%
      mutate(fullid=as.factor(rep(1:n,each=ntrials)))%>%
      group_by(fullid)%>%
      mutate(study=as.numeric(as.character(study)))
    
    bank_choices_summary<-bank_choices%>%
      summarize(
        total_rewards=sum(choices==1&rew==1|choices==2&rew==0),
        total_punishments=sum(choices==1&pun==1|choices==2&pun==0))
    
    accuracy_learning<-    
      inner_join(bank_choices,synthetic_choices,by=join)%>%
      mutate(bank_correct=(choices.x==1&rew==1|choices.x==2&rew==0))%>%
      mutate(synthetic_correct=(choices.y==1&rew==1|choices.y==2&rew==0))%>%
      group_by(trial)%>%
      summarize(bank_accuracy=mean(bank_correct),
                synthetic_accuracy=mean(synthetic_correct))
  }
  

  combination<-
    inner_join(bank_choices,synthetic_choices,by=join)%>%
    mutate(identical=(choices.x==choices.y))%>%
    summarize(replicated=sum(identical)/length(identical))
  
  combination_summary<-
    inner_join(bank_choices_summary,synthetic_choices_summary,by=c('fullid'))
  
  ggplot(combination,aes(x=replicated))+
    geom_density()+
    theme_minimal()+
    xlim(c(0,1))
  ggsave(filename=paste0(workingdir,'/figures/choices_predicted_',model,'_',taskname_short,'_',method,nprior,'.png'))

  print(cor.test(combination_summary$total_rewards.x,combination_summary$total_rewards.y))
  print(cor.test(combination_summary$total_punishments.x,combination_summary$total_punishments.y))
  
  combination<-inner_join(combination,combination_summary)
  
  return(data=list(combination=combination,accuracy_learning=accuracy_learning))
    
}