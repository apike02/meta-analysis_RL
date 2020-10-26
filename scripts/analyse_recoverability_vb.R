analyse_recoverability_vb<-function(model,ntrials,task) {
  workingdir='C:/Users/apike/OneDrive - University College London/metaRL/'
  source(paste0(workingdir,'scripts/bic.R'))
  source(paste0(workingdir,'scripts/get_previous_character.R'))
  library('reshape2')
  library('plyr')
  savedir<-paste0(workingdir,'/generate_recover/',model,'/')
  
  all_parameters<-list()
  
  scriptdir=workingdir
  
  readRDS(paste0(workingdir,'/generate_recover/',model,'/fit_',task,'.RDS'))
  
  n_lr<-get_previous_character(model,pattern='lr') #home-brewed function that gets previous character to a pattern
  n_b<-get_previous_character(model,pattern='t')
  n_s<-get_previous_character(model,pattern='s')
  
  if(is.na(n_s)){n_s<-0} #for if theres a lapse but no 's', it picks up the s in lapse
  
  n_lapse<-get_previous_character(model,pattern='lapse')
  n_bias<-get_previous_character(model,pattern='bias')
  n_param = sum(c(n_lr,n_b,n_s,n_lapse,n_bias))
  
  if (n_lr==2) param_names<-c('alpha_win','alpha_loss')
  if (n_lr==1) param_names<-c('alpha')
  if (n_b==2) param_names<-c(param_names,'beta_win','beta_loss')
  if (n_b==1) param_names<-c(param_names,'beta')
  if (n_s==2) param_names<-c(param_names,'sensitivity_win','sensitivity_loss')
  if (n_s==1) param_names<-c(param_names,'sensitivity')
  if (n_lapse==1) param_names<-c(param_names,'lapse')
  if (n_bias==1) param_names<-c(param_names,'go_bias')
  if (n_bias==2) param_names<-c(param_names,'go_bias','pav_bias')
  if (n_bias==3) param_names<-c(param_names,'go_bias','app_bias','av_bias')
  
  temp<-load(paste0(workingdir,'/simulated_data/generated_params')) #load original parameters
  original_params=get(temp)
  rm(temp)
  
  mname<-paste0('fit_',task)
  
  for (param in 1:n_param){
    
    if(param_names[param]=='beta_win'){original_param=original_params['beta']
    } else if(param_names[param]=='beta_loss'){original_param=original_params['betaloss']
    } else if(param_names[param]=='beta'){original_param=original_params['beta']
    } else if(param_names[param]=='alpha_win'){original_param=original_params['alpha']
    } else if(param_names[param]=='alpha_loss'){original_param=original_params['alphaloss']
    } else if(param_names[param]=='alpha'){original_param=original_params['alpha']
    } else if(param_names[param]=='sensitivity'){original_param=original_params['rewsens']
    } else if(param_names[param]=='sensitivity_win'){original_param=original_params['rewsens']
    } else if(param_names[param]=='sensitivity_loss'){original_param=original_params['punsens']
    } else if(param_names[param]=='go_bias'){original_param=original_params['gobias']
    } else if(param_names[param]=='pav_bias'){original_param=original_params['appbias']
    } else if(param_names[param]=='app_bias'){original_param=original_params['appbias']
    } else if(param_names[param]=='av_bias'){original_param=original_params['avbias']
    } else {original_param=original_params[param_names[param]]}
    
    original_param<-unlist(original_param)
    
    estimated_param<-colMeans(eval(parse(text=mname))$draws(param_names[param]))
    
    original_param<-original_param[1:length(estimated_param)]
    
    sink(paste(savedir,'correlations_',model,'_',task,'_',param_names[param],'.txt',sep=''))
    print('Stan')
    print(cor.test(original_param,estimated_param)) #correlation
    print(lm(original_param~estimated_param)) #regression equation
    sink()
    
    
    params<-data.frame(cbind(original_param,estimated_param))
    ggplot(params,aes(x=original_param,y=estimated_param))+
      geom_point()+
      geom_smooth(method=lm,  linetype="dashed",
                  color="darkred", fill="blue")+
      theme_classic()+
      geom_abline()
    ggsave(paste0(savedir,'corplot_',model,'_',task,'_',param_names[param],'_vb.png'))
    all_parameters[[param]]<-params
  }
  return(all_parameters)
}
