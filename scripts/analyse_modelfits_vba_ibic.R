analyse_modelfits_vba_ibic<-function(workingdir,model_details,task,ntrials) {
  library('reshape2')
  library('RColorBrewer')
  source(paste0(workingdir,'/scripts/integrated_bic.R'))
  model_names<-model_details[,1]
  model_params<-model_details[,2]
  ll_df=list()
  bic_df=list()
  ibic_pat<-list()
  ibic_con<-list()
  

  for (i in 1:length(model_names)){
    loadname_con=paste0('fit_',model_names[i],'_HC_',task)
    loadname_pat=paste0('fit_',model_names[i],'_PA_',task)
    
    mname_con<-paste('fit_',model_names[i],'_HC',sep='')
    mname_pat<-paste('fit_',model_names[i],'_PA',sep='')

    
    if (cluster==TRUE){
      load(paste(workingdir,'stan_outputs/',loadname_pat,'.RData',sep=''))
      load(paste(workingdir,'stan_outputs/',loadname_con,'.RData',sep=''))
    } else {
      load(paste(workingdir,'simulated_data/',loadname_pat,sep=''))
      load(paste(workingdir,'simulated_data/',loadname_con,sep=''))
      
    }
    
    llname_pat<-paste('loglike_',mname_pat,sep='')
    llname_con<-paste('loglike_',mname_con,sep='')
    assign(llname_pat,colMeans(rstan::extract(eval(parse(text=mname_pat)), "loglik")$loglik))
    assign(llname_con,colMeans(rstan::extract(eval(parse(text=mname_con)), "loglik")$loglik))
    ll_df[[i]]<-c(eval(parse(text=llname_pat)),eval(parse(text=llname_con)))
    
    ibic_pat[[i]]<-c(integrated_bic(eval(parse(text=mname_pat)),ntrials))
    ibic_con[[i]]<-c(integrated_bic(eval(parse(text=mname_con)),ntrials))
    
    rm(mname_pat,mname_con)
  }
  
  
  ibic_df<-data.frame(ibic_con)+data.frame(ibic_pat)
  names(ibic_df)<-model_names
  save(ibic_df,file=paste0(workingdir,'/simulated_data/ibic_df.RData'))
  ibic_df_long<-pivot_longer(ibic_df,everything(),names_to = 'model_name',values_to = 'ibic')
  
  modelcolours<-colorRampPalette(brewer.pal(9,'Set1'))(18)[1:length(model_names)]
  ibic_df_long$model_name<-factor(ibic_df_long$model_name,levels=c(model_names),ordered=TRUE)
  ibic_sums<-ggplot(ibic_df_long,aes(x=model_name,y=ibic,fill=model_name))+
    theme_minimal()+
    geom_bar(stat='identity')+
    labs(x='Model',y='integrated BIC',title=paste0('Task',substr(task,2,2)))+
    theme(legend.position='none',axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
    geom_text(data = ibic_df_long[which.min(ibic_df_long$ibic),], label = "*", nudge_y=100) +
    scale_fill_manual(values=modelcolours)
  
  
  ggsave(paste0(workingdir,'/figures/ibic_sums_',task,'.png'),ibic_sums)
  
  return(ibic_sums)
  
}
