analyse_modelfits_vba_singleprior<-function(workingdir,model_details,task,ntrials) {
  library('reshape2')
  library('plyr')
  model_names<-model_details[,1]
  model_params<-model_details[,2]
  ll_df=list()
  bic_df=list()
  

  for (i in 1:length(model_names)){
    loadname=paste0('fit_',model_names[i],'_ALL_',task)
    
    mname<-paste('fit_',model_names[i],'_ALL',sep='')
    

    if (cluster==TRUE){
      load(paste(workingdir,'stan_outputs/',loadname,'.RData',sep=''))
    } else {
      load(paste(workingdir,'simulated_data/',loadname,sep=''))
      
    }
    
    llname<-paste('loglike_',mname,sep='')
    assign(llname,colMeans(rstan::extract(eval(parse(text=mname)), "loglik")$loglik))
    ll_df[[i]]<-c(eval(parse(text=llname)))
    
    rm(mname)
  }
  
  
  ll_df<-data.frame(matrix(unlist(ll_df),ncol=length(ll_df),byrow=F))
  colnames(ll_df)<-model_names
  save(ll_df,file=paste0(workingdir,'/big_outputs/ll_df_vba_sp_',task))
  
  extract<-function(x) {
    y<-x$estimates[3,1]
  }
  bic_df<-matrix(data=NA,nrow=nrow(ll_df),ncol=ncol(ll_df))
  
  for (c in 1:ncol(ll_df)){
    bic_df[,c]<-bic(ntrials,-ll_df[,c],as.numeric(model_params[c]))
  }
  bic_df<-data.frame(bic_df)
  
  colnames(bic_df)<-model_names
  save(bic_df,file=paste0(workingdir,'/big_outputs/bic_df_vba_sp_',task))
  bic_df_long<-melt(bic_df)
  bic_sums<-ggplot(bic_df_long,aes(x=reorder(variable,value),y=value,fill=reorder(variable,value)))+
    geom_bar(stat='identity')+
    labs(x='Model',y='Total BIC',title='BIC for model fits')+
    theme_classic()+
    theme(legend.position='none',axis.text.x=element_text(angle=90,hjust=1))+
    coord_cartesian(ylim = c(round_any(min(colSums(bic_df)),1000,floor), 
                             round_any(max(colSums(bic_df)),1000,ceiling)))
  ggsave(paste0(workingdir,'/figures/bic_sums_vba_',task,'_sp.png'),bic_sums,scale=0.4,width=10,height=10)
  
  bic_sums_unordered<-ggplot(bic_df_long,aes(x=variable,y=value,fill=variable))+
    geom_bar(stat='identity')+
    labs(x='Model',y='Total BIC',title='BIC for model fits')+
    theme(legend.position='none',axis.text.x=element_text(angle=90,hjust=1))+
    coord_cartesian(ylim = c(round_any(min(colSums(bic_df)),1000,floor), 
                             round_any(max(colSums(bic_df)),1000,ceiling)))
  ggsave(paste0(workingdir,'/figures/bic_sums_vba_unordered_',task,'_sp.png'),bic_sums_unordered,scale=0.5,width=10,height=10)
  
  sum_bic<-colSums(bic_df)
  sum_bic<-sort(sum_bic)
  
  source(paste0(workingdir,'/scripts/bf_ic.R'))
  bf.bic<-bf_ic(sum_bic)
  bf.bic<-as.data.frame(bf.bic)
  bf.bic$Model<-rownames(as.data.frame(sum_bic))
  bf.bic$Model<-factor(bf.bic$Model,levels=bf.bic$Model)
  colnames(bf.bic)[1]<-'BayesFactor'
  print(bf.bic)
  ggplot(bf.bic,aes(x=Model,y=log(BayesFactor),fill=Model))+
    geom_bar(stat='identity')
  ggsave(paste0(workingdir,'/figures/bf_bic_vba_',task,'_sp.png'),scale=0.5,width=10,height=10)
  
}
