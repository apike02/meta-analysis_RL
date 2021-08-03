analyse_modelfits_vba<-function(workingdir,model_details,task,ntrials) {
  library('reshape2')
  library('RColorBrewer')
  source(paste0(workingdir,'/scripts/integrated_bic.R'))
  model_names<-model_details[,1]
  model_params<-model_details[,2]
  ll_df=list()
  bic_df=list()
  

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
    
    rm(mname_pat,mname_con)
  }
  
  
  ll_df<-data.frame(matrix(unlist(ll_df),ncol=length(ll_df),byrow=F))
  colnames(ll_df)<-model_names
  save(ll_df,file=paste0(workingdir,'/big_outputs/ll_df_vba_',task))
  
  extract<-function(x) {
    y<-x$estimates[3,1]
  }
  bic_df<-matrix(data=NA,nrow=nrow(ll_df),ncol=ncol(ll_df))
  
  for (c in 1:ncol(ll_df)){
    bic_df[,c]<-bic(ntrials,-ll_df[,c],as.numeric(model_params[c]))
  }
  bic_df<-data.frame(bic_df)
  colnames(bic_df)<-model_names
  save(bic_df,file=paste0(workingdir,'/big_outputs/bic_df_vba_',task))
  bic_df_long<-melt(bic_df)
  bic_df_long$variable<-factor(bic_df_long$variable,levels=c(model_names),ordered=TRUE)
  bic_sums<-ggplot(bic_df_long,aes(x=reorder(variable,value),y=value,fill=reorder(variable,value)))+
    geom_bar(stat='identity')+
    labs(x='Model',y='Total BIC')+
    theme_classic()+
    theme(legend.position='none',axis.text.x=element_text(angle=90,hjust=1))+
    coord_cartesian(ylim = c(round_any(min(colSums(bic_df)),1000,floor), 
                             round_any(max(colSums(bic_df)),1000,ceiling)))
  ggsave(paste0(workingdir,'/figures/bic_sums_vba_',task,'.png'),bic_sums,scale=0.4,width=10,height=10)
  
  modelcolours<-colorRampPalette(brewer.pal(9,'Set1'))(18)[1:length(model_names)]
  df<-data.frame(names(bic_df),colSums(bic_df))
  names(df)<-c('model','total')
  df$model<-factor(df$model,levels=c(model_names),ordered=TRUE)
  bic_sums_unordered<-ggplot(df,aes(x=model,y=total,fill=model,label=model))+
    geom_bar(stat='identity')+
    labs(x='Model',y='Total BIC',title=paste0('Task',substr(task,2,2)))+
    theme_classic()+
    theme(legend.position='none',axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+
    coord_cartesian(ylim = c(round_any(min(df$total),1000,floor), 
                             round_any(max(df$total),1000,ceiling)))+
    geom_text(data = df[which.min(df$total),], label = "*", nudge_y=100) +
    scale_fill_manual(values=modelcolours)
  ggsave(file=paste0(workingdir,'/figures/bic_sums_vba_unordered_',task,'.png'),bic_sums_unordered,scale=0.5,width=10,height=10)

  sum_bic<-colSums(bic_df)
  sum_bic<-sort(sum_bic)
  
  source(paste0(workingdir,'/scripts/bf_ic.R'))
  bf.bic<-bf_ic(sum_bic)
  bf.bic<-as.data.frame(bf.bic)
  bf.bic$Model<-rownames(as.data.frame(sum_bic))
  bf.bic$Model<-factor(bf.bic$Model,levels=bf.bic$Model)
  colnames(bf.bic)[1]<-'BayesFactor'
  #return(bic_df)
  ggplot(bf.bic,aes(x=Model,y=log(BayesFactor),fill=Model))+
    geom_bar(stat='identity')
  ggsave(paste0(workingdir,'/figures/bf_bic_vba_',task,'.png'),scale=0.5,width=10,height=10)
  
  print(bic_sums_unordered)
  
}
