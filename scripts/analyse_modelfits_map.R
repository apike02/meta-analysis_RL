analyse_modelfits_map<-function(workingdir,model_details,task,ntrials) {
  library('R.matlab')
  library('reshape2')
  model_names<-model_details[,1]
  model_params<-model_details[,2]
  ll_df=list()
  bic_df=list()
  
  matlab_data<-readMat(paste0(workingdir,'map/results_map_',task,'.mat'))
  
  map=matlab_data$results.map[,,1]
  
  for (i in 1:length(model_names)){
    mname=paste('map.',model_names[i],sep='')
    map_data=as.data.frame(map[mname])
  

    llname<-paste('loglike_',mname,sep='')
    assign(llname,map_data[,ncol(map_data)]) #last column in MAP has log likelihood not log posterior
    ll_df[[i]]<-c(eval(parse(text=llname)))
    
    #waicname<-paste('waic_',mname,sep='')

    #waic_list[[i]]<-assign(waicname,waic(eval(parse(text=llname))))

    bicname<-paste('bic_',mname,sep='')
    assign(bicname,bic(ntrials,eval(parse(text=llname)),as.numeric(model_params[i])))
    bic_df[[i]]<-c(eval(parse(text=bicname)))
    
    rm(mname)
    
  }
  
  
  ll_df<-data.frame(matrix(unlist(ll_df),ncol=length(ll_df),byrow=T))
  colnames(ll_df)<-model_names
  save(ll_df,file=paste0(workingdir,'/big_outputs/ll_df_map_',task))
  
  extract<-function(x) {
    y<-x$estimates[3,1]
  }
  

  bic_df<-data.frame(matrix(unlist(bic_df),ncol=length(bic_df),byrow=T))
  colnames(bic_df)<-model_names
  save(bic_df,file=paste0(workingdir,'/big_outputs/bic_df_map_',task))
  bic_df_long<-melt(bic_df)
  bic_sums<-ggplot(bic_df_long,aes(x=reorder(variable,value),y=value,fill=reorder(variable,value)))+
    geom_bar(stat='identity')+
    labs(x='Model',y='Total BIC',title='BIC for model fits')+
    theme_classic()+
    theme(legend.position='none',axis.text.x=element_text(angle=90,hjust=1))+
    coord_cartesian(ylim = c(round_any(min(colSums(bic_df)),1000,floor), 
                             round_any(max(colSums(bic_df)),1000,ceiling)))
  ggsave(paste0(workingdir,'/map/figures/bic_sums_map_',task,'.png'),bic_sums,scale=0.4,width=10,height=10)
  
  modelcolours<-colorRampPalette(brewer.pal(9,'Set1'))(18)[1:length(model_names)]
  bic_sums_unordered<-ggplot(bic_df_long,aes(x=variable,y=value,fill=variable))+
    geom_bar(stat='identity')+
    labs(x='Model',y='Total BIC')+
    theme_classic()+
    theme(legend.position='none',axis.text.x=element_text(angle=90,hjust=1))+
    coord_cartesian(ylim = c(round_any(min(colSums(bic_df)),1000,floor), 
                             round_any(max(colSums(bic_df)),1000,ceiling)))+
    scale_fill_manual(values=modelcolours)
  ggsave(paste0(workingdir,'/map/figures/bic_sums_map_unordered_',task,'.png'),bic_sums_unordered,scale=0.5,width=10,height=10)
  
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
  ggsave(paste0(workingdir,'/map/figures/bf_bic_map_',task,'.png'),scale=0.5,width=10,height=10)
  
  print(bic_sums_unordered)
  
}
