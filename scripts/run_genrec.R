run_genrec<-function(ntrials,newtask,model1,nsub,workingdir){
  
  datadir=paste0(workingdir,'/generate_recover/')
  taskdir=paste0(workingdir,'/tasks/')
  

  taskname<-paste0('t6_',ntrials)
  
  library('cmdstanr')
  
  source(paste0(scriptdir,'create_randomwalk_task.R')) #creates task with random walk if necessary
  source(paste0(scriptdir,'generate_random_walk.R'))
  source(paste0(scriptdir,'plot_task.R')) #creates task plot
  source(paste0(scriptdir,'loadRData.R')) #loads and names RData files
  source(paste0(scriptdir,'gen_rec_dataonly.R'))
  source(paste0(scriptdir,'stanify_data.R'))
  source(paste0(workingdir,'/scripts/confusion_matrix.R'))
  source(paste0(workingdir,'/scripts/analyse_recoverability_vb.R'))
  
  if (newtask==1){
    task6<-create_randomwalk_task(0,0.05,ntrials,0.7,0.3,0.9,0.1,coupled=FALSE,taskname='task6',workingdir=workingdir,orthogonal=FALSE)
  } else {
    task6<-loadRData(file.path(taskdir,'task6'))}
  if(newtask==0) {ntrials <- nrow(task6)} #if loads in a task, sets ntrials to be the number of trials in that task
  save(file=file.path(taskdir,taskname),task6)
  p6<-plot_task(taskname=taskname,taskdir)
  
  load('C:/Users/apike/OneDrive - University College London/metaRL/simulated_data/generated_params')
  n=nsub
  
  
  dir.create(paste0(datadir,model1,'/'))
  load(file.path(taskdir,taskname))
  data_t6<-gen_rec_dataonly(n,task6,model1,'t6',0,workingdir)
  save(data_t6,file=paste0(datadir,model1,'/data_',taskname))
  data_t6<-as.data.frame(data_t6)
  colnames(data_t6)<- c('study','pat_con','id','trial','reward','pun','choices')
  data_t6<-transform(data_t6,fullid=paste(study,id,sep='.'))
  data_t6$study<-as.numeric(data_t6$study)
  data_t6$fullid<-rep(seq(1:length(unique(data_t6$fullid))),each=ntrials)
  directory<-paste0(datadir,model1)
  write.csv(data_t6,paste0(directory,'/data_',taskname,'.csv'))

  stanify_data(taskname,directory)

  
  load(paste0(datadir,model1,'/con_data_',taskname,'.RData'))
  
  stanname=paste0('metaRL_',model1,'.stan')
  stanfile <- paste0(modeldir,stanname)
  model<-cmdstan_model(stanfile)
  dataname<-paste0('con_data_',taskname)
  fit <- model$variational(data = get(dataname),tol_rel_obj = 0.001,eta=0.1,adapt_engaged=FALSE)
  fit$save_object(file=paste0(datadir,model1,'/fit_',taskname,'.RDS'))

    
  params<-analyse_recoverability_vb(model1,ntrials,taskname)
  original<-do.call(cbind,lapply(params,function(x) x[,1]))
  estimated<-do.call(cbind,lapply(params,function(x) x[,2]))
  colnames(original)<-lapply(params, function(x) gsub("[[:digit:]]","",rownames(x)[1])) #gets column names
  colnames(estimated)<-colnames(original)
  task_matrix<-confusion_matrix(original,estimated,paramnames=c('learning rate', 'inverse temperature','decay','perseverance'))+
    labs(title=paste0('Task ',taskname))
  assign(paste0(taskname,'_matrix'),task_matrix)
  ggsave(paste0(datadir,model1,'/',taskname,'_confusion_matrix.png'),task_matrix)
  
  matrix<-cor(estimated)
  diag(matrix)<-diag(cor(original,estimated))
  
  corr_long<-data.frame(matrix)%>%
    rownames_to_column(var='parameter1')%>%
    pivot_longer(cols=-c('parameter1'),names_to = 'parameter2', values_to='coeff')
  
  return(corr_long)
}