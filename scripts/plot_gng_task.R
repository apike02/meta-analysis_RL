plot_gng_task<-function(taskname,taskdir,ntrials){
  library('reshape2')
  trial<-1:ntrials
  task<-data.frame(trial)
  load(paste0(taskdir,'/probabilities_',taskname))
  task$gtw<-rep(unlist(probabilities[[1]]),ntrials)
  task$gta<-rep(unlist(probabilities[[2]]),ntrials)
  task$ngtw<-rep(unlist(probabilities[[2]]),ntrials)
  task$ngta<-rep(unlist(probabilities[[1]]),ntrials)
  task_long<-melt(task,id.vars='trial')
  task_long$outcome<-c(rep('win',ntrials),rep('loss',ntrials),rep('win',ntrials),rep('loss',ntrials))
  plot<-ggplot(task_long,aes(x=trial,y=value,colour=variable))+
    theme_classic()+
    facet_wrap(facets='outcome')+
    geom_line(position=position_dodge(0.2))+
    labs(y='Probability that a GO action is \n associated with the outcome',
           x='Trial number')+
    scale_colour_discrete(name = "Stimulus", labels = c("Go to win", "Go to avoid", "No-Go to win","No-Go to avoid"))
  ggsave(paste0(taskdir,taskname,'_plot.png'),plot,scale=1,width=4,height=3,units='in')
  print(plot)
}  
  
