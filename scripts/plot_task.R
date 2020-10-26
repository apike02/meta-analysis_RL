plot_task<-function(taskname,taskdir){
  task<-taskname
  load(paste0(taskdir,'/probabilities_',taskname))
  task$reward<-unlist(probabilities[[1]])
  task$punish<-unlist(probabilities[[2]])
  task<-data.frame(task)
  plot<-ggplot(task,aes(x=1:nrow(task)))+
    theme_classic()+
    geom_point(aes(y=reward),colour='green')+
    geom_point(aes(y=punish),colour='red')+
    geom_line(aes(y=reward),colour='green')+
    geom_line(aes(y=punish),colour='red')+
    labs(y='Probability that option A is \n associated with the outcome',
         x='Trial number')
  ggsave(paste0(taskdir,taskname,'_plot.png'),plot,scale=1,width=4,height=3,units='in')
  print(plot)
}  

