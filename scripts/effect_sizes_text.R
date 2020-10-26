effect_sizes_text<-function(workingdir,analysis_type,priors,estimation_method,param_names,task,data){
  sink(paste0(workingdir,'effect_sizes/',analysis_type,priors,estimation_method,task,'.txt'),append=TRUE)
  for (parameter in 1:length(param_names)){
    pat_data<-dplyr::select(data[data$pat_con=='1',],contains(param_names[parameter]))
    con_data<-dplyr::select(data[data$pat_con=='0',],contains(param_names[parameter]))
    d<-cohen.d(pat_data[,1],con_data[,1])
    cat(analysis_type, priors, estimation_method, 
        task, param_names[parameter],d$estimate, d$conf.int[1], d$conf.int[2],'\n',sep='\t')
  }
  sink()
}

