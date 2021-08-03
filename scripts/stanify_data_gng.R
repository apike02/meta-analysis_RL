stanify_data_gng<-function(taskname,directory){
  
  library('tidyr')
  
  taskname_short<-paste0('t',substr(taskname,5,5))
  
  simdata<-load(paste0(directory,'/data_',taskname))
  simdata<-eval(parse(text=simdata))
  
  simdata<-as.data.frame(simdata)
  simdata$study<-as.numeric(as.character(simdata$study))
  simdata$pat_con<-as.numeric(as.character(simdata$pat_con))
  colnames(simdata)<- c('study','pat_con','id','trial','stim','go_outcome','nogo_outcome','choices')

  simdata<-transform(simdata,fullid=study*1000+id)
  
  simdata_p<-subset(simdata,simdata$pat_con==1) 
  
  if(nrow(simdata_p)>0){
  
    stim <- dplyr::select(simdata_p,c('fullid','trial','stim')) %>% spread(fullid, stim, fill = 0)
    stim$trial <- NULL
    go_outcome <- dplyr::select(simdata_p,c('fullid','trial','go_outcome')) %>% spread(fullid, go_outcome, fill = 0)
    go_outcome$trial <- NULL
    nogo_outcome <- dplyr::select(simdata_p,c('fullid','trial','nogo_outcome')) %>% spread(fullid, nogo_outcome, fill = 0)
    nogo_outcome$trial <- NULL
    choices <- dplyr::select(simdata_p,c('fullid','trial','choices')) %>% spread(fullid, choices, fill = 0)
    choices$trial <- NULL
    pat_con <- subset(simdata_p,trial==1,drop=TRUE) %>% dplyr::select(c('fullid','pat_con'))
    pat_con$fullid <- NULL
    pat_con<- pat_con[,1] #removes the extra singleton dimension
    nsub<- ncol(go_outcome)
    ntrials <-nrow(go_outcome)
    
    
    pat_data<-list(ntrials=ntrials,
                   nsub=nsub,
                   includeTrial = rep(1,ntrials),
                   stim=stim[,1],
                   go_outcome=go_outcome[,1],
                   nogo_outcome=nogo_outcome[,1],
                   choices=choices,
                   pat_con=pat_con)
    
    outputname<-paste0('pat_data_',taskname)
    assign(outputname,pat_data)
    save(list=outputname, file = paste0(directory,'/pat_data_',taskname,'.RData'))
    
  }
  
  
  simdata_c<-subset(simdata,simdata$pat_con==0) 
  
  if(nrow(simdata_c)>0){
    
    stim <- dplyr::select(simdata_c,c('fullid','trial','stim')) %>% spread(fullid, stim, fill = 0)
    stim$trial <- NULL
    go_outcome <- dplyr::select(simdata_c,c('fullid','trial','go_outcome')) %>% spread(fullid, go_outcome, fill = 0)
    go_outcome$trial <- NULL
    nogo_outcome <- dplyr::select(simdata_c,c('fullid','trial','nogo_outcome')) %>% spread(fullid, nogo_outcome, fill = 0)
    nogo_outcome$trial <- NULL
    choices <- dplyr::select(simdata_c,c('fullid','trial','choices')) %>% spread(fullid, choices, fill = 0)
    choices$trial <- NULL
    pat_con <- subset(simdata_c,trial==1,drop=TRUE) %>% dplyr::select(c('fullid','pat_con'))
    pat_con$fullid <- NULL
    pat_con<- pat_con[,1] #removes the extra singleton dimension
    nsub<- ncol(go_outcome)
    ntrials <-nrow(go_outcome)
    
    
    con_data<-list(ntrials=ntrials,
                   nsub=nsub,
                   includeTrial = rep(1,ntrials),
                   stim=stim[,1],
                   go_outcome=go_outcome[,1],
                   nogo_outcome=nogo_outcome[,1],
                   choices=choices,
                   pat_con=pat_con)
    
    
    outputname<-paste0('con_data_',taskname)
    assign(outputname,con_data)
    save(list=outputname, file = paste0(directory,'/con_data_',taskname,'.RData'))
  }
  
  if(nrow(simdata)>0){
    
    stim <- dplyr::select(simdata,c('fullid','trial','stim')) %>% spread(fullid, stim, fill = 0)
    stim$trial <- NULL
    go_outcome <- dplyr::select(simdata,c('fullid','trial','go_outcome')) %>% spread(fullid, go_outcome, fill = 0)
    go_outcome$trial <- NULL
    nogo_outcome <- dplyr::select(simdata,c('fullid','trial','nogo_outcome')) %>% spread(fullid, nogo_outcome, fill = 0)
    nogo_outcome$trial <- NULL
    choices <- dplyr::select(simdata,c('fullid','trial','choices')) %>% spread(fullid, choices, fill = 0)
    choices$trial <- NULL
    pat_con <- subset(simdata,trial==1,drop=TRUE) %>% dplyr::select(c('fullid','pat_con'))
    pat_con$fullid <- NULL
    pat_con<- pat_con[,1] #removes the extra singleton dimension
    nsub<- ncol(go_outcome)
    ntrials <-nrow(go_outcome)
    
    
    all_data<-list(ntrials=ntrials,
                   nsub=nsub,
                   includeTrial = rep(1,ntrials),
                   stim=stim[,1],
                   go_outcome=go_outcome[,1],
                   nogo_outcome=nogo_outcome[,1],
                   choices=choices,
                   pat_con=pat_con)
    
    
    outputname<-paste0('all_data_',taskname)
    assign(outputname,all_data)
    save(list=outputname, file = paste0(directory,'/all_data_',taskname,'.RData'))
  }
}
