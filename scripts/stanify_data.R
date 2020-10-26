stanify_data<-function(taskname,directory){

  library('tidyr')
  library('dplyr')
  
  taskname_short<-paste0('t',substr(taskname,5,5))
  
  simdata<-load(paste0(directory,'/data_',taskname))
  simdata<-eval(parse(text=simdata))
  
  simdata<-as.data.frame(simdata)
  colnames(simdata)<- c('study','pat_con','id','trial','reward','pun','choices')

  simdata<-transform(simdata,fullid=study*1000+id)
  
  simdata_p<-subset(simdata,simdata$pat_con==1) 
  
  if(nrow(simdata_p)>0){
  
    rewardA <- dplyr::select(simdata_p,c('fullid','trial','reward')) %>% spread(fullid, reward, fill = 0)
    rewardA$trial <- NULL
    punishA <- dplyr::select(simdata_p,c('fullid','trial','pun')) %>% spread(fullid, pun, fill = 0)
    punishA$trial <- NULL
    choices <- dplyr::select(simdata_p,c('fullid','trial','choices')) %>% spread(fullid, choices, fill = 0)
    choices$trial <- NULL
    pat_con <- subset(simdata_p,trial==1,drop=TRUE) %>% dplyr::select(c('fullid','pat_con'))
    pat_con$fullid <- NULL
    pat_con<- pat_con[,1] #removes the extra singleton dimension
    rewardB<-ifelse(rewardA==0,1,0)
    punishB<-ifelse(punishA==0,1,0)
    nsub<- ncol(rewardA)
    ntrials <-nrow(rewardA)
    
    
    pat_data<-list(ntrials=ntrials,
                   nsub=nsub,
                   includeTrial = rep(1,ntrials),
                   rewardA=rewardA[,1],
                   punishA=punishA[,1],
                   rewardB=rewardB[,1],
                   punishB=punishB[,1],
                   choices=choices,
                   pat_con=pat_con)
    
    outputname<-paste0('pat_data_',taskname)
    assign(outputname,pat_data)
    save(list=outputname, file = paste0(directory,'/pat_data_',taskname,'.RData'))
  }
  
  
  simdata_c<-subset(simdata,simdata$pat_con==0) 
  
  if(nrow(simdata_c)>0){
    
    rewardA <- dplyr::select(simdata_c,c('fullid','trial','reward')) %>% spread(fullid, reward, fill = 0)
    rewardA$trial <- NULL
    punishA <- dplyr::select(simdata_c,c('fullid','trial','pun')) %>% spread(fullid, pun, fill = 0)
    punishA$trial <- NULL
    choices <- dplyr::select(simdata_c,c('fullid','trial','choices')) %>% spread(fullid, choices, fill = 0)
    choices$trial <- NULL
    pat_con <- subset(simdata_c,trial==1,drop=TRUE) %>% dplyr::select(c('fullid','pat_con'))
    pat_con$fullid <- NULL
    pat_con<- pat_con[,1] #removes the extra singleton dimension
    rewardB<-ifelse(rewardA==0,1,0)
    punishB<-ifelse(punishA==0,1,0)
    nsub<- ncol(rewardA)
    ntrials <-nrow(rewardA)
    
    
    con_data<-list(ntrials=ntrials,
                   nsub=nsub,
                   includeTrial = rep(1,ntrials),
                   rewardA=rewardA[,1],
                   punishA=punishA[,1],
                   rewardB=rewardB[,1],
                   punishB=punishB[,1],
                   choices=choices,
                   pat_con=pat_con)
    
    
    outputname<-paste0('con_data_',taskname)
    assign(outputname,con_data)
    save(list=outputname, file = paste0(directory,'/con_data_',taskname,'.RData'))
  }
  
  #for all data
  
  if(nrow(simdata)>0){
    
    rewardA <- dplyr::select(simdata,c('fullid','trial','reward')) %>% spread(fullid, reward, fill = 0)
    rewardA$trial <- NULL
    punishA <- dplyr::select(simdata,c('fullid','trial','pun')) %>% spread(fullid, pun, fill = 0)
    punishA$trial <- NULL
    choices <- dplyr::select(simdata,c('fullid','trial','choices')) %>% spread(fullid, choices, fill = 0)
    choices$trial <- NULL
    pat_con <- subset(simdata,trial==1,drop=TRUE) %>% dplyr::select(c('fullid','pat_con'))
    pat_con$fullid <- NULL
    pat_con<- pat_con[,1] #removes the extra singleton dimension
    rewardB<-ifelse(rewardA==0,1,0)
    punishB<-ifelse(punishA==0,1,0)
    nsub<- ncol(rewardA)
    ntrials <-nrow(rewardA)
    
    
    all_data<-list(ntrials=ntrials,
                   nsub=nsub,
                   includeTrial = rep(1,ntrials),
                   rewardA=rewardA[,1],
                   punishA=punishA[,1],
                   rewardB=rewardB[,1],
                   punishB=punishB[,1],
                   choices=choices,
                   pat_con=pat_con)
    
    
    outputname<-paste0('all_data_',taskname)
    assign(outputname,all_data)
    save(list=outputname, file = paste0(directory,'/all_data_',taskname,'.RData'))
  }

}
