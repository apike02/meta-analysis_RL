create_sim_matrix <- function (temp,paper,pat_or_con,id,task,choices,ntrials,gng){
  if (gng==0){
    temp[1:ntrials+ntrials*(id-1),1]<-paper #number corresponding to which paper the data is simmed from
    temp[1:ntrials+ntrials*(id-1),2]<-pat_or_con #patient (1) or control (0)
    temp[1:ntrials+ntrials*(id-1),3]<-id #number/identifier
    temp[1:ntrials+ntrials*(id-1),4]<-1:ntrials #trialnum
    temp[1:ntrials+ntrials*(id-1),5]<-task[,1] #rewards #all doing same task atm
    temp[1:ntrials+ntrials*(id-1),6]<-task[,2] #punishments
    temp[1:ntrials+ntrials*(id-1),7]<-choices #generated choices
  } else {
    temp[1:ntrials+ntrials*(id-1),1]<-paper #number corresponding to which paper the data is simmed from
    temp[1:ntrials+ntrials*(id-1),2]<-pat_or_con #patient (1) or control (0)
    temp[1:ntrials+ntrials*(id-1),3]<-id #number/identifier
    temp[1:ntrials+ntrials*(id-1),4]<-1:ntrials #trialnum
    temp[1:ntrials+ntrials*(id-1),5]<-task[,1] #stimuli
    temp[1:ntrials+ntrials*(id-1),6]<-task[,2] #go outcome
    temp[1:ntrials+ntrials*(id-1),7]<-task[,3] #nogo outcome
    temp[1:ntrials+ntrials*(id-1),8]<-choices #generated choices
  }

  return(temp)
}
