create_reversal_task <- function (ntrials, nreversals, probability,
                         coupled, taskname,workingdir){
  blklength <- round (ntrials/nreversals)
  rew <- rep(NA, ntrials)
  pun <- rep(NA, ntrials)
  probabilities<-list()
  rew_p<-vector()
  pun_p<-vector()
  
  for (x in 1:nreversals){
    
    ifelse((x %% 2) == 0,prob <- probability, prob <- 1-probability)
    #first part of line below is overly complex way to get right number of trials
    rew[1:blklength+blklength*(x-1)] <- rbinom (blklength, size = 1, p = prob)
    
    #this creates a vector of the probabilities used 
    rew_p[1:blklength+blklength*(x-1)]<-prob
    
    # rew and pun independent
    pun[1:blklength+blklength*(x-1)] <- rbinom (blklength, size = 1, p = 1-prob)

    #creates second part of vector, for punishment
    pun_p[1:blklength+blklength*(x-1)]<-1-prob

  }
  pun<-c(pun[-c(1:(blklength/2))],pun[c(1:(blklength/2))]) #jitters by half blocklength so independent
  pun_p<-c(pun_p[-c(1:(blklength/2))],pun_p[c(1:(blklength/2))]) #jitters by half blocklength so independent
  
  if (coupled==TRUE) pun=1-rew
  if (coupled==TRUE) pun_p=1-rew_p
  
  probabilities[[1]]<-rew_p
  probabilities[[2]]<-pun_p
  
  #checks no NAs left
  ifelse(sum(is.na(rew))==0, print('No NAs!'), print('ERROR: NAs found'))
  ifelse(sum(is.na(pun))==0, print('No NAs!'), print('ERROR: NAs found'))
  
  #save task probabilities
  save(probabilities,file=paste0(workingdir,'/probabilities_',taskname))
  
  #return task
  task<-cbind(rew,pun)
  return(task)
}
  