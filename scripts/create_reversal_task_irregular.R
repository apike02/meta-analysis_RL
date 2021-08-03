create_reversal_task_irregular <- function (ntrials, nreversals, probability,
                                  coupled, taskname,workingdir){
  rew <- vector()
  pun <- vector()
  probabilities<-list()
  rew_p<-vector()
  pun_p<-vector()
  
  
  blklength_rew <- c(round(rnorm(nreversals-1,mean=ntrials/nreversals,sd=5)))
  blklength_rew <- c(blklength_rew,ntrials-sum(blklength_rew))
  
  blklength_pun <- c(round(rnorm(nreversals-1,mean=ntrials/nreversals,sd=5)))
  blklength_pun <- c(blklength_pun,ntrials-sum(blklength_pun))
  
  if(blklength_rew[nreversals]<0|blklength_pun[nreversals]<0) print('oops! try again - last element is negative')

  for (x in 1:nreversals){
    
    ifelse((x %% 2) == 0,prob <- probability, prob <- 1-probability)
    #first part of line below is overly complex way to get right number of trials
    rew<-c(rew,rbinom (blklength_rew[x], size = 1, p = prob))
    
    #this creates a vector of the probabilities used 
    rew_p<-c(rew_p,rep(prob,blklength_rew[x]))
    
  } 
  
  if (coupled==FALSE){
    for (x in 1:nreversals){
    
      ifelse((x %% 2) == 0,prob <- probability, prob <- 1-probability)
      #first part of line below is overly complex way to get right number of trials
      pun<-c(pun,rbinom (blklength_pun[x], size = 1, p = prob))
      
      #this creates a vector of the probabilities used 
      pun_p<-c(pun_p,rep(prob,blklength_pun[x]))
    }
    
  }
  
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
