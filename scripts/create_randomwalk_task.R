create_randomwalk_task <- function (mean, sd, ntrials, start_probability_rew, start_probability_pun,
                                    upper_bound, lower_bound, coupled, taskname){
  probabilities<-list()
  source('N:/Alex/metaRL/scripts/generate_random_walk.R')
  rew <- rep(NA, ntrials)
  pun <- rep(NA, ntrials)
  
  #generates vector of probabilities of rewards
  rew_p <- generate_random_walk(mean,sd,ntrials,start_probability_rew, upper_bound,lower_bound)
  
  #converts into actual rewards or not (0,1 rather than 0.3, 0.6 etc.)
  rew<- rbinom (ntrials, size = 1, rew_p)
  
  #assigns probabilities (not actual rewards) to list to save
  probabilities[[1]]<-rew_p
  
  # if rew and pun independent
  if (coupled==FALSE) {
    
    #generates punishment probabilities
    pun_p <- generate_random_walk(mean,sd,ntrials,start_probability_pun,upper_bound,lower_bound)
    
    #converts into actual punishments or not (0,1 not 0.3,0.7 etc.)
    pun<- rbinom (ntrials, size = 1, pun_p)
    
    #assigns probabilities to list to save
    probabilities[[2]]<-pun_p
  } else {
    
    #if not independent, punishment is just 1-rewards
    pun=1-rew #if coupled
    
    #punishment probabilities are 1- reward probabilities (into list)
    probabilities[[2]]<-1-rew_p
  }
  
  #saves task probabilities
  save(probabilities,file=paste('N:/Alex/metaRL/task/probabilities_',taskname,sep=''))
  
  #checks no NAs left
  ifelse(sum(is.na(rew))==0, print('No NAs!'), print('ERROR: NAs found'))
  ifelse(sum(is.na(pun))==0, print('No NAs!'), print('ERROR: NAs found'))
  
  #return task
  task<-cbind(rew,pun)
}
