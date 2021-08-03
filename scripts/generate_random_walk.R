generate_random_walk<-function(mean,sd,ntrials,start_probability,upper_bound,lower_bound){
  p<-rep(0,ntrials)
  p[1]<-start_probability
  for (t in 2:ntrials){
    noise = rnorm(1,mean,sd)
    temp=p[t-1]+noise
    p[t]<-ifelse(temp>upper_bound,p[t-1]-abs(noise),temp)
    p[t]<-ifelse(temp<lower_bound,p[t-1]+abs(noise),p[t])
  }
  
  plot(p)
  return(p)
}