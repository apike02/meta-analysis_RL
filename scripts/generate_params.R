generate_params <- function(n,est_mean,est_sd,plot,min0){
  temp <- mvrnorm (n = n, mu = est_mean, Sigma = (est_sd)^2, empirical = TRUE) #sigma is square of sd (covariance matrix)
  if (min0==TRUE) {temp[temp<0]<-0.001}
  if (plot==TRUE){plot (temp)}

  print ('Diagnostics...') 
  ifelse (all.equal (mean(temp), est_mean), print ('mean correct'), print ('error:mean incorrect'))
  ifelse (all.equal (sd(temp), est_sd), print ('sd correct'), print ('error:sd incorrect')) #sd doesn't work with == 
  #so using all.equal instead (ignores type)
  return(temp)
}
