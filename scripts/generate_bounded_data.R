generate_bounded_data<-function(n_con,n_pat,effsize,lb,ub,tol){
  library('Runuran')
  d<-999
  counter<-1
  while(abs(d-effsize)>tol){
    mean1<-runif(1,lb,ub)
    meandiff<-runif(1,lb,ub)
    if (effsize==0){
      pooled_sd<-runif(1,0,10)
    } else {
      pooled_sd<-solve(effsize,meandiff)
    }
    try(data<-list(con=urnorm(n_con,mean1,pooled_sd,lb,ub),pat=urnorm(n_pat,mean1-meandiff,pooled_sd,lb,ub)),silent=TRUE)
    d<-cohen.d(data[[1]],data[[2]])$estimate
    #print(counter)
    counter=counter+1
  }
  return(data)
}
