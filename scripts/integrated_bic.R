integrated_bic<-function(fit_dataframe,ntrials) {
  print('Make sure youre using something obtained by sampling!')
  library('loo')
  effective_parameters<-loo(fit_dataframe,'loglik')$estimates['p_loo','Estimate']
  log_like<-extract_log_lik(fit_dataframe,'loglik')
  mean_log_like<-mean(log_like)
  -2*mean_log_like+effective_parameters*log(ntrials)
}