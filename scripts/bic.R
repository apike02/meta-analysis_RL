bic<-function(trials,neg_log_like,nparam) {
  if (sum(neg_log_like<0)>0){print('check this is negative log likelihood!!')} 
  2*neg_log_like+nparam*log(trials) #canonical
}