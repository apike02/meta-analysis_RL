plottext_cohend<-function(model,d){
  if(model$table$`permutation P(>F)`[1]<0.05){
    plottext<-paste0('italic(d)==',d,'*"*"')
  } else {
    plottext<-paste0('italic(d)==',d)      
  }
}