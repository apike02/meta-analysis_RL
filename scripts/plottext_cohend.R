plottext_cohend<-function(model,d,permutation){
  if(permutation==1){
    if(model$table$`permutation P(>F)`[1]<0.05){
      plottext<-paste0('italic(d)==',d,'*"*"')
    } else {
      plottext<-paste0('italic(d)==',d)      
    }
  } else {
    if(anova$anova_table$`Pr(>F)`[1]<0.05){
      plottext<-paste0('italic(d)==',d,'*"*"')
    } else {
      plottext<-paste0('italic(d)==',d)      
    }
  }
}