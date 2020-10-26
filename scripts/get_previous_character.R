get_previous_character<-function(model,pattern) {
  index<-regexpr(pattern,model)[1]
  if (index==-1) {
    previous=0
  } else{
    previous=as.numeric(substr(model,index-1,index-1))
  }
  return(previous)
}
