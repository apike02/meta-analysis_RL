bf_ic<-function(ic_vector){
  bf.ic<-vector()
  bf.ic[1]<-1
  for (a in 2:length(ic_vector)){
       bf.ic[a]=exp((ic_vector[a]-ic_vector[a-1])/2)
  }
  return(bf.ic)
}