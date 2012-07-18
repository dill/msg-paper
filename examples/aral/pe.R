# routine to Pick Elements, pe
pe<-function(this.list,el){
   lapply(this.list,function(x) x[el])
}

