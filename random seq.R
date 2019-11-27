
#basic data
seq<-NULL
r<-c("A","T","C","G","-")
st<-matrix(c(0.25,0.25,0.25,0.25,0),nrow = 1,ncol = 5)
pro<-matrix(c(0.25,0.2,0.2,0.2,0.15,0.2,0.25,0.2,0.2,0.15,0.2,0.2,0.25,0.2,0.15,0.2,0.2,0.2,0.25,0.15,0,0,0,0,0),nrow = 5,ncol = 5,byrow = T)

#first letter
let1<-function(){
  le1<-sample(x=r,size = 1,replace = T,prob = c(0.25,0.25,0.25,0.25,0))
  return(le1)
                }

#subsequent letter
f<-function(va){ 
  if(va=="A"){
    let2<-sample(x=r,size=1,replace = T,prob = pro[1,])
    seq<<-c(seq,let2)
    f(let2)
    
  }else if(va=="T"){
    let2<-sample(x=r,size=1,replace = T,prob = pro[2,])
    seq<<-c(seq,let2)
    f(let2)
    
  }else if(va=="C"){
    let2<-sample(x=r,size=1,replace = T,prob = pro[3,]) 
    seq<<-c(seq,let2)
    f(let2)
    
  }else if(va=="G"){
    let2<-sample(x=r,size=1,replace = T,prob = pro[4,])
    seq<<-c(seq,let2)
    f(let2)
    
  }else{
    return(seq)
  }
  
}

#generate a sequence
ff<-function(){
  le1<-let1()
  seq<<-le1
  f(le1)
}


#generate n sequences, calculate length and draw picture
library(ggplot2)
fff<-function(num){
  res<-replicate(num,ff())
  len<-lengths(res)-1
  leng<- data.frame(matrix(unlist(len), ncol = 1, byrow=T),stringsAsFactors = F)
  colnames(leng)<-"Length"
  ggplot(leng, aes(x = Length)) + geom_line(colour = "cadetblue3", stat = "density") +
    geom_point(aes(y = 0, colour ="red"), shape = 25, alpha = 0.4, size = 4) +
    theme(legend.position = "none")
  
}

#



