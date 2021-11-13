viz.ggplot <- function(data) {
  # library(ggplot2)
  # colnames(data)=c("target","dr_1","dr_2")
  # T=unique(data$target)
  # ggplot(data,aes(x=`dr_1`,y=`dr_2`,color=target))+
  #   geom_point()+ 
  #   labs(x="dr_1",y="dr_2")+ theme_classic()
  # 
  # 
  # dr_mean1=c()
  # dr_mean2=c()
  # dr_var1=c()
  # dr_var2=c()
  # for(i in 1:5){
  #   dr_mean1[i]=mean(data[which(data$target==T[i]),]$dr_1)
  #   dr_mean2[i]=mean(data[which(data$target==T[i]),]$dr_2)
  #   dr_var1[i]=var(data[which(data$target==T[i]),]$dr_1)
  #   dr_var2[i]=var(data[which(data$target==T[i]),]$dr_2)
  # }
  # measure=data.frame(T,dr_mean1,dr_mean2,dr_var1,dr_var2)
  # head(measure)
  # plot(density(data$dr_1))
  # lines(density(data$dr_2),col="red")

  library(ggplot2)
  colnames(data)=c("target","dr_1","dr_2")
  T=unique(data$target)


  ggplot(data,aes(x=`dr_1`,y=`dr_2`,color=target))+
    geom_point()+ 
    labs(x="dr_1",y="dr_2")+
    theme_classic()

  mean1=c()
  mean2=c()
  var1=c()
  var2=c()
  for(i in 1:length(T)){
    mean1[i]=mean(data[which(data$target==T[i]),]$dr_1)
    mean2[i]=mean(data[which(data$target==T[i]),]$dr_2)
    var1[i]=var(data[which(data$target==T[i]),]$dr_1)
    var2[i]=var(data[which(data$target==T[i]),]$dr_2)
  }
  measure=data.frame(T,mean1,mean2,var1,var2)
  head(measure)
  plot(density(data$dr_1))
  lines(density(data$dr_2),col="red")
}