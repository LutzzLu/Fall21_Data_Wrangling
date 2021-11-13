viz.ggplot <- function(data) {
  library(ggplot2)
  # pca=read.csv("dr_pca.csv",header=TRUE,sep=",")
  # colnames(pca)=c("num","PCA_1","PCA_2","target")
  # data = pca
  colnames(data)=c("target","dr_1","dr_2")
  T=unique(data$target)
    # tar_cols<-c("#de8f6e","#aac0aa","#2a1a1f","#bb0a21","#087e8b")
    # names(tar_cols)<-levels(pca$target)
  ggplot(data,aes(x=`dr_1`,y=`dr_2`,color=target))+
    geom_point()+ 
    labs(x="dr_1",y="dr_2")+ theme_classic()
      #+ scale_color_manual(values=tar_cols)


  dr_mean1=c()
  dr_mean2=c()
  dr_var1=c()
  dr_var2=c()
  for(i in 1:5){
    dr_mean1[i]=mean(data[which(data$target==T[i]),]$dr_1)
    dr_mean2[i]=mean(data[which(data$target==T[i]),]$dr_2)
    dr_var1[i]=var(data[which(data$target==T[i]),]$dr_1)
    dr_var2[i]=var(data[which(data$target==T[i]),]$dr_2)
  }
  pcameasure=data.frame(T,dr_mean1,dr_mean2,dr_var1,dr_var2)
  head(pcameasure)
  plot(density(data$dr_1))
  lines(density(data$dr_2),col="red")
}