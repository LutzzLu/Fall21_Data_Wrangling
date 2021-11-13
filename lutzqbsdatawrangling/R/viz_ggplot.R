viz.ggplot <- function(pca) {
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
  colnames(pca)=c("target","PCA_1","PCA_2")
  T=unique(pca$target)


  ggplot(pca,aes(x=`PCA_1`,y=`PCA_2`,color=target))+
    geom_point()+ 
    labs(x="PCA_1",y="PCA_2")+
    theme_classic()

  pca_mean1=c()
  pca_mean2=c()
  pca_var1=c()
  pca_var2=c()
  for(i in 1:5){
    pca_mean1[i]=mean(pca[which(pca$target==T[i]),]$PCA_1)
    pca_mean2[i]=mean(pca[which(pca$target==T[i]),]$PCA_2)
    pca_var1[i]=var(pca[which(pca$target==T[i]),]$PCA_1)
    pca_var2[i]=var(pca[which(pca$target==T[i]),]$PCA_2)
  }
  pcameasure=data.frame(T,pca_mean1,pca_mean2,pca_var1,pca_var2)
  head(pcameasure)
  plot(density(pca$PCA_1))
  lines(density(pca$PCA_2),col="red")
}