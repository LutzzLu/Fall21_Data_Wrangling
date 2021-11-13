viz.meanvar <- function(dr.data) {
  library(ggplot2)
  pca=read.csv("dr_pca.csv",header=TRUE,sep=",")
  # colnames(pca)=c("num","PCA_1","PCA_2","target")
  data = pca
  colnames(data)=c("target","PCA_1","PCA_2")
  T=unique(data$target)
    # tar_cols<-c("#de8f6e","#aac0aa","#2a1a1f","#bb0a21","#087e8b")
    # names(tar_cols)<-levels(pca$target)
  ggplot(data,aes(x=`PCA_1`,y=`PCA_2`,color=target))+
    geom_point()+ 
    labs(x="PCA_1",y="PCA_2")+ theme_classic()
      #+ scale_color_manual(values=tar_cols)


  pca_mean1=c()
  pca_mean2=c()
  pca_var1=c()
  pca_var2=c()
  for(i in 1:5){
    pca_mean1[i]=mean(data[which(data$target==T[i]),]$PCA_1)
    pca_mean2[i]=mean(data[which(data$target==T[i]),]$PCA_2)
    pca_var1[i]=var(data[which(data$target==T[i]),]$PCA_1)
    pca_var2[i]=var(data[which(data$target==T[i]),]$PCA_2)
  }
  pcameasure=data.frame(T,pca_mean1,pca_mean2,pca_var1,pca_var2)
  head(pcameasure)
  plot(density(data$PCA_1))
  lines(density(data$PCA_2),col="red")
}