library(tsne)
library(umap)
library(randomForest)

dimensionreduce = function(data, label){
  library(tsne)
  library(umap)
  library(randomForest)
  data.pca = prcomp(data, center = TRUE)
  data.pca_1 = data.pca$x
  dr.pca = data.frame(class = label[,1], dr1 = data.pca_1[, 1], dr2 = data.pca_1[, 2])
  data.tsne = tsne(data, k=2, perplexity=50)
  dr.tsne = data.frame(class = label[, 1], dr1 = data.tsne[, 1], dr2 = data.tsne[, 2])
  data.umap.matrix = as.matrix(data)
  data.umap = umap(data.umap.matrix)
  dr.umap = data.frame(class = label[, 1], dr1 = data.umap[, 1], dr2 = data.umap[, 2])

  set.seed(100)
  dr.pca$class = as.factor(dr.pca$class)
  dr.tsne$class = as.factor(dr.tsne$class)
  dr.umap$class = as.factor(dr.umap$class)
  ind = sample(2, nrow(df.pca), replace = TRUE, prob = c(0.7, 0.3))
  dr.pca.train = dr.pca[ind == 1, ]
  dr.pca.test = dr.pca[ind == 2, ]
  dr.tsne.train = dr.tsne[ind == 1, ]
  dr.tsne.test = dr.tsne[ind == 2, ]
  dr.umap.train = dr.umap[ind == 1, ]
  dr.umap.test = dr.umap[ind == 2, ]
  rf.pca = randomForest(class~., data = dr.pca.train, mtry = 2, ntree = 500)
  rf.tsne = randomForest(class~., data = dr.tsne.train, mtry = 2, ntree = 500)
  rf.umap = randomForest(class~., data = dr.umap.train, mtry = 2, ntree = 500)
  pca.pred = predict(rf.pca,data=dr.pca.train)
  pca.Freq = table(pca.pred,dr.pca.train$class)
  pca.accu = sum(diag(pca.Freq))/sum(pca.Freq)

  tsne.pred = predict(rf.tsne,data=dr.tsne.train)
  tsne.Freq = table(tsne.pred,dr.tsne.train$class)
  tsne.accu = sum(diag(tsne.Freq))/sum(tsne.Freq)

  umap.pred = predict(rf.umap,data=dr.umap.train)
  umap.Freq = table(umap.pred,dr.umap.train$class)
  umap.accu = sum(diag(umap.Freq))/sum(umap.Freq)

  max.accu = max(c(pca.accu, tsne.accu, umap.accu))

  if (pca.accu == max.accu){
    return (dr.pca)
  } else if (tsne.accu == max.accu){
    return (dr.tsne)
  } else{
    return (dr.umap)
  }
}



