library(tsne)
library(umap)
library(randomForest)

dimensionreduce = function(data, label){
  data.pca = prcomp(data, center = TRUE)
  dr.pca = data.frame(class = label[,1], dr1 = data.pca[, 1], dr2 = data.pca[, 2])
  data.tsne = tsne(data, k=2, perplexity=50)
  dr.tsne = data.frame(class = label[, 1], dr1 = data.tsne[, 1], dr2 = data.tsne[, 2])
  data.umap.matrix = as.matrix(data)
  data.umap = umap(data.umap.matrix)
  dr.umap = data.frame(class = label[, 1], dr1 = data.umap[, 1], dr2 = data.umap[, 2])

  set.seed(100)
  dr.pca$class = as.factor(dr.pca$class)
  dr.tsne$class = as.factor(dr.tsne$class)
  dr.umap$class = as.factor(dr.umap$class)
  return()
}


hello <- function() {
  print("Hello, world!")
}