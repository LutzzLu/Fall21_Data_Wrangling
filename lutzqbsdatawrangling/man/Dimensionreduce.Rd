\name{dimensionreduce}
\alias{dimensionreduce}
\title{Reduce dimensions and return the best by testing accuracy}
\usage{
dimensionreduce(data, label)
}
\description{
In this function, it will reduce the data using three different dimension reduce methods, which are PCA, TSNE and UMAP. And after these three methods, we will get three different dimension reduced datasets. And then it will use Random Forest to split the traning set and test set, and test the prediction accuracy of these three datasets. And finally return the dataset with highest accuracy.

For the arduments:

data

Only contains the data columns that you want to reduce their dimensions, no label! And make sure it's in dataframe type.

label

Only contains the label column, which means it should be only one column, no data! And make sure its rows are matched with data, and make sure it's in dataframe type.
}
\examples{
data(iris)
data = iris[, 1:4]
label = data.frame(species = iris[, 5])
output = dimensionreduce(data, label)
}
