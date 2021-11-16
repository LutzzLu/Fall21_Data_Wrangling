\name{viz.meanvar}
\alias{viz.meanvar}
\title{return a dataframe containing information of mena and variance for your dataset}
\usage{
meanvar.df = viz.meanvar(data)
}
\description{
In this function, there are only one argument, which is data.

For the data, it should be a dataframe type. The first column should be the label. And the rest columns should be the dimension reduced data. Then it will return a dataframe, which contains the mean and variance information of this dataset.
}
\examples{
data(iris)
data = iris[, 1:4]
label = data.frame(species = iris[, 5])
output = dimensionreduce(data, label)
data = data.frame(label = species = iris[, 5], dr_1 = output[, 1], dr_2 = output[, 2])
meanvar.df = viz.meanvar(data)
}
