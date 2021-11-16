\name{viz.densityplot}
\alias{viz.densityplot}
\title{generate a density plot for your dataset}
\usage{
viz.densityplot(data)
}
\description{
In this function, there are only one argument, which is data.

For the data, it should be a dataframe type. The first column should be the label. And the rest columns should be the dimension reduced data. Then it will generate a density plot, and you can see the distribution of columns in this dimension reduced dataset.
}
\examples{
data(iris)
data = iris[, 1:4]
label = data.frame(species = iris[, 5])
output = dimensionreduce(data, label)
data = data.frame(label = species = iris[, 5], dr_1 = output[, 1], dr_2 = output[, 2])
viz.densityplot(data)
}
