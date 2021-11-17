\name{viz}
\alias{viz}
\title{do all viz work}
\usage{
viz(data)

viz(data, method = 'ggplot')

viz(data, method = 'meanvar')

viz(data, method = 'densityplot')
}
\description{
In this function, there are two arguments, the first one is data, the second one is method.

For the data, it should be a dataframe type. The first column should be the label. And the rest columns should be the dimension reduced data.

For the method, the default is generate ggplot. And other than this, there three options, which are ggplot, mean and variance dataframe, and density plot.

If the input value is ggplot, then it will generate a scatter plot, which different colors for different labels.

If the input value is meanvar, then it will return a dataframe, which contains the mean and variance information of this dataset.

If the input value is densityplot, then it will generate a density plot, and you can see the distribution of columns in this dimension reduced dataset.
}
\examples{
data(iris)
data = iris[, 1:4]
label = data.frame(species = iris[, 5])
output = dimensionreduce(data, label)
data = data.frame(label = iris[, 5], dr_1 = output[, 1], dr_2 = output[, 2])
viz(data)
viz(data, method = 'ggplot')
viz(data, method = 'meanvar')
viz(data, method = 'densityplot')
}
