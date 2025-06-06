\name{build_tree}
\alias{build.tree}
\alias{build_tree}

\title{
Exploratory building of partition models
}
\description{
A tool to choose the "correct" complexity parameter of a tree
}
\usage{
build_tree(form, data, minbucket = 5, seed=NA, holdout, mincp=0)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{form}{
A formula describing the tree to be built
}
  \item{data}{
Data frame containing the variables to build the tree
}
  \item{minbucket}{
The minimum number of cases allowed in any leaf in the tree
}
  \item{seed}{
If given, specifies the random number seed so the crossvalidation error can be reproduced.
}

  \item{holdout}{
If given, the error on the holdout sample is calculated and given in the cp table.
}

  \item{mincp}{The \code{cp} parameter to which the tree will be grown.  By default it is 0 (recommended), but it can be changed for large datasets.  A value of 0.0001 is likely reasonable.
}

}
\details{
This command combines the action of building a tree to its maximum possible extent using \code{rpart} and looking at the results using \code{getcp}.  A plot of the estimated relative generalization error (as determined by 10-fold cross validation) versus the number of splits is provided.  In addition, the complexity parameter table giving the \code{cp} of the tree with the lowest error (and of the simplest tree with an error within one standard deviation of the lowest error) is reported.

If \code{holdout} is given, the RMSE/misclassification rate on the training and holdout samples are provided in the cp table.
}

\seealso{ \code{\link[rpart]{rpart}}, \code{\link{getcp}}}


\references{
Introduction to Regression and Modeling
}

\author{
Adam Petrie
}

\examples{
  data(JUNK)
  build_tree(Junk~.,data=JUNK,seed=1337)
  data(CENSUS)
  build_tree(ResponseRate~.,data=CENSUS,seed=2017,mincp=0.001)
  data(OFFENSE)
  build_tree(Win~.,data=OFFENSE[1:200,],seed=2029,holdout=OFFENSE[201:352,])
}

