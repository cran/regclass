\name{summarize_tree}
\alias{summarize.tree}
\alias{summarize_tree}

\title{
Useful summaries of partition models from rpart
}

\description{
Reports the RMSE, AIC, and variable importances for a partition model or the variable importances from a random forest.
}
\usage{
summarize_tree(TREE)
}

\arguments{
  \item{TREE}{
A partition model created with \code{rpart} or a random forest from \code{randomForest} 
}
}

\details{
Extracts the RMSE and AIC of a partition model and the variable importances of partition models or random forests.
}


\references{
Introduction to Regression and Modeling
}

\author{
Adam Petrie
}

\seealso{ \code{\link{rpart}}, \code{\link{randomForest}}}
\examples{
  data(WINE)
	TREE <- rpart(Quality~.,data=WINE,control=rpart.control(cp=0.01,xval=10,minbucket=5))
	summarize_tree(TREE)
	RF <- randomForest(Quality~.,data=WINE,ntree=50)
	summarize_tree(RF)
	
	data(NFL)
	TREE <- rpart(X4.Wins~.,data=NFL,control=rpart.control(cp=0.002,xval=10,minbucket=5))
	summarize_tree(TREE)
	RF <- randomForest(X4.Wins~.,data=NFL,ntree=50)
	summarize_tree(RF)
	 }
