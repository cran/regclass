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

\seealso{ \code{\link[rpart]{rpart}}, \code{\link[randomForest]{randomForest}}}
\examples{
  data(WINE)
  set.seed(2025); SUBSET <- WINE[sample(1:nrow(WINE),size=500),]
	TREE <- rpart(Quality~.,data=SUBSET,control=rpart.control(cp=0.01,xval=10,minbucket=5))
	summarize_tree(TREE)
	RF <- randomForest(Quality~.,data=SUBSET,ntrees=50)
	summarize_tree(RF)
	
	data(NFL)
	SUBSET <- NFL[,1:10]
	TREE <- rpart(X4.Wins~.,data=SUBSET,control=rpart.control(cp=0.002,xval=10,minbucket=5))
	summarize_tree(TREE)
	RF <- randomForest(X4.Wins~.,data=SUBSET,ntrees=50)
	summarize_tree(RF)
	 }
