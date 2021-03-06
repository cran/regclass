\name{overfit_demo}
\alias{overfit.demo}
\alias{overfit_demo}

\title{
Demonstration of overfitting
}

\description{
This function gives a demonstration of how overfitting occurs on a user-inputted dataset by showing the estimated generalization error as additional variables are added to the regression model (up to all two-way interactions). }


\usage{
overfit_demo(DF,y=NA,seed=NA,aic=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{DF}{
The data frame where demonstration will occur.
}
  \item{y}{
The response variable (in quotes)
}
 \item{seed}{Optional argument setting the random number seed if results need to be reproduced}

  \item{aic}{
logical, if \code{FALSE} the demo will show the RMSE on the training sample instead of the AIC.
}
}


\details{
This function splits \code{DF} in half to obtain training and holdout samples.  Regression models are constructed using a forward selection procedure (adding the variable that decreases the AIC the most on the training set), starting at the naive model and terminating at the full model with all two-way interactions.  

The generalization error of each model is computed on the holdout sample.  The AIC (or RMSE on the training) and generalization errors are plotted versus the number of variables in the model to illustrate overfitting.  Typically, the generalization error decreases at first as useful variables are added to the model, then the generalization error increases after the new variables added start to fit the quirks present only in the training data.  When this happens, the model is said to be overfit.
}

\references{
Introduction to Regression and Modeling
}
\author{
Adam Petrie
}


\examples{
  #Overfitting occurs after about 10 predictors (AIC begins to increase after 12/13)
  data(BODYFAT)
  overfit_demo(BODYFAT,y="BodyFat",seed=1010)
  
  #Overfitting occurs after about 5 predictors 
  data(OFFENSE)
  overfit_demo(OFFENSE,y="Win",seed=1997,aic=FALSE)
	 }
