\name{mode_factor}
\alias{mode_factor}

\title{
Find the mode of a categorical variable
}
\description{
This function finds the mode of a categorical variable
}
\usage{
mode_factor(x)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a factor}
}

\details{
The mode is the most frequently occuring level of a categorical variable.  This function returns the mode of a categorical variable.  If there is a tie for the most frequent level, it returns all modes.
}

\references{
Introduction to Regression and Modeling
}
\author{
Adam Petrie
}

\examples{
	data(EX6.CLICK)
  mode_factor(EX6.CLICK$DeviceModel)
  
  #To see how often it appears try sorting a table
  sort( table(EX6.CLICK$DeviceModel),decreasing=TRUE )
  
  x <- c( rep(letters[1:4],5), "e", "f" )  #multimodel
  mode_factor(x)

  
}
