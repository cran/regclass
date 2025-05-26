\name{examine_driver_Ynumeric}
\alias{examine_driver_Ynumeric}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Main driver analysis when Y is a numeric quantity
}
\description{
This function provides a "main driver analysis" on the association between a numeric y variable and the "driver" x.  A visualization of the strength of the relationship is provided as well as numerical output to help quantify the variation of y across possible values of the "driver" x.}
\usage{
examine_driver_Ynumeric(formula,data,sort=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
A standard R formula written as y~x, where y is the variable of interest and x is the driver.  
}
  \item{data}{
An argument giving the name of the data frame that contains x and y.
}
  \item{sort}{
\code{TRUE} (default) or \code{FALSE}.  Indicates whether to sort the avearge value of y from largest to smallest.
}
}
\details{
Main driver analysis is a cornerstone of business analytics where we identify and quantify the key factors (drivers) that most strongly influence a business outcome or performance metric.

This function handles the case when y (the outcome variable) is numeric (see \code{\link{examine_driver_Ycat}} when y is categorical).  

If the driver x is numeric, a scatterplot is presented along with a trend line (in blue; a black line for the average value of y is added).  A summary of a simple linear regression model is also provided.   

If the driver x is categorical, side-by-side boxplots of the distribution of y for each value of x is provided (a black line gives the average value of y in the data).  A table giving the average value of y for each value of x is provided along with a "connecting letters report" to discern which levels have statistically significant differences in the average value of y (if ANY letters are in common between two values of x, there is not a statistically significant difference in the average value of y between those two values of x; if ALL letters are different, the difference in the average value of y is statistically significant).  

The function also provides a "Driver Score" (a value between 0 and 1 which is simply the R-squared of a simple linear regression predicting y from x).  Larger driver scores indicate stronger associations between y and x.

}


\references{
Introduction to Regression and Modeling
}

\author{
Adam Petrie
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{examine_driver_Ycat}} }
\examples{
  #X numeric
  data(CUSTLOYALTY)
	examine_driver_Ynumeric(CustomerLV~WalletShare,data=CUSTLOYALTY)
	
	#X categorical (no statistically significant differences in levels)
  data(CUSTLOYALTY)
	examine_driver_Ynumeric(CustomerLV~Married,data=CUSTLOYALTY)
	
	#X categorical (statistically significant differences in levels)
  data(CUSTLOYALTY)
	examine_driver_Ynumeric(CustomerLV~Income,data=CUSTLOYALTY)
	 }


