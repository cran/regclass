\name{examine_driver_Ycat}
\alias{examine_driver_Ycat}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Main driver analysis when Y is a categorical quantity
}
\description{
This function provides a "main driver analysis" on the association between a categorical y variable and the "driver" x.  A visualization (mosaic plot) of the strength of the relationship is provided as well as numerical output to help quantify the variation of y across possible values of the "driver" x.}
\usage{
examine_driver_Ycat(formula,data,sort=TRUE,inside=TRUE,equal=TRUE)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
A standard R formula written as y=="Yes"~x, where y is the variable of interest and "Yes" is the level of interest (you need to pick one of the levels of y to be the level of interest) and x is the driver.  
}
  \item{data}{
An argument giving the name of the data frame that contains x and y.
}
  \item{sort}{
\code{TRUE} (default) or \code{FALSE}.  Indicates whether to sort the avearge value of y from largest to smallest.
}
    \item{equal}{If \code{FALSE}, the bar widths in the mosaic plot are proportional to the frequency of the corresponding level.  If \code{TRUE}, the bar widths are all equal (useful if there are many levels or some are extremely rare).}

   \item{inside}{If \code{FALSE}, labels in the mosaic plot are beneath the bars.  If \code{TRUE}, labels are placed inside the bars and rotated (useful if the levels have long names) }

}
\details{
Main driver analysis is a cornerstone of business analytics where we identify and quantify the key factors (drivers) that most strongly influence a business outcome or performance metric.

This function handles the case when y (the outcome variable) is categorical and you want to analyze the chance that an entity has a specific value of y (the level of interest).  See \code{\link{examine_driver_Ynumeric}} when y is numeric.  

This function works best if x is a categorical variable (with multiple examples of each level of x), since the probability that y equals the level of interest is estimated for each unique value of x.

A mosaic plot (see \code{\link{mosaic}} and its associated arguments) is presented to visualize the relationship between y and the driver.

A table giving the estimated probability that y has the level of interest for each value of x is provided.  A "connecting letters report" shows which levels have statistically significant differences in the probability that y has the level of interest (if ANY letters are in common between two values of x, there is not a statistically significant difference in the probability that y has the level of interest between those two values of x; if ALL letters are different, the difference is statistically significant).  

The function also provides a "Driver Score" (a value between 0 and 1; larger driver scores indicate stronger associations between the chance that y has the level of interest and x).  This driver is the R-squared of a simple linear regression predicting 1 (y has the level of interest) or 0 (y does not have the level of interest) from x, and is at best treated as a relative score indicating the strength of the relationship (the value itself does not hold any practical significance).


}


\references{
Introduction to Regression and Modeling
}

\author{
Adam Petrie
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\seealso{
\code{\link{examine_driver_Ynumeric}},\code{\link{mosaic}} }
\examples{

	#No statistically significant differences in levels
  data(CUSTLOYALTY)
	examine_driver_Ycat(Married=="Single"~Income,data=CUSTLOYALTY)
	
	#Some statistically significant differences in levels
  data(EX6.CLICK)
	examine_driver_Ycat(Click=="Yes"~SiteID,data=EX6.CLICK)
	examine_driver_Ycat(Click=="Yes"~DeviceModel,data=EX6.CLICK)

	 }


