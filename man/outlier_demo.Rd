\name{outlier_demo}
\alias{outlier_demo}
\alias{outlier_demo}

%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Interactive demonstration of the effect of an outlier on a regression
}
\description{
This function shows regression lines on user-defined data before and after adding an additional point.}
\usage{
outlier_demo(newplot=FALSE,cex.leg=0.8)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{newplot}{
If \code{TRUE}, a new x11 window is created to show the demo.  This helps if the points appear in places different than where the user clicks on some versions of R.
}
  \item{cex.leg}{
A number specifying the magnification of legends inside the plot.  Smaller numbers mean smaller font.
}
}
\details{
This function allows the user to generate data by click on a plot.  Once two points are added, the least squares regression line is draw.  When an additional point is added, the regression line updates while also showing the line without that point.  The effect of outliers on a regression line can easily be illustrated.  Pressing the red UNDO button on the plot will allow you to take away recently added points for further exploration. 

Note:  To end the demo, you MUST click on the red box labeled "End" (or press Escape, which will return an error)

}

\references{
Introduction to Regression and Modeling
}

\author{
Adam Petrie
}
