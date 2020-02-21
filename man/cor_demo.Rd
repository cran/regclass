\name{cor_demo}
\alias{cor.demo}
\alias{cor_demo}


\title{
Correlation demo
}
\description{
This function shows the correlation and coefficient of determination as user interactively adds datapoints.  Useful for seeing what different values of correlation look like and seeing the effect of outliers.  }
\usage{
cor_demo(cex.leg=0.8)
}
\arguments{
  \item{cex.leg}{
A number specifying the magnification of legends inside the plot.  Smaller numbers mean smaller font.
}
}
\details{
This function allows the user to generate data by click on a plot.  Once two points are added, the correlation (r) and coefficient of determination (r^2) are displayed.  When an additional point is added, these values are updated in the upper left with previous values being displayed in the upper right.  The effect of outliers on the correlation and coefficient of determination can easily be illustrated.  Pressing the red UNDO button on the plot will allow you to take away recently added points for further exploration.

Note:  To end the demo, you MUST click on the red box labeled "End" (or press Escape, which will return an error)
}

\author{
Adam Petrie
}

