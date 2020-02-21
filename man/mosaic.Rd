\name{mosaic}
\alias{mosaic}

\title{
Mosaic plot
}

\description{
Provides a mosaic plot to visualize the association between two categorical variables }

\usage{
mosaic(formula,data,color=TRUE,labelat=c(),xlab=c(),ylab=c(),
                            magnification=1,equal=FALSE,inside=FALSE,ordered=FALSE) 
}

\arguments{

  \item{formula}{
A standard R formula written as y~x, where y is the name of the variable playing the role of y and x is the name of the variable playing the role of x.  
}
  \item{data}{
An optional argument giving the name of the data frame that contains x and y.  If not specified, the function will use existing definitions in the parent environment.
}

  \item{color}{
\code{TRUE} or \code{FALSE}.   If \code{FALSE}, plots are presented in greyscale.  If \code{TRUE}, an intelligent color scheme is chosen to shade the plot.  
}

  \item{labelat}{a vector of factor levels of \code{x} to be labeled (in the case that you want only certain levels to be labeled) }
  
   \item{xlab}{Label of horizontal axis if you want something different that the name of the \code{x} variable}
   
   \item{ylab}{Label of vertical axis if you want something different that the name of the \code{y} variable}
 
    \item{magnification}{Magnification of the labels of the \code{x} variable.  A number smaller than 1 shrinks everything.  A number larger than 1 makes everything larger }

    \item{equal}{If \code{FALSE}, the bar widths are proportional to the frequency of the corresponding level.  If \code{TRUE}, the bar widths are all equal (useful if there are many levels or some are extremely rare).}

   \item{inside}{If \code{FALSE}, labels are beneath the bars.  If \code{TRUE}, labels are placed inside the bars and rotated (useful if the levels have long names) }

   \item{ordered}{If \code{FALSE}, bars are in alphabetical order.  If \code{TRUE}, the ordering of the bars reflects the ordering of the factor levels.}

}

\details{
This function shows a mosaic plot to visualize the conditional distributions of \code{y} for each level of \code{x}, along with the marginal distribution of \code{y} to the right of the plot.  The widths of the segmented bar charts are proportional to the frequency of each level of \code{x}.  These plots are the same that appear using \code{associate}.

}



\references{
Introduction to Regression and Modeling
}

\author{
Adam Petrie
}


\seealso{ \code{\link{associate}}}
\examples{
  data(ACCOUNT)
	mosaic(Area.Classification~Purchase,data=ACCOUNT,color=TRUE)
	
	data(EX6.CLICK)
	#Default presentation:  not very useful
	mosaic(Click~DeviceModel,data=EX6.CLICK)  
	#Better presentation
	mosaic(Click~DeviceModel,data=EX6.CLICK,equal=TRUE,inside=TRUE,magnification=0.8)  
	 }
