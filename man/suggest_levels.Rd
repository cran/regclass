\name{suggest_levels}
\alias{suggest_levels}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Combining levels of a categorical variable
}
\description{
This function determines levels that are similar to each other either in terms of their average value of some quantitative variable or the percentages of each level of a two-level categorical variable.  Use it to get a rough idea of what levels are "about the same" with regard to some variable.}
\usage{
suggest_levels(formula,data,maxlevels=NA,target=NA,recode=FALSE,plot=TRUE,...) }
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{formula}{
A standard R formula written as y~x.  Here, x is the variable whose levels you wish to combine, and y is the quantitative or two-level categorical variable.  
}
  \item{data}{
An optional argument giving the name of the data frame that contains x and y.  If not specified, the function will use existing definitions in the parent environment.
}
  \item{maxlevels}{
The maximum number of combined levels to consider (cannot exceed 26).
}
  \item{target}{
The number of resulting levels into which the levels of x will be combined.  Will default to the suggested value of the fewest number whose resulting BIC is no more than 4 above the lowest BIC of any combination.
}

  \item{recode}{
\code{TRUE} or \code{FALSE}.  If \code{TRUE}, the function outputs a conversion table as well as the new level identities
}

  \item{plot}{
\code{TRUE} or \code{FALSE}.  If \code{TRUE}, a plot is provided which shows the distribution of \code{y} for each level of \code{x} and lines showing which levels are grouped together.
}

  \item{...}{
Additional arguments used to make the plot.  Typically this will be \code{equal=TRUE} and \code{inside=TRUE} to be passed to \code{mosaic}.
}



}
\details{
This function calculates the average value (or percentage of each level) of y for each level of x.  It then builds a partition model taking y to be this average value (or percentage) with x being the predictor variable.  The first split yields the "best" scheme for combining levels of x into 2 values.  The second split yields the "best" scheme for combining levels of x into 3 values, etc.  

The argument \code{maxlevels} specifies the maximum numbers of levels in the combination scheme.  By default, it will use the number of levels of x (ie, no combination).  Setting this to a lower number saves time, since most likely a small number of combined levels is desired.  This is useful for seeing how different combination schemes compare.

The argument \code{target} will force the algorithm to producing exactly this number of combined levels.  This is useful once you have determined how many levels of x you want.

If \code{recode} is \code{FALSE}, a table showing the combined levels along with the "BIC" of the combination scheme (lower is better, but a difference of around 4 or less is negligible).  The suggested combination will be the fewer number of levels which has as BIC no more than 4 above the scheme that gave the lowest BIC.  

If \code{recode} is \code{TRUE}, a list of three elements is produced.  \code{$Conversion1} gives a table of the Old and New levels alphabetized by Old while \code{$Conversion2} gives a table of the Old and New levels alphabized by New.  \code{$newlevels} gives a factor of the cases levels under the new combination scheme.  If \code{target} is not set, it will use the suggested number of levels.

}


\references{
Introduction to Regression and Modeling
}

\author{
Adam Petrie
}


%% ~Make other sections like Warning with \section{Warning }{....} ~

\examples{
 
 
  data(DONOR)
  
  #Can levels of URBANICITY be treated the same with regards to probability of donation?
  #Analysis suggests yes (all levels in one)
  suggest_levels(Donate~URBANICITY,data=DONOR)

  #Can levels of URBANICITY be treated the same with regards to donation amount?
  #Analysis suggests yes, but perhaps there are four "effective levels"
  
  suggest_levels(Donation.Amount~URBANICITY,data=DONOR)
  SL <- suggest_levels(Donation.Amount~URBANICITY,data=DONOR,target=4,recode=TRUE)
	SL$Conversion

	#Add a column to the DONOR dataframe that contains these new cluster identities
  DONOR$newCLUSTER_CODE <- SL$newlevels
	 }


