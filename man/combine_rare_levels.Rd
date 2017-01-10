\name{combine_rare_levels}
\alias{combine_rare_levels}

\title{
Combines rare levels of a categorical variable
}
\description{
This function takes a categorical variable and combines all levels with frequencies less than a user-specified threshold named \code{Combined}
}
\usage{
combine_rare_levels(x,threshold=20)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector of categorical values}
  \item{threshold}{levels that appear a total of \code{threshold} times or fewer will be combined into a new level called \code{Combined} }
}

\details{
Returns a list of two objects:

\code{values} - The recoded values of the categorical variable.  All levels which appeared \code{threshold} times or fewer are now known as \code{Combined}
\code{combined} - The levels that have been combined together

If, after being combined, the \code{Combined} level has \code{threshold} or fewer instances, the remaining level that appears least often is combined as well.
}

\references{
Introduction to Regression and Modeling
}
\author{
Adam Petrie
}

\examples{
	data(EX6.CLICK)
	x <- EX6.CLICK[,15]
	table(x)
	
	#Combine all levels which appear 700 or fewer times (AA, CC, DD)
	y <- combine_rare_levels(x,700)
  table( y$values )
  
  #Combine all levels which appear 1350 or fewer times.  This forces BB (which
  #occurs 2422 times) into the Combined level since the three levels that appear
  #fewer than 1350 times do not appear more than 1350 times combined
	y <- combine_rare_levels(x,1350)
  table( y$values )

}
