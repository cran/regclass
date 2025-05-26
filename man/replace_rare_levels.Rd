\name{replace_rare_levels}
\alias{replace_rare_levels}

\title{
Replaces rare levels of a categorical variable
}
\description{
This function takes a categorical variable and replaces all levels with frequencies less than or equal to a user-specified threshold named \code{Other}
}
\usage{
replace_rare_levels(x,threshold=20,newname="Other")
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{x}{a vector of categorical values}
  \item{threshold}{levels that appear a total of \code{threshold} times or fewer will be combined into a new level called \code{Other} }
  \item{newname}{defaults to \code{Other}, but give the option as to what this new level will be called}
}

\details{
Returns the recoded values of the categorical variable. All levels which appeared \code{threshold} times or fewer are now known as \code{Other}

If, after being combined, the \code{newname} level has \code{threshold} or fewer instances, the remaining level that appears least often is combined as well.
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
	
	#Replace all levels which appear 700 or fewer times (AA, CC, DD)
	y <- replace_rare_levels(x,700)
  table( y )
  
  #Replace all levels which appear 1350 or fewer times.  This forces BB (which
  #occurs 2422 times) into the Other level since the three levels that appear
  #fewer than 1350 times do not appear more than 1350 times combined
	y <- replace_rare_levels(x,1350)
  table( y )

}
