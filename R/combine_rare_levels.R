combine_rare_levels <- function(x,threshold=20) { 
  x <- factor(x)
  rare.levels <- names( which( sort( table(x) ) <= threshold ) )
  if(length(rare.levels)==0) { return(list(values=x,combined=NULL)) }
  
  levels(x)[ which(levels(x) %in% rare.levels) ] <- "Combined"
  ST <- sort(table(x))
  if(ST["Combined"]<=threshold) {  #Combined will be the least frequent level
    levels(x)[1:2] <- "Combined"
    rare.levels <- c(rare.levels,levels(x)[2])}
  return(list(values=x,combined=rare.levels))
}

