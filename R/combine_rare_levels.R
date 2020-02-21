combine_rare_levels <- function(x,threshold=20,newname="Combined") { 
  x <- factor(x)
  rare.levels <- names( which( sort( table(x) ) <= threshold ) )
  if(length(rare.levels)==0) { return(list(values=x,combined=NULL)) }
  
  levels(x)[ which(levels(x) %in% rare.levels) ] <- newname
  ST <- sort(table(x))
  if(ST["Combined"]<=threshold) {  #Combined will be the least frequent level
    levels.to.combine <- which( levels(x) %in% c(newname,names(ST)[2]))
    levels(x)[levels.to.combine] <- newname
    rare.levels <- c(rare.levels,names(ST)[2])}
  return(list(values=x,combined=rare.levels))
}

