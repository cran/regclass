replace_rare_levels <- function(x, threshold = 20, newname = "Other")  {
  x <- factor(x)
  rare.levels <- names(which(sort(table(x)) <= threshold))
  if (length(rare.levels) == 0) { return(x) }
  levels(x)[which(levels(x) %in% rare.levels)] <- newname
  ST <- sort(table(x))
  if (ST[newname] <= threshold) {
    levels.to.combine <- which(levels(x) %in% c(newname, 
                                                names(ST)[2]))
    levels(x)[levels.to.combine] <- newname
    rare.levels <- c(rare.levels, names(ST)[2])
  }
  return(factor(as.character(x)))
}