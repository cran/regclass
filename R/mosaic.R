mosaic <- function(formula, data, color = TRUE,labelat=c(),xlab=c(),ylab=c(),magnification=1,equal=FALSE,inside=FALSE,ordered=FALSE) 
{
  par(mfrow = c(1, 1))
  par(mar = c(5, 4, 4, 2) + 0.1)
  op <- options()
  FORM <- formula(formula)
  variables <- as.character(attr(terms(FORM), "variables"))[-1]
  if (length(variables) == 1) {
    variables <- c(variables, variables)
  }
  if(length(xlab)==0) { x.label <- variables[2] } else { x.label <- xlab }
  if(length(ylab)==0) { y.label <- variables[1] } else { y.label <- ylab }
  
  
  if (sum( class(try(eval(parse(text = variables[2]), envir = data), 
                silent = TRUE)) == "try-error")>0) {
    x <- eval(parse(text = variables[2]))
  }
  else {
    x <- eval(parse(text = variables[2]), envir = data)
  }
  if (class(try(eval(parse(text = variables[1]), envir = data), 
                silent = TRUE)) == "try-error") {
    y <- eval(parse(text = variables[1]))
  }
  else {
    y <- eval(parse(text = variables[1]), envir = data)
  }
  complete.x <- which(!is.na(x))
  complete.y <- which(!is.na(y))
  complete.cases <- intersect(complete.x, complete.y)
  x <- x[complete.cases]
  y <- y[complete.cases]
  if (head(class(x),1) != "ordered") {
    x <- factor(x)
  }
  if (head(class(y),1) != "ordered") {
    y <- factor(y)
  }
  if (length(x) < 2 | length(y) < 2) {
    stop(paste("Error:  need at least 2 observations to proceed.  Currently, only have", 
               length(x), "\n"))
  }
  n <- length(x)
  nx.levels <- length(unique(x))
  ny.levels <- length(unique(y))
  if (nx.levels < 2 | ny.levels < 2) {
    stop(paste("Error:  need at least 2 levels to proceed.  x has", 
               nx.levels, "and y has", ny.levels))
  }
  
  xlevel.names <- levels(x)
  if(length(labelat)>0) { xlevel.names[!(xlevel.names %in% labelat)] <- "" }
  
  ylevel.names <- levels(y)
  if(ordered==TRUE) {
    A <- aggregate(y~x,FUN=function(x)mean(x==levels(y)[1]))
    A$x <- as.character(A$x)
    A <- A[order(A$y,decreasing=TRUE),]
    x <- factor(x,ordered=TRUE,levels=A$x)
  }
  CONT.TAB <- table(x, y)
  CONT.TAB <- addmargins(CONT.TAB)
  rownames(CONT.TAB)[nx.levels + 1] <- "Total"
  colnames(CONT.TAB)[ny.levels + 1] <- "Total"
  O <- matrix(table(x, y), nrow = nx.levels, ncol = ny.levels)
  E <- (apply(O, 1, sum) %o% apply(O, 2, sum))/n
  par(mar = c(4, 4.2, 0.4, 2))
  plot(0, 0, col = "white", xlim = c(0, 1.3), ylim = c(-0.05, 
                                                       1), xlab = x.label, ylab = y.label, main = "", axes = FALSE)
  axis(1, at = seq(0, 1, 0.05))
  axis(2, at = seq(0, 1, 0.05))
  if (color == TRUE) {
    if (ny.levels > 8) {
      COLORS <- rainbow(2 * ny.levels - 1)[seq(1, by = 2, 
                                               length = ny.levels)]
    }
    else {
      COLORS <- 1:ny.levels
    }
  }
  else {
    COLORS <- grey(seq(0.1, 0.7, length = ny.levels))
  }
  marginal.y <- apply(O, 2, sum)/n
  break.y <- c(0, cumsum(marginal.y))
  for (i in 1:ny.levels) {
    rect(1.01, break.y[i], 1.11, break.y[i + 1], col = COLORS[i])
    text(1.1, (break.y[i + 1] + break.y[i])/2, ylevel.names[i], 
         srt = 0, pos = 4, cex = 1)
  }
  marginal.x <- apply(O, 1, sum)/n
  
  if(equal==FALSE) { break.x <- c(0, cumsum(marginal.x)) } else { break.x <- seq(0,1,length=1+length(xlevel.names)) }
  
  for (i in 1:nx.levels) {
    marginal.y <- O[i, ]/sum(O[i, ])
    break.y <- c(0, cumsum(marginal.y))
    for (j in 1:ny.levels) {
      rect(break.x[i], break.y[j], break.x[i + 1], break.y[j + 
                                                             1], col = COLORS[j])
    }
    if(inside==TRUE) { 
      text((break.x[i + 1] + break.x[i])/2, 0.5, xlevel.names[i],cex=magnification,srt=90,col=ifelse(color==FALSE,"red","white"))
    } else {
      text((break.x[i + 1] + break.x[i])/2, -0.05, xlevel.names[i],cex=magnification) }
    
  }
}

