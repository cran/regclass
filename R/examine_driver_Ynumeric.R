examine_driver_Ynumeric <- function(formula,data,sort=TRUE) { 
  FORM <- as.formula(formula)
  variables <- as.character(attr(terms(FORM), "variables"))[-1]
  x <- data[,variables[2]]
  y <- data[,variables[1]]
  complete.x <- which(!is.na(x))
  complete.y <- which(!is.na(y))
  complete.cases <- intersect(complete.x, complete.y)
  x <- x[complete.cases]
  if(inherits(x,c("character","logical"))) { x <- factor(x) }
  y <- y[complete.cases]
  plot(y~x,xlab=variables[2],ylab=variables[1])
  abline(h=mean(y))
  M <- lm(FORM,data)
  if(inherits(x,c("integer","numeric"))) { 
    abline(M,col="blue",lwd=3)
  }
  if(inherits(x,c("integer","numeric"))) { 
    print( summary(M) )
  } else { 
    
    SUMMARY <- aggregate(FORM,data=data,FUN=mean)
    AOV <- aov(FORM,data=data)
    TUKEY <- TukeyHSD(AOV)
    LETTERS <- multcompView::multcompLetters4(AOV,TUKEY) 
    SUMMARY$letters <- LETTERS[[1]][1]$Letters[match(SUMMARY[,1],names( LETTERS[[1]][1]$Letters ) )]
    SUMMARY$n <- as.numeric(table(x))
    names(SUMMARY)[2] <- paste("Avg",variables[1],sep="")
    if(sort==TRUE) { SUMMARY <- SUMMARY[order(SUMMARY[,2],decreasing=TRUE),] }
    rownames(SUMMARY) <- NULL
    print(SUMMARY) }
  
  cat(paste("\nDriver Score:",round(summary(M)[8]$r.squared,digits=3),"\n"))
  cat(paste("Caution: misleading if driver is numerical and trend isn't linear.\n"))
  cat(paste("Scores range between 0 and 1.  Larger scores = stronger driver.\n"))
  cat(paste("Although context dependent, values above 0.02 or so are 'reasonably strong' drivers.\n"))
}
