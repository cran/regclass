examine_driver_Ycat <- function(formula,data,sort=TRUE,inside=TRUE,equal=TRUE) { 
  FORM <- as.character(formula)
  temp <- stringr::str_extract(FORM,"^.*==")
  temp <- temp[!is.na(temp)]
  y.label <- gsub(" ==","",temp)
  temp <- stringr::str_extract(FORM,'\\"[:alnum:]*')
  temp <- temp[!is.na(temp)]
  level <- substr(temp,2,nchar(temp))
  FORM <- as.formula(formula)
  
  variables <- as.character(attr(terms(FORM), "variables"))[-1]
  x.label <- variables[2]
  data <- data[,c(x.label,y.label)]
  if (!inherits(data[[y.label]], "ordered")) {
    data[[y.label]] <- factor(data[[y.label]])
  }
  data[,y.label] <- factor(data[,y.label],ordered=TRUE,levels=c(level,setdiff(levels(data[,y.label]),level)))
  if(nlevels(data[,y.label])>2) { levels(data[,y.label]) <- c(levels(data[,y.label])[1],rep(paste("Not",level),nlevels(data[,y.label])-1)) }
  x <- data[,x.label]
  y <- data[,y.label]
  
  color <- FALSE
  labelat=c(); xlab=c(); ylab=c(); magnification=1
  complete.x <- which(!is.na(x))
  complete.y <- which(!is.na(y))
  complete.cases <- intersect(complete.x, complete.y)
  x <- x[complete.cases]
  y <- y[complete.cases]
  if (!inherits(x, "ordered")) {
    x <- factor(x)
  }
  if (!inherits(y, "ordered")) {
    y <- factor(y)
  }
  data[,1] <- x
  data[,2] <- y
  n <- length(x)
  nx.levels <- length(unique(x))
  ny.levels <- length(unique(y))
  if (nx.levels < 2 | ny.levels < 2) {
    stop(paste("Error:  need at least 2 levels to proceed.  x has", 
               nx.levels, "and y has", ny.levels))
  }
  if (nx.levels > 100) {
    stop(paste("Error:  function not designed for more than 100 levels of x"))
  }
  
  xlevel.names <- levels(x)
  if(length(labelat)>0) { xlevel.names[!(xlevel.names %in% labelat)] <- "" }
  
  ylevel.names <- levels(y)
  CONT.TAB <- table(x, y)
  CONT.TAB <- addmargins(CONT.TAB)
  rownames(CONT.TAB)[nx.levels + 1] <- "Total"
  colnames(CONT.TAB)[ny.levels + 1] <- "Total"
  O <- matrix(table(x, y), nrow = nx.levels, ncol = ny.levels)
  E <- (apply(O, 1, sum) %o% apply(O, 2, sum))/n
  plot(0, 0, col = "white", xlim = c(0, 1.3), ylim = c(-0.05, 
                                                       1), xlab=ifelse(equal==TRUE,"",x.label),cex.main=0.7,main = "", ylab = paste("Probability",y.label,"is",level), axes = FALSE)
  if(equal==FALSE) { axis(1, at = seq(0, 1, 0.05)) }
  axis(2, at = seq(0, 1, 0.05))
  COLORS <- grey(seq(0.1, 0.7, length = ny.levels))
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
      text((break.x[i + 1] + break.x[i])/2, 0.5, xlevel.names[i],cex=magnification,srt=90,col="white")
    } else {
      text((break.x[i + 1] + break.x[i])/2, -0.05, xlevel.names[i],cex=magnification) }
    
  }
  lines(c(0,1),rep(mean(data[,y.label]==level),2),lwd=2,col="red")
  if(equal==TRUE) { text(0.5,-0.05,x.label)   }
  FORM3 <- formula(paste(y.label,"=='",level,"'~", x.label,sep=""))
  SUMMARY <- aggregate(FORM3,data=data,FUN=mean)
  AOV <- aov(FORM3,data=data)
  TUKEY <- TukeyHSD(AOV)
  LETTERS <- multcompView::multcompLetters4(AOV,TUKEY) 
  SUMMARY$letters <- LETTERS[[1]][1]$Letters[match(SUMMARY[,1],names( LETTERS[[1]][1]$Letters ) )]
  SUMMARY$n <- as.numeric(table(data[,x.label]))
  names(SUMMARY)[2] <- paste("Prob",level,sep="")
  if(sort==TRUE) { SUMMARY <- SUMMARY[order(SUMMARY[,2],decreasing=TRUE),] }
  rownames(SUMMARY) <- NULL
  print(SUMMARY)
  R2 <- summary(lm( formula, data ) )[8]$r.squared
  cat(paste("\nDriver Score:",round(R2,digits=3),"\n"))
  cat(paste("Scores range between 0 and 1.  Larger scores = stronger driver.\n"))
  cat(paste("Although context dependent, values above 0.02 or so are 'reasonably strong' drivers.\n"))
}