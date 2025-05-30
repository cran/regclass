choose_order <-
  function(M,max.order=6,sort=FALSE,loc="topleft",show=NULL,...) {
    if(head(class(M),1)!="lm") { cat("Argument must be a fitted model\n"); return() }
    V <- names(M$coef)
    if(length(V)>2) { stop("This function is valid only for simple linear regression") }
    
    DATA <- M$model
    DATA <- DATA[order(DATA[,2]),]
    y <- DATA[,1]
    x <- DATA[,2]
    if(length(unique(x))<2) { stop("Need at least two unique x values to proceed") }
    n <- nrow(DATA)
    R2adj <- c()
    A <- c()
    par(mfrow=c(1,1))
    plot(y~x,xlab=V[2],ylab=V[3],...)
    max.order <- min(c(max.order),length(unique(x))-1)
    if(length(show)==0) { orders.to.show <- 1:max.order } else {
      orders.to.show <- show
    }
    for (i in orders.to.show) {
      M <- lm(y~poly(x,i))
      lines(x,fitted(M),col=i,lwd=2)
      k <- i+1
      A <- c(A,AIC(M)+2*k*(k+1)/(n-k-1))
      R2adj <- c(R2adj,summary(M)$adj.r.sq) }
    D <-data.frame(order=orders.to.show,R2adj=R2adj,AICc=A)
    if(sort==TRUE | sort=="R2adj" | sort=="r2adj" | sort=="r2" | sort=="R2") { D <- D[order(D$R2adj,decreasing=TRUE),] }
    if(sort=="aic" | sort=="AIC" | sort=="AICc" | sort=="aicc" | sort=="AICC") { D <- D[order(D$AICc),] }
    legend(loc,paste("Order",orders.to.show),col=orders.to.show,lty=1,lwd=2,cex=0.7) 
    rownames(D) <- NULL
    return(D)
  }
