cor_demo <-
  function(cex.leg=0.8) {
    par(mfrow=c(1,1))
    plot(0,0,xlim=c(0,1),col="white",ylim=c(0,1),xlab="x",ylab="y",main="click plot to add points then click END below to stop demo",cex.main=0.9)
    rect(-1,.9,.2,2,col="white",border=NA);  text(0.1,0.95,"r2 =    ")
    rect(0.55,.9,2,2,col="white",border=NA);  text(0.8,0.95,"r2 (without red dot) =    ")
    
    rect(-1,-1,.1,.03,col="red");  text(0.01,0,"END",cex=0.8)
    rect(.9,-1,2,.03,col="red");  text(0.98,0,"UNDO",cex=0.8)
    
    x <- c(); y <- c()
    r.old <- 0
    keep.going <- 1
    while (keep.going==1) {
      new.point <- unlist(locator(1))
      if( new.point[1] > .9 & new.point[2] < .05 & length(x)>1 ) {  #Undo
        x <- x[-length(x)]
        y <- y[-length(y)]
        r <- cor(x,y)
        r.old <- r
        plot(0,0,xlim=c(0,1),col="white",ylim=c(0,1),xlab="x",ylab="y",main="")
        rect(-1,-1,.1,.03,col="red");  text(0.01,0,"END",cex=0.8)
        rect(.9,-1,2,.03,col="red");  text(0.98,0,"UNDO",cex=0.8)
        points(x,y,pch=20,cex=2,col="black")
        rect(-1,.9,.2,2,col="white",border=NA)
        text(0.075,1,paste("r =",round(r,digits=2)))
        text(0.075,0.95,paste("r2 =",round(r^2,digits=2)))
        rect(0.55,.9,2,2,col="white");  
      } else {
        
        if( new.point[1] > .1 | new.point[2] > .05 ) {   #Did not end
          points(x,y,pch=20,cex=2,col="black")
          points(new.point[1],new.point[2],pch=20,cex=2,col="red")
          x <- c(x,new.point[1]);  y <- c(y,new.point[2])
          r <- cor(x,y)
          rect(-1,.9,.2,2,col="white",border=NA)
          text(0.075,1,paste("r =",round(r,digits=2)))
          text(0.075,0.95,paste("r2 =",round(r^2,digits=2)))
          if(length(x)>2) {
            rect(0.55,.9,2,2,col="white",border=NA)
            text(0.8,1,paste("r (without red dot) =",round(r.old,digits=2)))
            text(0.8,0.95,paste("r2 (without red dot) =",round(r.old^2,digits=2))) }
          r.old <- r
          
        } else { keep.going <- 0 } }
    }
  }
