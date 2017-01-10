all_correlations <- 
function(X,type="pearson",interest=NA,sorted="none") {
    if(sorted!="none" & type=="both") { stop("Sorting is only supported if type='pearson' or type='spearman'")}
    if(class(X)!="data.frame") { 
      stop(paste("This function only accepts data frames.",deparse(substitute(X)),"is a",class(X),"\n"))
    }
    if(type!="pearson" & type != "spearman" & type != "both") { 
      stop(paste("type argument must be 'pearson', 'spearman' or 'both', you entered",type,"\n")) }
    
    if( !is.na(interest) & length(intersect(interest,names(X)))==0) { 
      stop(paste(deparse(substitute(interest)),"not found in the data frame",deparse(substitute(X)),"\n")) }
    
     nvar <- ncol(X)
    
    #Turn off/on warnings
    old.warn <- options()$warn
    options(warn = -1)
    
    if(type!="both") {
    RESULTS <- data.frame(var1=names(X),var2=names(X),correlation=rep(0,length(names(X))),pval=rep(rep(0,length(names(X))) ))
    
    
    
    z<-1
    for (i in 1:(nvar-1)) {
      for (j in (i+1):nvar) {
        if( (class(X[,i])=="numeric" | class(X[,i]) =="integer") & (class(X[,j])=="numeric" | class(X[,j]) =="integer"))      {
          COR <- cor.test(X[,i],X[,j],method=type)
          RESULTS[z,] <- list(names(X)[i],names(X)[j],COR$est, COR$p.val)
          z<-z+1
        }
      }}
    
    }
    
    if(type=="both") {
      RESULTS <- data.frame(var1=names(X),var2=names(X),pearson=rep(0,length(names(X))),pearpval=rep(rep(0,length(names(X))) ),
                            spearman=rep(0,length(names(X))),spearpval=rep(rep(0,length(names(X))) ) )
      z<-1
      for (i in 1:(nvar-1)) {
        for (j in (i+1):nvar) {
          if( (class(X[,i])=="numeric" | class(X[,i]) =="integer") & (class(X[,j])=="numeric" | class(X[,j]) =="integer"))      {
            COR1 <- cor.test(X[,i],X[,j],method="pearson")
            COR2 <- cor.test(X[,i],X[,j],method="spearman")
            RESULTS[z,] <- list(names(X)[i],names(X)[j],COR1$est, COR1$p.val,COR2$est, COR2$p.val)
            z<-z+1
          }
        }}
      
      
    }
    options(warn = old.warn)


    RESULTS <- RESULTS[1:(z-1),]
    if( !is.na(interest) ) {
        selected <- union(which(RESULTS[,1]==interest),which(RESULTS[,2]==interest))
        unselected <- setdiff(1:(z-1),selected)
        RESULTS <- RESULTS[c(selected),]
    }
    if(sorted=="strength" & type!="both") {
        RESULTS <- RESULTS[order(RESULTS$correlation),]
    }
    if(sorted=="magnitude" & type!="both") {
      RESULTS <- RESULTS[order(abs(RESULTS$correlation),decreasing = TRUE),]
    }
    if(sorted=="significance" & type!="both") {
      RESULTS <- RESULTS[order(RESULTS$pval),]
    }
    rownames(RESULTS)<-NULL
    return(RESULTS)
}
