suggest_levels <-
  function(formula,data,maxlevels=NA,target=NA,recode=FALSE,plot=TRUE,...) {
    
    FORM <- formula(formula)
    variables <- as.character(attr(terms(FORM), "variables"))[-1]
    if(length(variables)==1) { variables <- c(variables,variables) }
    x.label <- variables[2]
    y.label <- variables[1]
    
    
    #Define x and y, checking data first before the existing environment 
    if (inherits(try(eval(parse(text = variables[2]), envir = data), silent = TRUE), "try-error")) {
      x <- eval(parse(text = variables[2]))
    }
    else {
      x <- eval(parse(text = variables[2]), envir = data)
    }
    if (inherits(try(eval(parse(text = variables[2]), envir = data), silent = TRUE), "try-error")) {
      y <- eval(parse(text = variables[1]))
    }
    else {
      y <- eval(parse(text = variables[1]), envir = data)
    }   
    x <- factor(x)
    if(is.na(maxlevels)) { maxlevels <- min(26,nlevels(x)) }
    
    
    #Make sure user hasn't requested too many levels
    if(recode==TRUE & maxlevels>26) { stop("Can recode into a max of 26 levels") }
    
    #Make sure y is the right type of variable
    if (!inherits(y, c("integer", "numeric"))) {
      y <- factor(y) 
      if( length(levels(y)) > 2 ) { stop("Error:  y should only have two possible values.") } 
    }
    
    #Make sure there's enough data
    if( length(x) < 2 | length(y) < 2) { stop(paste("Error:  need at least 2 observations to proceed.  Currently, only have",length(x)," x and ",length(y),"y\n")) }
    
    #Make sure there's enough unique values
    if( length(unique(x)) < 2 | length(unique(y)) < 2) { stop(paste("Error:  need at least 2 unique values to proceed.")) }
    
    
    if (inherits(y, c("numeric", "integer"))) { TAB <- aggregate(y~x,FUN=mean) }
    if (inherits(y, c("factor"))) {  TAB <- aggregate(y~x,FUN=function(x)mean(x==levels(x)[1])) }
    
    TAB <- TAB[order(TAB$y),]

    PART <- rpart(y~x,data=TAB,control=rpart.control(cp=0,minbucket=1,minsplit = 1))
    
    if (inherits(y, c("numeric", "integer"))) { BIC <- BIC(lm(y~1)) }
    if (inherits(y, c("factor"))) {  BIC <- BIC(glm(y~1,family=binomial)) }
    
    if(missing(maxlevels)) { maxlevels <- nrow(TAB) }
    
    BIC.table <- data.frame(ID = 1:maxlevels, BIC=rep(BIC,maxlevels ), clusters = rep("all in one",maxlevels),stringsAsFactors = FALSE)

    for (tree in 2:maxlevels) {
      ID <- prune(PART,cp=PART$cptable[tree,1])$where
      x.temp <- x
      levels(x.temp) <- ID
      
      clustID <- c()
      uID <- unique(ID)

      for (j in uID) { clustID <- c(clustID, paste( TAB$x[which(ID==j)], collapse="&") ) }

      BIC.table$clusters[tree] <- paste("(",paste(clustID, collapse=")("),")",sep="")
      
      if (inherits(y, c("numeric", "integer"))) { BIC.table$BIC[tree] <- BIC(lm(y~x.temp)) }
      if (inherits(y, c("factor"))) { BIC.table$BIC[tree] <- BIC(glm(y~x.temp,family=binomial)) }
    }
      
   suggested <- min( which(BIC.table$BIC <= ( min(BIC.table$BIC) + 5 ) ) )
   if(is.na(target)) { 
   cat(paste(suggested,"suggested clusters:",BIC.table$clusters[suggested],"\n")) } else {
     suggested <- target
     cat(paste(suggested,"target clusters:",BIC.table$clusters[suggested],"\n"))
     
   }
   
   
   
   xx <- factor(x,levels=as.character( TAB$x ), ordered=TRUE )

  DF <- data.frame(y=y,xx=xx)
  if (inherits(y, c("factor"))) {
     if(plot==TRUE) { mosaic(y~xx,data=DF,...) }
    if(suggested>1) {
     ID <- prune(PART,cp=PART$cptable[suggested,1])$where
     break.x <- c(0, (1:length(levels(xx)) )/length(levels(x)) )
     if(plot==TRUE) { abline(v=break.x[1+which(diff(ID)!=0)],lwd=4,col="blue") }
     }     }
   

         

  if (inherits(y, c("numeric", "integer"))) {
     if(plot==TRUE) { plot(y~xx,xlab=x.label,ylab=y.label,...) }
     if(suggested>1) {
       ID <- prune(PART,cp=PART$cptable[suggested,1])$where
       if(plot==TRUE) { abline(v=0.5+which(diff(ID)!=0),lwd=4,col="red") }
     }
     
   }
   
   if(recode==FALSE){ 
    return(BIC.table) 
     }
   if(recode==TRUE) {
       if(suggested<=1) { 
           old.levels <- levels(x)
         new.levels <- factor(rep("A",length(old.levels)))
         RECODE <- data.frame(Old=old.levels,New=new.levels)
         RECODE2 <- RECODE[order(RECODE$New),]
         rownames(RECODE2) <- NULL
         return(list(Conversion=RECODE,Conversion2=RECODE2,newlevels=RECODE$New[as.numeric(x)]))
         
         }
       ID <- prune(PART,cp=PART$cptable[suggested,1])$where
       old.levels <- levels(x)
       new.levels <- c()
       z<-1
       for (j in unique(ID)) {
        new.levels[which( old.levels %in% as.character(TAB$x[which(ID==j)]) )] <- LETTERS[z]
        z<-z+1
       }
       RECODE <- data.frame(Old=old.levels,New=new.levels)
       RECODE2 <- RECODE[order(RECODE$New),]
       rownames(RECODE2) <- NULL
       return(list(Conversion=RECODE,Conversion2=RECODE2,newlevels=RECODE$New[as.numeric(x)]))
     
   }
    }
   
