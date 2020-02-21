confusion_matrix <-
function(M,DATA=NA) {

  is.caret.glm <- ( head(class(M),1)=="train" & (if(length(M$method>0)){M$method=="glm"}else{FALSE})  )
  if(is.caret.glm) {
    NEW <- M$trainingData
    form <- as.character(formula(M))
    names(NEW)[1] <- form[2]
    form <- as.formula(paste(form[2],form[1],form[-(1:2)]))
    M <- glm(form,data=NEW,family=binomial) 
  }
  
    if(missing(DATA)) { DATA <- M$model }
    D <- names(DATA)
    y.label <- as.character(M$terms[[2]])
    y.pos <- which(D==y.label)
    y.levels <- names(table(DATA[,y.pos]))
    y.pred <- ifelse( predict(M,newdata=DATA,type="response") >= 0.5, y.levels[2],y.levels[1])
    T<-table(DATA[,y.pos],y.pred,dnn=c())
    if(  dim(T)[2]==1  )  {
        colnames(T) <- paste("Predicted",names(which.max(table(DATA[,y.pos]))))
        rownames(T) <- c(paste("Actual",y.levels))
        cat("Predicted levels same as naive model (majority level)\n");
        return(T) }
    T<-rbind(T,apply(T,2,sum))
    T<-cbind(T,apply(T,1,sum))
    colnames(T) <- c(paste("Predicted",y.levels),"Total")
    rownames(T) <- c(paste("Actual",y.levels),"Total")
    T }
