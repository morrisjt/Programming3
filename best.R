best <- function(state,outcome)
{
    file <- read.csv("~/R_Code/hospital/outcome-of-care-measures.csv", colClasses = "character")
    
    ailment<-c( "heart attack", "heart failure", "pneumonia")
    
    #state<-"TX"
    #outcome <-"heart failure"
    
    #sub<-file[,c(2,7,11,17,23)]
    
    outcomeVector <- data.frame(Names = ailment,c(11,17,23))
    
    if(nrow(file[file$State== state,])==0)
    {
        stop("invalid state")
    }
    if(nrow(outcomeVector[outcomeVector$Names == outcome,])==0)
    {
        stop("invalid outcome")
    }
    
    ailment<-subset(file,file$State == state)
    
    ailmentSub<-ailment[order(ailment$Hospital.Name),]
    
    suppressWarnings(ailmentSub[,11]<-as.numeric(ailmentSub[,11]))
    suppressWarnings(ailmentSub[,17]<-as.numeric(ailmentSub[,17]))
    suppressWarnings(ailmentSub[,23]<-as.numeric(ailmentSub[,23]))
    
    numcol<-outcomeVector[outcomeVector$Names == outcome,2]

    answer<- as.numeric(which.min(ailmentSub[,numcol]))
    
    return(ailmentSub[answer,2])
}