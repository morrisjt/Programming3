rankhospital<-function(state,outcome,rank)
{
    
    file <- read.csv("~/R_Code/hospital/outcome-of-care-measures.csv", colClasses = "character")
    
    ailment<-c( "heart attack", "heart failure", "pneumonia")
    
    #state<-"TX"
    #outcome <-"heart failure"
    #rank<-4
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
    
    ailmentSub<-subset(file,file$State == state)
    suppressWarnings(ailmentSub[,11]<-as.numeric(ailmentSub[,11]))
    suppressWarnings(ailmentSub[,17]<-as.numeric(ailmentSub[,17]))
    suppressWarnings(ailmentSub[,23]<-as.numeric(ailmentSub[,23]))
    
    
    
    numcol<-outcomeVector[outcomeVector$Names == outcome,2]
    
    ailmentSub<-ailmentSub[complete.cases(ailmentSub[,numcol]),]
    ailmentSub<-ailmentSub[order(ailmentSub[,numcol],ailmentSub$Hospital.Name),]
    
    if(rank=="best")
    {
        rank<-1
    }
    if(rank=="worst")
    {
        rank<-nrow(ailmentSub)
    }
    if(rank>nrow(ailmentSub))
    {
        return(NA)
    }
    suppressWarnings(rank<-as.numeric(rank))
    
    return(ailmentSub[rank,2])    
    
    
}