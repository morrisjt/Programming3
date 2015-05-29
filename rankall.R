rankall<-function(outcome,rank)
{
    
    file <- read.csv("~/R_Code/hospital/outcome-of-care-measures.csv", colClasses = "character")
    file<-file[order(file$State),]
    ailment<-c( "heart attack", "heart failure", "pneumonia")
    
    state<-"WV"
    outcome <-"pneumonia"
    rank<-"worst"
    
    outcomeVector <- data.frame(Names = ailment,c(11,17,23))
    
    
    if(nrow(outcomeVector[outcomeVector$Names == outcome,])==0)
    {
        stop("invalid outcome")
    }
    #print(unique(file$State))
    holder<-NULL
    rankHolder<-rank
    for(state in unique(file$State))
    {
        ailmentSub<-subset(file,file$State == state)
        suppressWarnings(ailmentSub[,11]<-as.numeric(ailmentSub[,11]))
        suppressWarnings(ailmentSub[,17]<-as.numeric(ailmentSub[,17]))
        suppressWarnings(ailmentSub[,23]<-as.numeric(ailmentSub[,23]))
        
        
        
        numcol<-outcomeVector[outcomeVector$Names == outcome,2]
        
        ailmentSub<-ailmentSub[complete.cases(ailmentSub[,numcol]),]
        ailmentSub<-ailmentSub[order(ailmentSub[,numcol],ailmentSub$Hospital.Name),]
        
        if(rank=="best")
        {
            rankHolder<-1
        }
        if(rank=="worst")
        {
            rankHolder<-nrow(ailmentSub)
        }
     #   if(rank>nrow(ailmentSub))
      #  {
       #     return(NA)
        #}
        suppressWarnings(rankHolder<-as.numeric(rankHolder))
        #print(c(ailmentSub[rank,2],state))
        holder<-rbind(holder,c(ailmentSub[rankHolder,2],state))
        #holder2<-c(ailmentSub[rank,7],ailmentSub[rank,2])
    }
    holder<-data.frame(holder)
   colnames(holder)[1]<-"hospital"
    colnames(holder)[2]<-"state"
    return(holder)    
    
    
}