best<-function(STATE, disease){
        
        outcomeFirst<-read.csv("outcome-of-care-measures.csv", colClasses="character")
        outcomeSecond<-split(outcomeFirst, outcomeFirst$State)
        outcomeThird<-outcomeSecond[STATE]
        outcomeForth<-as.data.frame(outcomeThird)
        
        x<-c("heart attack", "heart failure", "pneumonia")
        y<-NULL
        if(STATE %in% outcomeFirst[,7]){}
        else{
                stop("Invalid Value")
        }
        
        if(disease %in% x){}
        else{
                stop("Invalid Outcome")
        }
        
        if(disease == x[1]){
                y<-11
        }else if(disease == x[2]){
                y<-17
        }else
        {
                y<-23
        }
        
        outcomeForth[,y]<-as.numeric(outcomeForth[,y])
        
        t<-table(outcomeForth[,y])
        if(t[1]==1){
                z<-which.min(outcomeForth[,y])
                w<-outcomeForth[,2][z]
                w 
        }
        else{
                q<-min(outcomeForth[,y], na.rm=TRUE)
                group<-which(outcomeForth[,y]==q)
                Comparison<-vector(length=length(group))
                for(i in 1:length(group)){
                        Comparison[i]<-outcomeForth[,2][group[i]]
                }
                Final<-sort(Comparison)
                Final[1]
           }
}




