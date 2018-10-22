rankhospital<-function(STATE, disease, num= "best"){
        
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
        
        outcomeForth[,y]<-as.numeric(outcomeForth[,y]) #By using order function to sort the data
        v<-names(outcomeForth)[2]
        outcomeForth<-outcomeForth[order(outcomeForth[v]),]
        w<-names(outcomeForth)[y]
        outcomeForth<-outcomeForth[order(outcomeForth[w]),]
        
        if(num == "best"){
                outcomeForth[,2][1]
        }else if(num == "worst"){
                m<-which.max(outcomeForth[,y])
                outcomeForth[,2][m]
        }else if(num <= nrow(outcomeForth)){
                outcomeForth[,2][num]
        }else{
                NA
        }
}




