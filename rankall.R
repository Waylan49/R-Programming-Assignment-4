rankall<-function(outcome, num= "best"){
        
        
        outcomeFirst<-read.csv("outcome-of-care-measures.csv", colClasses="character")

        x<-c("heart attack", "heart failure", "pneumonia")
        y<-NULL
        
        if(outcome %in% x){}
        else{
                stop("Invalid Outcome")
        }
        
        if(outcome == x[1]){
                y<-11
        }else if(outcome == x[2]){
                y<-17
        }else
        {
                y<-23
        }
        
        outcomeFirst[,y]<-as.numeric(outcomeFirst[,y])
        outcomeSecond<-split(outcomeFirst, outcomeFirst$State)
        
        for(i in 1:54){
                x<-outcomeSecond[[i]]
                v<-names(x)[2]
                outcomeSecond[[i]]<-x[order(x[v]),]
        }
        
        for(i in 1:54){
                x<-outcomeSecond[[i]]
                v<-names(x)[y]
                outcomeSecond[[i]]<-x[order(x[v]),]
        }  
        
        if(num == "best"){
                Result<-as.data.frame(matrix(nrow=54, ncol=2))
                Result[,2]<-as.character(Result[,2])
                colnames(Result)<-c("hospital", "state")
                for(i in 1:54){
                        Result[i,1]<-outcomeSecond[[i]][1, 2]
                        Result[i,2]<-outcomeSecond[[i]][1, 7]
                }
                Result
        }else if(num == "worst"){
                Result<-as.data.frame(matrix(nrow=54, ncol=2))
                Result[,2]<-as.character(Result[,2])
                colnames(Result)<-c("hospital", "state")
                for(i in 1:54){
                        x<-which.max(outcomeSecond[[i]][,y])
                        Result[i,1]<-outcomeSecond[[i]][x, 2]
                        Result[i,2]<-outcomeSecond[[i]][x, 7]
                }
                Result
                
        }else{
                Result<-as.data.frame(matrix(nrow=54, ncol=2))
                Result[,2]<-as.character(Result[,2])
                colnames(Result)<-c("hospital", "state")
                for(i in 1:54){
                        Result[i,1]<-outcomeSecond[[i]][num, 2]
                        Result[i,2]<-outcomeSecond[[i]][num, 7]
                }
                Result
        }
}
