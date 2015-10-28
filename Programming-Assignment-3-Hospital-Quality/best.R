#Clean R memory
#rm(list=ls(all=T))

best <- function(state, outcome) {

## Read outcome data
Data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

State <- names(table(Data$State))
 
Outcomes <-c("heart attack", "heart failure", "pneumonia")  #This Outcomes are valid
   
  if (!(state %in% State)) {
	             #Check that state are valid
	 	stop("invalid state, ")
	      #print(unique(Data[["State"]])) #This States are valid
      }
  else if(!(outcome %in% Outcomes )) {
	              # Check that outcome are valid
            stop("invalid outcome")
  	     }
    Col <- switch(outcome, "heart attack"=11, "heart failure"=17, "pneumonia"=23)
      
    state_select <- Data[Data[,7]==state,]
    Result <- state_select[,Col]
    Result <- suppressWarnings(as.numeric(Result,na.rm=T))
    min <- min(Result, na.rm=T) 
    index <- which(Result == min)

    #Hospital name in that state with lowest 30-day death     
    Hospital_name <- state_select[index, 2]  
	  
   return(Hospital_name)  
}

best("TX", "heart attack")
#[1] "CYPRESS FAIRBANKS MEDICAL CENTER"


