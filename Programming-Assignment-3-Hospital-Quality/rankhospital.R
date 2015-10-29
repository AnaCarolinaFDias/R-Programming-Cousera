rm(list=ls(all=T))

rankhospital <- function(state, outcome, num = "best") {

## Read outcome data

Data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
State <- names(table(Data$State))
Outcomes <-c("heart attack", "heart failure", "pneumonia")
   
if (!state %in% State) {
	 #Check that state are valid
        stop("invalid state")
}
  else if(!(outcome %in% Outcomes )) {
	 #Check that outcome are valid
            stop("invalid outcome")
  	     }
Col <- switch(outcome, "heart attack"=11, "heart failure"=17, "pneumonia"=23)
    
    select_state <- Data[Data[,7]== state,]
    select_state[,Col] <- suppressWarnings(as.numeric(select_state[,Col]))
    select_state<-select_state[c(2,Col)]
    select_state<- na.omit(select_state)
 
 # Hospital name = [,1] , Outcome = [,2]
 select_state <- select_state[order(select_state[,2],select_state[,1]),]

switch(num, "best" = {num = 1}, "worst" = {num = nrow(select_state)})
if (num > nrow(select_state)) {
	return(NA)
}        

 Hospital_Names <- select_state[num, ]$Hospital.Name

 	return(Hospital_Names)
}
rankhospital("TX", "heart failure", 4)
rankhospital("MD", "heart attack", "worst")
