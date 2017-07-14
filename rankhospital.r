rankhospital <- function(state, outcome, num = "best") {
        ## Read outcome data
        read_data <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", 
                              stringsAsFactors = FALSE)
        outcomes_names <- c("heart attack", "heart failure","pneumonia")
        
        
        ## Check that state and outcome are valid
        if(!state %in% read_data$State){
                stop(print("invalid state"))
        }else if(!outcome %in% outcomes_names){
                stop(print("invalid outcome"))
        
        }else{ 
                col_index <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
                outcome_selected <- read_data[,c(2,7,col_index[outcome])]
                names(outcome_selected)[3] <- "Rate"
                
                state_selected <- subset(outcome_selected, outcome_selected$State == state)
                
                order_data <- state_selected[order(state_selected$Rate,state_selected$Hospital.Name,
                                                  na.last = NA, decreasing = FALSE),]
                
                Rank <- as.numeric(1:nrow(order_data))
                final_data <- cbind(order_data, Rank)
                
                ## Return hospital name in that state with the given rank
                ## 30-day death rate
                if(num == "best"){
                        subset(final_data$"Hospital.Name", final_data$Rank == 1)
                }else if(num == "worst"){
                        subset(final_data$"Hospital.Name", final_data$Rank == nrow(final_data))
                }else if (num > nrow(final_data) ){
                        print(NA)
                }else {
                        subset(final_data$"Hospital.Name", final_data$Rank == num)
                }
        } 
}
