best <- function(state, outcome) {
        ## Read outcome data
        read_data <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", 
                              stringsAsFactors = FALSE)
        outcomes <- read_data[,c(2,7,11,17,23)]
        colnames(outcomes) <- c("hospital", "state name", "heart attack", "heart failure",
                                "pneumonia")
        outcomes_names <- c("heart attack", "heart failure","pneumonia")
        
        ## Check that state and outcome are valid
        if(!state %in% outcomes$`state name`){
                stop(print("invalid state"))
        }else if(!outcome %in% outcomes_names){
                stop(print("invalid outcome"))
        }else{
                col_index <- c("heart attack"=3, "heart failure"=4, "pneumonia"=5)
                outcomes_selected <- outcomes[,c(1,2,col_index[outcome])]
                state_selected <- subset(outcomes_selected, 
                                         outcomes_selected$'state name' == state)
                
                ## Return hospital name in that state with lowest 30-day death
                order_data <- state_selected[order(state_selected[3],state_selected[1],
                                                   na.last = NA, decreasing = FALSE),]
                order_data[1,1]
        }
}