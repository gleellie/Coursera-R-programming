rankall <- function (outcome, num = "best"){
        
        ## Read outcome data
        read_data <- read.csv("outcome-of-care-measures.csv", na.strings="Not Available", 
                              stringsAsFactors = FALSE)
        outcomes_names <- c("heart attack", "heart failure","pneumonia")
        
        
        ## Check that state and outcome are valid
        if(!outcome %in% outcomes_names){
                stop(print("invalid outcome"))
                
        }else{ 
                col_index <- c("heart attack"=11, "heart failure"=17, "pneumonia"=23)
                outcome_selected <- read_data[,c(2,7,col_index[outcome])]
                colnames(outcome_selected) <- c("hospital", "state", "rank")
        
                ## order data by state, then outcome and hospital name
                
                order_data <- outcome_selected[order(outcome_selected[2], outcome_selected[3],
                                                     outcome_selected[1], decreasing = FALSE, na.last = NA), ]
                split_data <- split(order_data, order_data[2])
              
                ## For each state, find the hospital of the given rank
                ## Return a data frame with the hospital names and the
                ## (abbreviated) state name
                
                if(num == "best"){
                        lapplied <- lapply(split_data, function(x) {x[1,c(1,2)]})
                        data_final <- do.call(rbind, lapplied)
                        return(data_final) 
                        
                }else if(num == "worst"){
                        lapplied <-lapply(split_data, function(x) {x[nrow(x),c(1,2)]})
                        data_final <- do.call(rbind, lapplied)
                        return(data_final) 
                        
                        
                }else{
                        lapplied <-lapply(split_data, function(x) {x[num,c(1,2)]})
                        data_final <- do.call(rbind, lapplied)
                        return(data_final) 
                }
        }
}
