corr <- function(directory, threshold = 0){
        list_files <- list.files(directory, full.names = TRUE)
        cor <- numeric()
        for (i in 1:332){
                dat1 <- read.csv(list_files[i])
                sums <- sum(complete.cases(dat1))
                if (sums > threshold){
                       cor<- c(cor, cor(dat1$sulfate, dat1$nitrate, use = "na.or.complete"))
                }else{
                        cor
                }
        }
        cor
}        
       
        
        
