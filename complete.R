complete <- function (directory, id = 1:332){
        list_files <- list.files(directory, full.names = TRUE)
        nobs <- numeric()
        for(i in id){
                dat <- read.csv(list_files[i])
                nobs <- c(nobs, sum(complete.cases(dat)))
        }
        data.frame(id, nobs)
}                
 
