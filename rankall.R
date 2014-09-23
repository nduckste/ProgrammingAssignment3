rankhospital <- function(state, outcome, num = "best") {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check that num  is valid
    if (!(num == "best" || num == "worst" || is.numeric(num))) {
        errmsg <- "invalid num. Valid values are 'best','worst', or an integer.\n"
        stop(errmsg)
    }
    
    ## Check that outcome is valid
    if (outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia") {
        errmsg <- "invalid outcome\n"
        stop(errmsg)
    }

    #create data.frame of unique states
#     states <- data.frame(stateval=unique(data[,7]))
    ## Check that state is valid, i.e. in data frame of unique states
    if (length(data[data[,7]==state,7])==0) {
#      checkstate <- subset(states,stateval==state)
#      if (nrow(checkstate) == 0) {
        errmsg <- "invalid state\n"
        stop(errmsg)
    }
    
    #generate the column index associated with the outcome parameter    
    oi <- data.frame(outcomedesc = c("heart attack","heart failure","pneumonia"), index=c(11,17,23))
    oiindex <- oi[oi[,"outcomedesc"]==outcome,"index"]

    #now pull hospitals for given state. Include the outcome column
    h <- data[data[,7] == state,c(2,7,as.numeric(oiindex))]

    #convert outcome column to numeric, supressing warning messages
    suppressWarnings(h[,3] <- as.numeric(h[,3]))

    #order data by outcome, hospital name, removing NAs
    h <- h[order(h[,3],h[,1],na.last=NA),]

    
    #Return hospital name in that state with lowest 30-day death rate
    if (num=="best") {
        #return the first row
        h[1,1]
    }
    else if (num=="worst"){
        #return the last row
        #tail(h[,1],1)
        h[nrow(h),1]
    }
    else {
        #Turn num into an integer
        num <- as.integer(num)
        
        #If num is greater than size of h return NA
        if (nrow(h) < num) {
            return(NA)
        }
        else {
            #return the row equal to num
            h[num,1]
        }
    }
}

# Rprof("best.out")
# y <- best("TX","heart attack")
# Rprof(NULL)