rankhospital <- function(state, outcome) {
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")

    ## Check that outcome is valid
    if (outcome != "heart attack" && outcome != "heart failure" && outcome != "pneumonia") {
        errmsg <- " : invalid outcome\n"
        stop(errmsg)
    }

    #create data.frame of unique states
#     states <- data.frame(stateval=unique(data[,7]))
    ## Check that state is valid, i.e. in data frame of unique states
    if (length(data[data[,7]==state,7])==0) {
#      checkstate <- subset(states,stateval==state)
#      if (nrow(checkstate) == 0) {
        errmsg <- " : invalid state\n"
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
    #That would be the first one in the ordered data.frame.
    h[1,1]
}

# Rprof("best.out")
# y <- best("TX","heart attack")
# Rprof(NULL)