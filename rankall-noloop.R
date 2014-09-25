rankall <- function(outcome, num = "best") {
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
    
    #generate the column index associated with the outcome parameter    
    oi <- data.frame(outcomedesc = c("heart attack","heart failure","pneumonia"), index=c(11,17,23))
    oiindex <- oi[oi[,"outcomedesc"]==outcome,"index"]
    
    #now pull hospitals, state, and outcome
    h <- data[,c(2,7,as.numeric(oiindex))]
    names(h) <- c("hospital","state","rate")
    
    #convert outcome column to numeric, supressing warning messages
    suppressWarnings(h[,"rate"] <- as.numeric(h[,"rate"]))
    
    #Return hospital name in that state with lowest 30-day death rate
    if (num=="best") {
        #Sort ascending
        #order data  by outcome, hospital name, removing NAs
        h <- h[order(h[,"state"],h[,"rate"],h[,"hospital"],na.last=NA),]
        rowindex <- 1
    }
    else if (num=="worst"){
        #Sort descending
        #order data  by outcome, hospital name, removing NAs
        h <- h[order(h[,"state"],-h[,"rate"],h[,"hospital"],na.last=NA),]
        rowindex <- 1
    }
    else {
        #Sort ascending
        #order data  by outcome, hospital name, removing NAs
        h <- h[order(h[,"state"],h[,"rate"],h[,"hospital"],na.last=NA),]
        rowindex <- 1
        
        #Turn num into an integer
        rowindex <- as.integer(num)
        
    }
    
    as.data.frame(
        #This generates a matrix
        mat <- do.call("rbind", as.list(
            by(h, list(state=h$state), function(x){
                #return a data.frame
                #                 col1 = name of hospital associated with rowindex 
                #                 col2 = name of the first state
                data.frame(hospital=x[rowindex,"hospital"], state=x[1,"state"])
            }
            )
        )
        )
    )
}

# Rprof("best.out")
# y <- best("TX","heart attack")
# Rprof(NULL)
# head(rankall("heart attack",20),10)
# tail(rankall("pneumonia","worst"),3)
# tail(rankall("heart failure"),10)
