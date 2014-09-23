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

    #order data by outcome, hospital name, removing NAs
    h <- h[order(h[,2],h[,3],h[,1],na.last=NA),]

    #Return hospital name in that state with lowest 30-day death rate
    if (num=="best") {
        #Sor ascending
        h <- h[order(h[,2],h[,3],h[,1],na.last=NA),]
    }
    else if (num=="worst"){
        #Sort descending
        h <- h[order(h[,2],-h[,3],h[,1],na.last=NA),]
    }
    else {
        #Sor ascending
        h <- h[order(h[,2],h[,3],h[,1],na.last=NA),]
    }

    #     create data.frame of unique states
    states <- data.frame(stateval=unique(data[,7]))
    states <- sort(states[,"stateval"])
    for (s in states) {
        #bring back top row where s == state
        if (num=="best" || num=="worst") {
            data_subset <- head(h[h[,2] == s,1:2],1)
        }
        #bring back last row where s == state
        else if (num=="worst") {
            data_subset <- tail(h[h[,2] == s,1:2],1)
        }
        #bring back num row where s == state
        else  {
            data_subset <- h[h[,2] == s,1:2]
            data_subset <- data_subset[num,]
        }
        
        #assign s (current state) to the State column in case row was NA
        data_subset[,"state"]=s
        if (exists("d")) {
            d <- rbind(d, data_subset)
        }
        #else dataset the first time, assinging data_subset to it.
        else {
            d <- data_subset
        }
    }
    d
}

# Rprof("best.out")
# y <- best("TX","heart attack")
# Rprof(NULL)
# head(rankall("heart attack",20),10)
# tail(rankall("pneumonia","worst"),3)
# tail(rankall("heart failure"),10)
