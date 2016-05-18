#assign IP counts within a range to the ip address interval 
#calls ipwrange()
countip <- function(dat) {
    output <- data.frame(rownumber = numeric(0), interval = numeric(0), count = numeric(0))
    #print(str(output))
    for(rownum in 1:nrow(dat)) {
        if(dat[rownum,"iprange"] == 0){
            intervals <- findInterval(dat[rownum,"startnum"], ipmap$nummap)
            counts <- 1
            output <- rbind(output,cbind(rownum, intervals, counts))
            #print(output)
            cat("***",rownum,"***\n")
        } else {
            #print(dat)
            cat("+++",rownum,"+++\n")
            output.new <- ipwrange(dat,rownum)
            output <- rbind(output,output.new)
        }
    }
    return(output)
}

##NOT CALLED DIRECTLY