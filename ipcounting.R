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
#Takes the cleaned df from krampus
#return the intervals and count of ips in the interval from a range
ipwrange <- function(dat,rownum){
    if (dat[rownum,"iprange"] <= 65536){
        intervals <- findInterval(dat[rownum,"startnum"],data.map$nummap)
        counts <- dat[rownum,"iprange"]
        output <- cbind(rownum,intervals,count)
    } else {
        iprange <- dat[rownum,"iprange"]
            #cat(IPs in range: ", iprange, "\n")
        firstint <- findInterval(dat[rownum,"startnum"],data.map$nummap)
            #cat("First Interval: ",firstint, "\n")
        firstipmap <- data.map[firstint,"nummap"]
            #cat("start IP# of First Interval: ", firstipmap, "\n")
        firstcount <- dat[rownum,"startnum"] - firstipmap
            #cat ("Coun6t of IPs in first interval: ", firstcount,"\n")
        quo <- iprange %/% 65536
        rem <- iprange %% 65536
            cat("Total number of intervals: ", quo, "Total number of ips remaining: ", rem, "\n")
            #need to include last interval calcs for non roun intervals IE remainder = 0
        lastint <- firstint + quo + 1 
        lastcount <- rem
        fullintervals <- (firstint+1):(firstint+quo)
        intervals <- firstint:(firstint+quo+1)
        counts <- c(firstcount, rep(65536, times = length(fullintervals)),lastcount)
        output <- cbind(rownum,intervals,counts)
    }
return(output)
}

labeloct <- function(){
    x <- 1
    octlabel <- rep("",times = 65536)
    for(i in 0:255){
        octlabel[x] <- i
        x <- x + 256
    }
}

#run all the functions together
makeHilbert <- function(data, n) {
    clearHilbert()
    data.curve <- hilbert(0,0,1,0,0,1,n)
    data.map <<- makeIPmap()
    intervalcount <- countip(data)
    countfinal <- aggregate(intervalcount['counts'], by = intervalcount['intervals'],sum)
    data.map$intervals <- 1:65536
    data.hilbert <- left_join(data.map,countfinal, by = "intervals")
    data.hilbert$counts <- ifelse(is.na(data.hilbert$counts),0,data.hilbert$counts)
    data.hilbert$counts <- ifelse(data.hilbert$counts > 65536, 65536, data.hilbert$counts)
    return(data.hilbert)
}
    

makeHeatmap <- function(dat){
    data.heatmap <- acast(dat, xlist~ylist,value.var = "counts")
    heatmap.row <- rep("",times = 256)
    heatmap.col <- rep("",times = 256)
    d3heatmap(data.heatmap, Rowv = FALSE, Colv = FALSE, labRow = heatmap.row, labCol = heatmap.col)
}

    