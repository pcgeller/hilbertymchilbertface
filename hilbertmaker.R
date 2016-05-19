library(ggplot2)
library(iptools)
library(reshape2)
library(d3heatmap)

clearHilbert <- function(){
    xlist <<- {}
    ylist <<- {}
}
calcHilbert <- function(){
    #How many points are you trying to plot
}

hilbert <- function(x0,y0,xi,xj,yi,yj,n){
   # cat("Hilbert function called\n")
    if (n <= 0){
       # cat("*** Point Calculated\n")
        x <- (x0 +(xi+yi)/2)
        y <- (y0 +(xj+yj)/2)
        xlist <<- c(xlist,x)
        ylist <<- c(ylist,y)
        #cat("*** x0:",x0,"y0:",y0,"xi:",xi,"xj:",xj,"yi:", yi,"yj:",yj,"N:",n,"\n")
    } else {
        hilbert(x0, y0, yi/2, yj/2, xi/2, xj/2, n-1)
           # cat(sprintf("point=A:::x0=%s:::y0=%s:::xi=%s:::xj=%s:::yi=%s:::yj=%s:::n-1=%s\n",
                 #   x0, y0, yi/2, yj/2, xi/2, xj/2, n-1))
            
        hilbert(x0+xi/2,y0+xj/2,xi/2,xj/2,yi/2,yj/2,n-1)
           # cat(sprintf("point=B:::x0=%s:::y0=%s:::xi=%s:::xj=%s:::yi=%s:::yj=%s:::n-1=%s\n",
                   # x0+xi/2,y0+xj/2,xi/2,xj/2,yi/2,yj/2,n-1))
            
        hilbert(x0+xi/2+yi/2,y0+xj/2+yj/2,xi/2,xj/2,yi/2,yj/2,n-1)
           # cat(sprintf("point=C:::x0=%s:::y0=%s:::xi=%s:::xj=%s:::yi=%s:::yj=%s:::n-1=%s\n",
                  #  x0+xi/2+yi/2,y0+xj/2+yj/2,xi/2,xj/2,yi/2,yj/2,n-1))
            
        hilbert(x0+xi/2+yi, y0+xj/2+yj,-yi/2,-yj/2,-xi/2,-xj/2,n-1)
           # cat(sprintf("point=D:::x0=%s:::y0=%s:::xi=%s:::xj=%s:::yi=%s:::yj=%s:::n-1=%s\n",
                   # x0+xi/2+yi, y0+xj/2+yj,-yi/2,-yj/2,-xi/2,-xj/2,n-1))
    }
}

drawHilbert <- function(xlist,ylist){
    xtemp <-unlist(xlist)
    ytemp <-unlist(ylist)
    points <- cbind(xlist,ylist)
    points <- as.data.frame(points)
    c <- ggplot(points, aes(xtemp,ytemp))
    c + geom_path()
}

makeIPmap <- function () {
    oct1 <- rep(0:255, each = 256)
    oct2 <- rep(0:255, 256)
    ipmap <- data.frame(oct1,oct2)
    #print(head(dat))
    ip <- paste(ipmap$oct1, ipmap$oct2, '0', '0', sep = '.')
    ipmap <- cbind(ipmap,ip)
    nummap <- ip_to_numeric(ip)
    ipmap <- cbind(ipmap, nummap)
    return(ipmap)
}

#assign IP counts within a range to the ip address interval 
#calls ipwrange()
countip <- function(dat) {
    output <- data.frame(rownumber = numeric(0), interval = numeric(0), count = numeric(0))
    #print(str(output))
    for(rownum in 1:nrow(dat)) {
        if(dat[rownum,"iprange"] == 0){
            intervals <- findInterval(dat[rownum,"ipnumeric"], ipmap$nummap)
            counts <- 1
            output <- rbind(output,cbind(rownum, intervals, counts))
            #print(output)
            cat("***SINGLE",rownum,"***\n")
        } else {
            #print(dat)
            cat("+++INTERVAL",rownum,"+++\n")
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

labeloct <- function(){
    x <- 1
    octlabel <- rep("",times = 65536)
    for(i in 0:255){
        octlabel[x] <- i
        x <- x + 256
    }
}

makeheatmap <- function(dat){
    data.heatmap <- acast(dat, xlist~ylist,value.var = "counts")
    heatmap.row <- rep("",times = 256)
    heatmap.col <- rep("",times = 256)
    d3heatmap(data.heatmap, Rowv = FALSE, Colv = FALSE, labRow = heatmap.row, labCol = heatmap.col)
}


    