library(ggplot2)
library(iptools)

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
    ipmap2 <- paste(ipmap$oct1, ipmap$oct2, '0', '0', sep = '.')
    ipmap <- cbind(ipmap,ipmap2)
    nummap <- ip_to_numberic(ipmap2)
    ipmap <- cbind(ipmap, nummap)
    return(ipmap)
}
    