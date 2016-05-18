library(ggplot2)
library(iptools)

Hilbert <- function(){
    xlist <<- {}
    ylist <<- {}
}

hilbert <- function(x,y,xi,xj,yi,yj,n){
    print("Hilbert function called")
    if (n <= 0){
        print("Point Calculated")
        x0 <- (x +(xi+yi)/2)
        y0 <- (y +(xj+yj)/2)
        xlist <<- c(xlist,x0)
        ylist <<- c(ylist,y0)
        cat("/////////x:",x,"Y:",y,"xi:",xi,"xj:",xj,"yi:", yi,"yj:",yj,"N:",n,"\n")
    } else {
        hilbert(x, y, yi/2, yj/2, xi/2, xj/2, n-1)
            cat("POINT A:", n,"X:", x, "Y:", y, "\n")
        hilbert(x+xi/2,y+xj/2,xi/2,xj/2,yi/2,yj/2,n-1)
            cat("POINT B:", n,"X:",x,"Y:",y,"\n")
        hilbert(x+xi/2+yi/2,y+xj/2+yj/2,xi/2,xj/2,yi/2,yj/2,n-1)
            cat("POINT C:", n, "X:",x,"Y:", y, "\n")
        hilbert(x+xi/2+yi, y+xj/2+yj,-yi/2,-yj/2,-xi/2,-xj/2,n-1)
            cat("POINT D:", n, "X:",x,"Y:",y,"\n")
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
    