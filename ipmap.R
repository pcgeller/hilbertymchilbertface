makeipmap <- function () {
    oct1 <- rep(0:255, each = 256)
    oct2 <- rep(0:255, 256)
    ipmap <- data.frame(oct1,oct2)
    #print(head(dat))
    ipmap2 <- paste(ipmap$oct1, ipmap$oct2, '0', '0', sep = '.')
    ipmap <- cbind(ipmap,ipmap2)
}