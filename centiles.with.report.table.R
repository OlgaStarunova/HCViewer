centiles.with.report.table<-function (obj, xvar = NULL, cent = c(0.4, 2, 10, 25, 50, 75, 
    90, 98, 99.6), legend = TRUE, ylab = "y", xlab = "x", main = NULL, 
    main.gsub = "@", xleg = min(xvar), yleg = max(obj$y), xlim = range(xvar), 
    ylim = range(obj$y), save = FALSE, plot = TRUE, points = TRUE, 
    pch = "+", col = "blue", col.centiles = 1:length(cent) + 
        2, lty.centiles = 1, lwd.centiles = 1, accurancy = 0, ...) 
{
    if (!is.gamlss(obj)) 
        stop(paste("This is not an gamlss object", "\n", ""))
    if (is.null(xvar)) 
        stop(paste("The xvar argument is not specified", "\n", 
            ""))
    fname <- obj$family[1]
    qfun <- paste("q", fname, sep = "")
    Title <- paste("Centile curves using", fname, sep = " ")
    main <- if (is.null(main)) 
        paste("Centile curves using", fname, sep = " ")
    else gsub(main.gsub, Title, main)
    oxvar <- xvar[order(xvar)]
    oyvar <- obj$y[order(xvar)]
    if (is.matrix(obj$y)) {
        oyvar <- obj$y[, 1][order(xvar)]
        ylim <- range(obj$y[, 1])
        yleg = max(obj$y[, 1])
    }
    if (plot) {
        lty.centiles <- rep(lty.centiles, length(cent))
        lwd.centiles <- rep(lwd.centiles, length(cent))
        col.centiles <- rep(col.centiles, length(cent))
        if (points == TRUE) {
            plot(oxvar, oyvar, type = "p", col = col, pch = pch, 
                xlab = xlab, ylab = ylab, xlim = xlim, ylim, 
                ...)
        }
        else {
            plot(oxvar, oyvar, type = "n", col = col, pch = pch, 
                xlab = xlab, ylab = ylab, xlim = xlim, ylim, 
                ...)
        }
        title(main)
    }
    col <- 3
    lpar <- length(obj$parameters)
    ii <- 0
    myll<-data.frame()
    
    myll[1:length(oxvar),1] <- oxvar
    myll[1:length(oxvar),2] <- 0
    
    per <- rep(0, length(cent))
    for (var in cent) {
        if (lpar == 1) {
            newcall <- call(qfun, var/100, mu = fitted(obj, "mu")[order(xvar)])
        }
        else if (lpar == 2) {
            newcall <- call(qfun, var/100, mu = fitted(obj, "mu")[order(xvar)], 
                sigma = fitted(obj, "sigma")[order(xvar)])
        }
        else if (lpar == 3) {
            newcall <- call(qfun, var/100, mu = fitted(obj, "mu")[order(xvar)], 
                sigma = fitted(obj, "sigma")[order(xvar)], nu = fitted(obj, 
                  "nu")[order(xvar)])
        }
        else {
            newcall <- call(qfun, var/100, mu = fitted(obj, "mu")[order(xvar)], 
                sigma = fitted(obj, "sigma")[order(xvar)], nu = fitted(obj, 
                  "nu")[order(xvar)], tau = fitted(obj, "tau")[order(xvar)])
        }
        ii <- ii + 1
        ll <- eval(newcall)
        myll[1:length(ll),ii+2]<-ll
        if (plot) {
            lines(oxvar, ll, col = col.centiles[ii], lty = lty.centiles[ii], 
                lwd = lwd.centiles[ii], ...)
        }
        per[ii] <- (1 - sum(oyvar > ll)/length(oyvar)) * 100
    }
    myll[1:length(ll),ii+3]<-fitted(obj, "mu")
    
    myll[1:length(ll),ii+4]<-fitted(obj, "sigma")

    myll[1:length(ll),ii+5]<-fitted(obj, "nu")
    
    myll[1:length(ll),ii+6]<-fitted(obj, "tau")
    
    

    if (plot) {
        if (legend == TRUE) 
            legend(list(x = xleg, y = yleg), legend = cent, col = col.centiles, 
                lty = lty.centiles, lwd = lwd.centiles, ncol = 1, 
                ...)
    }
    if (save) 
    {
        return(cbind(cent, per))
    }
    
    names(myll) <- c("age", "size", cent, "mu", "sigma", "nu", "tau")
    
    #????????? ?????? ?? ????? ?????? ?? ???? ???
    myll$age <- round(myll$age, accurancy)

    #???????????? ????? ????????? ? ?????? ?????????? ??????
    f <- as.data.frame(table(factor(myll$age)))$Freq 
        
    myll<-myll[!duplicated(myll$age),]
    
    myll$size <- f
    
    print.data.frame(format(myll, digits = 4, decimal.mark = "."), row.names = FALSE)
    centiles.with.report.table<-myll
    
    
    
}
#centiles.with.report.table(mBCT, xvar=two.data.columns$age, cent=c(3, 15, 50, 85, 97), points= FALSE,main = "", xlab="???????", ylab="???? (??)", lwd=c(2), col.cent = "black", legend = FALSE, pch = 21, col = "gray")
