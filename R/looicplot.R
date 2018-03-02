looicplot <- function(looiclist, modnames, perc=90) {
    looF <- function(x) loo::loo(x, cores = 1)
    n <- length(looiclist)
    model <- numeric(n)
    looics <- numeric(n)
    ic <- numeric(n)
    se <- numeric(n)
    lwr <- numeric(n)
    upr <- numeric(n)
    options("loo.cores" = 1)
    qq <- lapply(looiclist, looF)
    for(i in 1:n) {
        model[i] <- modnames[i]
        ic[i] <- qq[[i]][[3]]
        se[i] <- qq[[i]][[6]]
        upr[i] <- ic[i] - se[i] * qnorm((100-perc)/100)
        lwr[i] <- ic[i] + se[i] * qnorm((100-perc)/100)
    }
    df <- data.frame("model" = model, "looic"=ic, "se"=se,
                     "lwr" = lwr, "upr" = upr)
    rdr <- rev(order(df$looic))
    df <- df[rdr, ]   
    print(df)
    sumchart(df, df$model)
    invisible(df)
}
sumchart <- function(df, rownames=df$modnames, groups=rownames[1], perc=90) {
   n <- dim(df)[1]
   mn <- min(df[[2]]) 
   mx <- max(df[[2]]) 
   xrng <- c(mn, mx)
   xtr1 <- floor(xrng[1]/10)
   xtr2 <- ceiling(xrng[2]/10)
   xrng <- c(mn - xtr1, mx + xtr2)
   dotchart(df$looic, labels=rownames, xlim=xrng, xlab="Values")
   title("LOOIC-values")
   for(i in 1:n){
       segments(df$lwr[i], i, df$upr[i], i, lwd=4, col="skyblue")
       }
   }
