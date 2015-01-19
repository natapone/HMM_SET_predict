get_time_series_set <- function(symbol, directory = "export", period = 200) {
    source("set_util.R")
    prices <- read_symbol_column(directory, symbol, "close")
    rcount <- nrow(prices)
    if(rcount > period) {
        price <- prices[ ((rcount-period)+1):rcount , symbol]
    } else {
        price <- prices[symbol]
    }
    
    #TODO: fix missing date and assign day of week
    
    # create time serie
    # assume trading data Mon - Fri
    #print(price)
    price <- ts(price,frequency=4)
    #print(price)
    price
    
    # fit the regression
    #W = factor(rep(1:5,40)) # Days of week factors
    #t = time(price)
    #reg_s = lm(log(price)~0+t+W, na.action=NULL)
    #summary(reg_s)
    
}

#source("set_time_series.R")
#price <- get_time_series_set(symbol = "DTAC")

#source("set_time_series.R")
#plot_density(price)

plot_density <- function(price) {
    dlp = diff(log(price))
    #dlp = diff(price)
    print(shapiro.test(dlp)) # just normality test
    
    hist(dlp, prob=TRUE, 10)
    d <- density(dlp)
    lines(d)
    
    #d$x
    #d$y
    d
}

plot_prediction <- function(price) {
    p.fit = arima(price, order = c(1, 0, 1))
    p.fore = predict(p.fit, n.ahead=5)
    
    U = p.fore$pred + 2*p.fore$se
    L = p.fore$pred - 2*p.fore$se
    minx=min(price,L)
    maxx=max(price,U)
    ts.plot(price,p.fore$pred,col=1:2, ylim=c(minx,maxx))
    lines(U, col="blue", lty="dashed")
    lines(L, col="blue", lty="dashed") 
}

