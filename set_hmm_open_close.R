#require(PerformanceAnalytics)

# USAGE
# source("set_hmm_open_close.R")
# m = train_hmm(n_state = 5)
# plot_state("SET50", m)

# Save
# save_model(m)

# Load
# m = load_model()

library(quantmod)
library(RHmm)
library(reshape)
library(ggplot2)
source("set_util.R")

model_file_name = "set_model_open_close.RData"
market_open = "10:00:00"
market_close = "16:00:00"

index_set <- c('SET', 'SET50', 'SET100')

get_price_state_train_set <- function (hm_model, symbols = index_set, directory = "data/export_all", period = 10) {
    
}

plot_state <- function (symbol, hm_model, directory = "data/predict", period = 60) {
    data_test = get_open_close_train_set(symbol, directory, period, with_price=T) #, time_til_next_trade
    
    # drop close price from test set
    drops <- c("price")
    model_test = data_test[,!(names(data_test) %in% drops)]
    
    #print(tail(model_test))
    
    # Viterbi Algorithm
    VitPath <- viterbi(hm_model, model_test)
    
    # sort states by mean
    sorted =sort_state_by_mean(hm_model)
    
    #state = VitPath$states
    #data_test = cbind(data_test, "state" = VitPath$states)
    #print(data_test)
    
    data_plot = data.frame (
        "date" = index(data_test),
        "price" = data_test$price,
        "state" = sorted[VitPath$states]
    )
    print(tail(data_plot))
    
    plot = ggplot(data=data_plot,
         aes(x=date, y=price, colour=state)) +
        geom_line() + geom_point() + 
        scale_colour_gradientn( colours=rainbow(hm_model$HMM$distribution$nStates)) +
        theme(
            panel.background=element_rect(fill="black"), 
            panel.grid.minor = element_line(colour=160, size=0.1),
            panel.grid.major = element_line(colour=160, size=0.3)
        )
        #scale_colour_gradient2(
        #    low="#FF0000", mid="white", high="green", 
        #    midpoint=median(iris$Sepal.Length)) + 
        #theme(panel.grid=element_blank(), panel.background=element_rect(fill="black"))
    
    print(plot)
    
    
    
    
    #last_state = tail(data_test, n=1)
    #last_date = index(last_state)
    #VitPath
    
    #data_test
}

train_hmm <- function(directory = "data/export_all", period = 3000, n_state = 5) {
    
    symbols = index_set
    
    data_train = data.frame()
    for (symbol in symbols) {
        data = get_open_close_train_set(symbol, directory, period)
        #return(data)
        #print (head(data))
        if(nrow(data_train) > 0) {
            data_train = rbind(data_train, data)
        } else {
            data_train = data
        }
    }
    #return(data_train)
    
    message ("Train ", ncol(data_train), " dimensions")
    # Baum-Welch Algorithm
    hm_model <- HMMFit(obs=data_train , nStates = n_state,
                       control=list(verbose=0, init="KMEANS"),
                       asymptCov=F)
    
#     hm_model <- HMMFit(obs=data_train , nStates = n_state,
#                        control=list(verbose=0, init="KMEANS"),
#                        asymptCov=F)
    
    #print(summary(hm_model))
    
    hm_model
}

get_open_close_train_set <- function(symbol, directory = "data/export_all", period, with_price=F, time_til_next_trade=NULL) {
    # read from file
    p = read_symbol_data(directory, symbol)
    data_count = nrow(p)
    index_start = 0
    if(data_count > period) {
        index_start = data_count - period + 1
    }
    
    p = p[complete.cases(p),]
    p = p[index_start:data_count, ]
    
    # Open
    p_open = data.frame(
        date = strptime( paste(p[,"date"], market_open, sep=" "), "%Y%m%d %H:%M:%S" ),
        type = 0,
        price = p[,"open"]
    )
    p_open = xts(p_open[, c("price", "type")], p_open$date )
    
    # Close
    p_close = data.frame(
        date = strptime( paste(p[,"date"], market_close, sep=" "), "%Y%m%d %H:%M:%S" ),
        type = 1,
        price = p[,"close"]
    )
    p_close = xts(p_close[, c("price", "type")], p_close$date )
     
    #print(p_open)
    #print(p_close)
    
    p = rbind(p_open, p_close)
    p_raw = as.numeric(p$price)
    rsi = RSI(p_raw, n=7)
    
    # cal time until next trade
    #time_til_next_trade = diff(as.numeric(index(p)))
    time_til_next_trade = Delt(as.numeric(index(p)),type='log')
    time_til_next_trade = time_til_next_trade[!is.na(time_til_next_trade)]
        
    # End of test data, next trade is unknown
    time_til_next_trade = append(time_til_next_trade, NA)
    
    # Estimate time until next trade
    last_set = tail(p, n=1)
    last_date = index(last_set)
    message("Estimate time until next trade", last_date)
    
#     tt = estimate_time_til_next_trade()
    
    # search next weekday
    #weekdays.Date(index(m))
    
    # add indicator here
    p_ret = data.frame(
        "return" = Delt(p_raw,type='log'),
        "rsi" = RSI(p_raw, n=7), 
        "del_rsi" = Delt(rsi,type='log'),
        "time_til_next_trade"  = time_til_next_trade
    )
    
    colnames(p_ret) = c(
        "return", 
        "rsi",
        "del_rsi",
        "time_til_next_trade"
    )
    
    if (with_price) {
        price = p_raw
        p_ret = cbind(p_ret, price)
    }
    
    p_ret = xts(p_ret, index(p))
#     p_ret = p_ret[complete.cases(p_ret),]
    
    #print(head(p_ret))
    return (p_ret)
    
}

# make sure states are sorted by mean of price
sort_state_by_mean <- function (hm_model) {
    #sorted_means = numeric(0)
    sorted_means = matrix(ncol=3, nrow=hm_model$HMM$distribution$nStates)
    for (s in 1:hm_model$HMM$distribution$nStates) {
        mean = hm_model$HMM$distribution$mean[[s]][1] # mean of price
        #message (s, " ",mean)
        sorted_means[s, 1] = mean
        sorted_means[s, 2] = s
    }
    sorted_means = sorted_means[ order(sorted_means[,1],decreasing = F  ), ]
    sorted_means[,3] = c(1:hm_model$HMM$distribution$nStates)
    
    results = numeric(hm_model$HMM$distribution$nStates)
    for(i in 1:hm_model$HMM$distribution$nStates) {
        state = sorted_means[i,2]
        rank  = sorted_means[i,3]
        #message(state , " ", rank)
        
        results[state] = rank
    }
    
    return(results)
    
}

save_model <- function (hm_model, file = model_file_name) {
    saveRDS(m, file)
}

load_model <- function (file = model_file_name) {
    readRDS(file)
}
