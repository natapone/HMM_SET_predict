# USAGE
# source("set_hmm.R")
# m = train_hmm(n_state = 5)
# plot_state("DTAC", m, n_state = 5)
# plot_state("SET", m, n_state = 7)
# data_plot = plot_forecast("SET", m, mode = "demo", n_day_forecast=10)

# m <- load_model()

library(quantmod)
library(RHmm)
library(reshape)
library(ggplot2)
library(scales) # for muted
source("set_util.R")

sim <- function (hm_model, last_state, n_sim, rand_num = 100, forecast_dates=NULL) {
    n_state = hm_model$HMM$distribution$nStates
    last_date = index(last_state)
    
    #Initial probabilities:
    prob = c(rep(0,times=n_state))
    prob[last_state$state] = 1
    #print(prob)
    
    last_hmm = hm_model
    last_hmm$HMM$initProb = prob
    
    
    predict_set = NULL
    
    # create unique combination of states
    state_combo = NULL
    for (r in 1:rand_num) {
        set.seed(r)
        simul <- HMMSim(n_sim+1, last_hmm$HMM)
        state_combo = rbind(state_combo,simul$states)
    }
    
    message("count all states combination: ", nrow(state_combo))
    
    state_combo = unique.data.frame(state_combo)
    message("count unique states combination: ", nrow(state_combo))
    
#     for (r in 1:nrow(ss)) {
#         print(ss[r,])
#     }
    
    # -------------
    for (r in 1:nrow(state_combo)) {
        simul_states = state_combo[r,]
        message("Seed = ", r)
        print(simul_states)
#     for (r in 1:rand_num) {
#         set.seed(r)
#         # simulate
#         simul <- HMMSim(n_sim+1, last_hmm$HMM)
#         print(simul$states)
#         #print(simul$states[-1])
        
        last_price = as.numeric(last_state$close)
        predict_prices = c(last_price)
        #state_from = last_state$state
        state_from = NULL
        
        # from last state to first simulate state
        sim_prob = 1
            
            
        #for (state in simul$states[-1]) { # first element is initial state
        for (state in simul_states[-1]) { # first element is initial state
            future_price = state_to_price(hm_model, state, last_price)
            predict_prices = append(predict_prices, future_price)
            
            #message(state, ": ", " ==> ", last_price, " => ", future_price)
            
            if(is.null(state_from)) {
                state_from = state
                sim_prob = hm_model$HMM$transMat[last_state$state, state]
                #message("1st state from ",last_state$state, " to " ,state, " P = ", sim_prob)
            } else {
                
                sim_prob = sim_prob * hm_model$HMM$transMat[state_from, state]
                #message("state from: ", state_from, " to ", state , " P= " ,hm_model$HMM$transMat[state_from, state], " ------ ", sim_prob)
            }
            
            last_price = future_price
            state_from = state
        }
        
        
        print(predict_prices)
        if(is.null(forecast_dates)) {
            forecast_dates = c( last_date + 0:n_sim)
        }
        
        #if (sim_prob >= (0.1 ^ n_sim)) { # lowest limit prob = 0.01
        if (sim_prob >= 0) { # lowest limit prob = 0.01
            predict_prices = data.frame(
                "date"  = forecast_dates,
                "seed"  = r,
                "price" = predict_prices, 
                "probability"  = sim_prob
            )
            #print (predict_prices)
            
            if (is.null(predict_set)) {
                predict_set = predict_prices
            } else {
                #predict_set = cbind(predict_set, predict_prices)
                predict_set = rbind(predict_set, predict_prices)
            }
        }
        message("---------")
    }
    
    # filter low probability
    limit_prob = median(predict_set$probability)
    message(" - median probability", limit_prob)
    
    message(" - count all probability = ", nrow(predict_set))
    predict_set = predict_set[predict_set$probability > limit_prob, ]
    message(" - count plot probability = ", nrow(predict_set))
    
    #colnames(predict_set) = c(1:rand_num)
    
    
    # add dates
    #predict_set = xts(predict_set, c( last_date + 0:n_sim) );
    
#     predict_set = xts( 
#         data.frame("seed"=predict_set$seed, "price"=predict_set$price, "prob"=predict_set$prob) 
#         , predict_set$date);
    
    print (head(predict_set))
    
    #print(xxx)
    
    predict_set
    
}

state_to_price <- function(hm_model, state, last_price) {
    #message("    ", state, ": ", hm_model$HMM$distribution$mean[[ state ]][1] )
    
    
    #state_mean = hm_model$HMM$distribution$mean[[ state ]][1]
    #state_mean = state_mean * rnorm(mean=1)
    
    price = exp(hm_model$HMM$distribution$mean[[ state ]][1]) * last_price
    price
    #add variation to pricesss
    #rnorm(1,mean=price, sd=price*0.0001)
}

save_model <- function (hm_model, file = "set_100_model.RData") {
    saveRDS(m, file)
}

load_model <- function (file = "set_100_model.RData") {
    m <- readRDS(file)
    m
}

plot_forecast <- function (symbol, hm_model, directory = "data/predict", period = c(0:300), n_day_forecast = 5, mode = "demo") {
    data_test = get_train_set(symbol, directory, period)
    
    #print(tail(data_test, n=10))
    data_actual = NULL
    if (mode == "demo") {
        data_actual = tail(data_test, n=n_day_forecast)
        data_test   = head(data_test, n=( nrow(data_test) - n_day_forecast ))
#         data_test[1:]
        
        message("tail data_test")
        print(tail(data_test))
        message("data_actual")
        print(data_actual)
    }
    
    data_forecast = do_forecast(data_test, hm_model)
    message("tail data forecast")
    print(tail(data_forecast))

    # Simulation
    last_state = tail(data_forecast, n=1)
    last_date = index(last_state)
    
    if (mode == "demo") {
        forecast_dates = c(last_date, data_actual$date)
    } else {
        forecast_dates = c(last_date + 0:n_day_forecast)
    }
#print(forecast_dates)
#print(c(last_date + 0:n_day_forecast))
#return()
    
    sim_set = sim(hm_model, last_state, n_day_forecast, forecast_dates=forecast_dates)
    #print(tail(sim_set))
    #print(data_actual)
    
    data_plot = NULL
    if (mode == "demo") {
        data_plot = sim_set
        
        # balance color and size
        max_prob = max(data_plot$probability)
        dot_size = max_prob / 4
        
        #add actual data
        actual_price_from_test = data.frame(
            "date" = data_test$date,
            "seed" = 0,
            "price" = data_test$close,
            "probability" = -1 * max_prob,
            "dot_size" = dot_size
        )
        actual_price_from_test = tail(actual_price_from_test, n=(n_day_forecast * 2))
        #message("actual_price_from_test")
        #print(actual_price_from_test)
        
        actual_price_from_actual = data.frame(
            "date" = data_actual$date,
            "seed" = 0,
            "price" = data_actual$close,
            "probability" = -1 * max_prob,
            "dot_size" = dot_size
        )
        
        #actual_price_from_test = (actual_price_from_test)
        
        #data_plot = rbind(data_plot, actual_price_from_test)
        #data_plot = rbind(data_plot, actual_price_from_actual)
        actual_price = rbind(actual_price_from_test, actual_price_from_actual)
    }
    
    plot = ggplot(data=data_plot,
                  aes(x=date, y=price, colour=probability, group=seed, size=probability )) +
        #geom_smooth(method = "loess", se=F,n=5) +
        #geom_smooth(method = "loess", se=F, n=10) +
        geom_line(alpha=0.7,stat="smooth",method = "loess", n=n_day_forecast, position = "identity")+
        #geom_line(alpha=0.7,stat="smooth",method = "lm", formula = y ~ poly(x, 3) )+
        #scale_colour_gradient2(low="white", high=muted("orange")) + 
        scale_colour_gradient2(low=muted("green"), high=muted("red"), mid="white", midpoint=0) +
        #scale_colour_gradient2(low=muted("green"), high="orange", mid="white", midpoint=0) +
        #scale_colour_gradientn( colours=rainbow(hm_model$HMM$distribution$nStates)) +
        theme(
            panel.background=element_rect(fill=160), 
            panel.grid.minor = element_line(colour="white", size=0.1),
            panel.grid.major = element_line(colour="white", size=0.3)
        )
    # geom_line(linetype="dotted") +  
    # scale_colour_gradient2(low="white", high=muted("green"), mid="blue", midpoint=0.2) +
    
    #small_dot_size = min(data_plot$probability) * 4
    plot = plot + geom_line(data=actual_price,
                aes(x=date, y=price, colour=probability, group=seed, size=seed )            
            ) + 
            geom_point(data=actual_price,
                       aes(x=date, y=price, colour=probability, group=seed, size=dot_size  )   
            )
    
    plot_title = c(
        symbol, ": ", n_day_forecast, " days forecast @ ",  format(last_date, format="%d %b %Y")   
    )
    plot_title = paste(plot_title, collapse=' ')
    plot = plot + ggtitle(plot_title)
    plot = plot + scale_x_date(labels = date_format("%d %b %Y"))

print(min(data_plot$probability))
    print(plot)


    return(data_plot)
}

do_forecast <- function (data_test, hm_model) {
    # drop close price from test set
    drops <- c("close", "date")
    set_test = data_test[,!(names(data_test) %in% drops)]
    
    # Viterbi Algorithm
    VitPath <- viterbi(hm_model, set_test)
    
    data_forecast = cbind( data_test$close, VitPath$states )
    colnames(data_forecast) = c( "close", "state")
    
    data_forecast = xts(data_forecast, data_test$date );
    
    data_forecast
}

plot_state <- function (symbol, hm_model, directory = "data/predict", period = c(0:300), n_state = 5, n_sim = 5, mode = "test") {
    data_test = get_train_set(symbol, directory, period)
    
    # drop close price from test set
    drops <- c("close", "date")
    set_test = data_test[,!(names(data_test) %in% drops)]
    
    # return(set_test)
    
    # Viterbi Algorithm
    VitPath <- viterbi(hm_model, set_test)
    
    # data_predict(delta, state)
    data_predict = cbind( data_test$close, VitPath$states )
    colnames(data_predict) = c( "close", "state")
    
    #data_predict = cbind( data_test$date, data_test$close, VitPath$states )
    #colnames(data_predict) = c( "date", "close", "state")
    
    #return(data_predict)
    #data_predict = xts(data_predict, as.Date(1:nrow(data_predict)) );
    data_predict = xts(data_predict, data_test$date );
    #print ( tail(data_predict) )
    
    # sim
    # last state
    #last_state = tail(VitPath$states, n=1)
    last_state = tail(data_predict, n=1)
    last_date = index(last_state)
    
    #print( last_state$state )
    #print( last_state$close )
    
    sim_set = sim(hm_model, last_state, n_sim)
    #return(sim_set)
    
    #####
    if (mode == 'dev') {
        data_plot = sim_set
        data_plot = data_plot[ order(-data_plot$prob, data_plot$seed, data_plot$date), ] # sort plot order
        plot = ggplot(data=data_plot,
                      aes(x=date, y=price, colour=prob, group=seed, size=prob )) +
            geom_point() + 
            geom_smooth(method = "loess", se=F) +
            scale_colour_gradient2(low="white", high="blue") + 
            theme(
                panel.background=element_rect(fill=160), 
                panel.grid.minor = element_line(colour="white", size=0.1),
                panel.grid.major = element_line(colour="white", size=0.3)
            )
        # geom_line(linetype="dotted") +  
        # scale_colour_gradient2(low="white", high=muted("green"), mid="blue", midpoint=0.2) +
        
        print(plot)
        return (data_plot)
    }
    #######
    
    # plot last 60 days
    data_plot = tail(data_predict, n=3000)
    
    # create space for predict data
    empty = data.frame(close=rep(NA, times=n_sim), state=rep(NA, times=n_sim))
    empty = xts(empty, c( last_date + 1:n_sim) );
    data_plot = rbind(data_plot, empty)
    #print(empty)
    #return(data_plot)
    
    plot(data_plot[,1], type="l", main = paste("Plot", symbol, "states") )
    #chartSeries(data_predict[,1])
    for (state in 1:n_state) {
        plot_state = data_plot
        plot_state[plot_state[,2] != state, 1] = NA
        points(plot_state[,1],  type="p", col= state, )
        
        #addTA(plot_state[,1],type="p", on=1)
    }
    
    # plot simulation
    for (r in 1:ncol( sim_set )) {
        #print(sim_set[,r])
        #lines(sim_set[,r], lty = "dashed", col="gray")
        #lines(sim_set[,r], lty = "dotted", col="gray")
        lines(sim_set[,r], col="gray", type="b", pch=20)
        #lines(stats::lowess(sim_set[,r]), lty = "dotted", col="gray")
        
    }

}

train_hmm <- function(directory = "data/export_all", period = c(0:3000), n_state = 5) {
    
    
    # set 50
    set_50 <- c('ADVANC','AOT','BANPU','BAY','BBL','BCP','BEC','BGH','BH','BIGC',
                 'BJC','BLA','BTS','CENTEL','CK','CPALL','CPF','CPN','DELTA','DTAC','EGCO',
                 'GLOBAL','GLOW','HMPRO','INTUCH','IRPC','IVL','JAS','KBANK','KKP','KTB','LH',
                 'MAKRO','MINT','PS','PTT','PTTEP','PTTGC','RATCH','ROBINS','SCB','SCC','SCCC',
                 'TCAP','THAI','TMB','TOP','TRUE','TTW','TUF')
    
    # set 100
    set_100 <- c('AAV','ADVANC','AMATA','AOT','AP','BANPU','BAY','BBL','BCH','BCP',
                 'BEC','BECL','BGH','BH','BIGC','BJC','BLA','BLAND','BTS','CENTEL','CK','CPALL',
                 'CPF','CPN','DCC','DELTA','DEMCO','DTAC','EGCO','ESSO','GLOBAL','GLOW','GOLD',
                 'GSTEL','GUNKUL','HEMRAJ','HMPRO','INTUCH','IRPC','ITD','IVL','JAS','KBANK',
                 'KCE','KKP','KTB','KTC','LH','LOXLEY','LPN','MAJOR','MAKRO','MALEE','MBK','MCOT',
                 'MDX','MINT','PF','PS','PTT','PTTEP','PTTGC','QH','RATCH','ROBINS','ROJNA','RS',
                 'SAMART','SAMTEL','SAT','SC','SCB','SCC','SCCC','SF','SIRI','SPALI','SPCG',
                 'SRICHA','SSI','STA','STEC','STPI','TCAP','THAI','THCOM','THRE','TISCO','TMB',
                 'TOP','TPIPL','TRUE','TTA','TTCL','TTW','TUF','TVO','UV','VGI','WHA')
    
    index_set <- c('SET', 'SET50', 'SET100')
    
    #symbols = set_50
    #symbols = c("DTAC")
    #symbols = c("EA")
    symbols = index_set
    
    data_train = data.frame()
    for (symbol in symbols) {
        data = get_train_set(symbol, directory, period)
        #return(data)
        #print (head(data))
        if(nrow(data_train) > 0) {
            data_train = rbind(data_train, data)
        } else {
            data_train = data
        }
    }
    #return(data_train)
    
    # drop close price from trainig set
    drops <- c("close", "date")
    set_train = data_train[,!(names(data_train) %in% drops)]
    
    message ("Train ", length(set_train), " dimensions")
    # Baum-Welch Algorithm
    hm_model <- HMMFit(obs=set_train , nStates = n_state,
                       control=list(verbose=0, init="KMEANS"),
                       asymptCov=F)
    #print(summary(hm_model))
    
    hm_model
}

get_train_set <- function(symbol, directory, period, predict_column = "close" ) {
    # read from file
    p = read_symbol_data(directory, symbol)
    p = p[period, ]
    p = p[complete.cases(p),]
    #return (p)
    
    hlc <- data.frame(High = p$high, Low = p$low, Close = p$close)
    #return(hlc)
    
    #test = data.frame(
    #    Delt(p[,"close"],type='log'),
    #    Delt(p[,"close"],type='arithmetic')
    #)
    #print(head(test))
    #print(head(p[,"close"]))
    #return()
    
    # Create trainig set from Close and Vol
    t = data.frame(
        Delt(p[,"close"],type='log'),
        Delt(p[,"high"],type='log'),
        Delt(p[,"low"],type='log'),
        #Delt(p[,"open"],type='log'),
        #Delt(p$volumn,type='log'),
        
        #RSI(p[,"close"], n=100),
        RSI(p[,"close"], n=7), 
        #RSI(p[,"close"], n=5),
        #MFI(hlc, volume=p$volumn,n=14),
        p[,"close"], 
        #p[,"date"],
        as.Date.character( p[,"date"], "%Y%m%d")
    )
    
    
    #t = data.frame(Delt(p[,predict_column]) , p[,"close"], p[,"date"])
    t = t[complete.cases(t),]
    #colnames(t) = c("delta_close", "delta_volumn", "close", "date")
    colnames(t) = c(
        "del_close", 
        "del_high",
        "del_low",
        #"del_open",
        #"del_vol",
        
        #"rsi-long",
        "rsi", 
        #"rsi-short",
        #"mfi", 
        "close", "date"
    )
    t
}

list_file <- function(directory = "data/export") {
    files = list.files(path=directory)
}

# source("set_hmm.R")
# t = train_hmm()
