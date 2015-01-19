cal_set_corr <- function(directory = "export") {
    
    source("set_util.R")
#     symbols <- c("PTT", "SCB", "DTAC")
    
    # set 50
    symbols <- c('ADVANC','AOT','BANPU','BAY','BBL','BCP','BEC','BGH','BH','BIGC',
      'BJC','BLA','BTS','CENTEL','CK','CPALL','CPF','CPN','DELTA','DTAC','EGCO',
      'GLOBAL','GLOW','HMPRO','INTUCH','IRPC','IVL','JAS','KBANK','KKP','KTB','LH',
      'MAKRO','MINT','PS','PTT','PTTEP','PTTGC','RATCH','ROBINS','SCB','SCC','SCCC',
      'TCAP','THAI','TMB','TOP','TRUE','TTW','TUF')
    
    symbols <- c(symbols, 'SET')
    print(symbols)
    corr_data <- data.frame()
    print( corr_data)
    for (symbol in symbols) {
        print(symbol)
        
        prices <- read_symbol_column(directory, symbol, "close")
        print(head(prices))
        if (nrow(corr_data) == 0) {
            corr_data <- prices
        } else {
            corr_data <- merge(corr_data, prices)
        }
        
        
    }
    
    #print(head(corr_data))
    
    # remove date
    corr_data <- corr_data[-1]
    
    # cal correlation
    corr_price <- cor(corr_data)
    #print(corr_price[,'SET'])
    
    good <- corr_price >= 0.8 | corr_price <= -0.8    
    
    #print(corr_price[good])
    print(good)
    

}
