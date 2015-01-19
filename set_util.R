read_symbol_path <- function(directory, symbol) {
    
    file_name <- paste(directory, symbol, sep = "/")
    file_name
}

read_symbol_data <- function(directory, symbol) {
    symbol_path <- read_symbol_path(directory, symbol)
    print(symbol_path)
    
    # read data for each symbol
    con <- file(symbol_path, "r")
    symbol_data <- read.csv(con, header=F,)
    close.connection(con)
    
    # set column name
    colnames(symbol_data) <- c("date", "open", "high", "low", "close", "volumn")
    symbol_data
}

read_symbol_column <- function(directory, symbol, column = "close") {
    
    symbol_data <- read_symbol_data(directory, symbol)
    symbol_data <- symbol_data[, c("date", column)]
    colnames(symbol_data) <- c("date", symbol)
    symbol_data
}

next_weekday <- function(date) {
    
}

is.Weekday <- function(date) {
    weekday
    
    
}

