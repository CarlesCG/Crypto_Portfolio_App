library(tidyverse)

## Download list of available coins + info
#
download_coin_metadata <- function(){
   url.download <- "https://www.cryptocompare.com/api/data/coinlist/"
   data.list <- jsonlite::fromJSON(url.download)$Data
   df <- data.table::rbindlist(data.list, fill = TRUE)
   df <- df %>% 
      mutate(Sort = as.integer(SortOrder)) %>% 
      arrange(Sort)
   return(df)
}

## Download historical data for mention coins
#
download_historical_coinList <- function(coin.list=c("BTC", 'ETH'),
                                         base.currency='EUR', 
                                         exceptions=c('YBC')){
   # In case we will like to use different pairs
   # to remove 1:1 and error when downloading
   # Also remove known corrupt pairs
   if(base.currency %in% coin.list){
      bolean.remove <- !grepl(pattern = base.currency, 
                              x = coin.list)
      coin.list <- coin.list[bolean.remove]
   }
   if(exceptions %in% coin.list){
      bolean.remove <- !grepl(pattern = exceptions, 
                              x = coin.list)
      coin.list <- coin.list[bolean.remove]
   }
   
   # Download data from cryptocompare recursive list
   download_coin_data <- function(coin.symbol, 
                                  base.currency){
      url.download <- paste0("https://min-api.cryptocompare.com/data/histoday?fsym=",
                             coin.symbol,
                             "&tsym=",
                             base.currency,
                             # "&toTs=1480302400" , 
                             "&allData=true")
      coin.data <- jsonlite::fromJSON(url.download)$Data
      message( paste0("Cryptocompare... ", coin.symbol) ) 
      coin.data$coin <- coin.symbol
      return(coin.data)
      
   }
   
   data.list <- lapply(coin.list, download_coin_data, base.currency)
   
   # Convert to data.frame and time to posixct
   df <- do.call(rbind, data.list)
   df$time <- as.POSIXct(df$time, origin = "1970-01-01")
   return(df)
}

