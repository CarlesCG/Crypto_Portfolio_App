library(jsonlite)
library(tidyverse)
library(doSNOW)
library(doParallel)
library(lubridate)
library(stringr)
library(rvest)

###################################################
# Download cryptodata based on top n range
###################################################
download_crypto_data_byRange <- function(cpu=1, range, 
                                         start, end) {
   ptm <- proc.time()
   
   # Retrieve listing of top {RANGE} of coins and get slugs to be used for searching.
   retrival_coin_slug <- function(range) {
      url <- "https://files.coinmarketcap.com/generated/search/quick_search.json"
      coins <- jsonlite::read_json(url, simplifyVector = TRUE) %>% 
         head(arrange(coins,rank), n = max(range))
      symbol <- coins$slug
      return(symbol)
   }
   symbol <- retrival_coin_slug(range)
   url <- paste0(
      "https://coinmarketcap.com/currencies/",
      symbol, "/historical-data/?start=", 
      start, "&end=", 
      end )
   attr <- c(url)
   
   # Start parallel processing!!!
   cluster <- makeCluster(cpu, type = "SOCK")
   registerDoSNOW(cluster)
   
   # Start scraping function to extract historical results table.
   abstracts <- function(attr) {
      library(rvest)
      page <- read_html(attr)
      # Get coin name.
      names <- page %>% 
         html_nodes(".col-sm-4 .text-large") %>% 
         html_text(trim = TRUE) %>%
         replace(!nzchar(.), NA)
      # Get historical data.
      nodes <- page %>% 
         html_nodes("table") %>% 
         .[1] %>% 
         html_table(fill = TRUE)
      
      # Combine the two and normalise names.
      abstracts <- Reduce(rbind, nodes)
      abstracts$coinname <- names
      names(abstracts) <- c(
         "date", "open", "high", "low",
         "close", "volume", "market", "coin")
      
      return(abstracts)
   }
   
   # Progress bar
   pb <- txtProgressBar(max = max(range), style = 3)
   progress <- function(n) setTxtProgressBar(pb, n)
   opts <- list(progress = progress)
   
   # This took me ages to work out how to do nicely, but will combine data frames in parallel.
   results <- foreach(i = range, .combine = rbind, 
                      .options.snow = opts) %dopar% {
                         abstracts(attr[i])
                      }
   close(pb)
   
   # Clean up 
   marketdata <- results %>% 
      drop_na() %>% 
      mutate(
         volume = as.numeric(gsub(",|-", "", volume)), 
         market = as.numeric(gsub(",|-", "", market)), 
         date = mdy(date), 
         open = as.numeric(open) %>% round(., 2), 
         close = as.numeric(close) %>% round(., 2), 
         high = as.numeric(high) %>% round(., 2),  
         low = as.numeric(low) %>% round(., 2),  
         coin = as.factor(coin), 
         name = stringr::str_extract_all(coin, "^[^\\(]+") %>% str_trim(), 
         symbol = stringr::str_extract_all(coin, "\\(([^()]+)\\)") %>% 
            gsub("\\(([^()]+)\\)", "\\1", x =. ) %>% 
            as.factor()
      ) 
   
   ## Stats on the data
   rate <- fromJSON("https://api.fixer.io/latest?base=USD")
   eur <- rate$rates$EUR
   
   marketdata <- marketdata %>% 
      mutate(
         eur_open = (open * eur) %>% round(., 2), 
         eur_close = (close * eur) %>% round(., 2), 
         variance = ((eur_close - eur_open) / eur_close)
      )
   
   # Stop the amazing parallel processing power
   stopCluster(cluster)
   print(proc.time() - ptm)
   
   return(marketdata)
}
#  
# t <- download_crypto_data_byRange(cpu = 3, range = 1:5, start = 20170901, end = 20170905)

###################################################
# Download the names of the 300 criptocoins
###################################################
retrival_coin_slug <- function(range=1:200) {
   url <- "https://files.coinmarketcap.com/generated/search/quick_search.json"
   coins <- jsonlite::read_json(url, simplifyVector = TRUE) %>% 
      head(arrange(coins,rank), n = max(range))
   symbol <- coins$slug
   return(symbol)
}
# 
# t <- retrival_coin_slug(range)

retrival_coin_info <- function(range=1:200) {
   url <- "https://files.coinmarketcap.com/generated/search/quick_search.json"
   coins <- jsonlite::read_json(url, simplifyVector = TRUE) %>% 
      head(arrange(coins,rank), n = max(range))
   # name <- coins$name
   return(coins)
}


###################################################
# Download cryptodata based on names
###################################################
download_crypto_data_byName <- function(cpu=1, names, 
                                        start, end, 
                                        query.EURexchange=FALSE) {
   
   # Variable symbol need to be from the 
   # foo retrival_coin_slug
   ptm <- proc.time()
   
   url <- paste0(
      "https://coinmarketcap.com/currencies/",
      names, "/historical-data/?start=", 
      start, "&end=", 
      end )
   attr <- c(url)
   
   # Start parallel processing!!!
   cluster <- makeCluster(cpu, type = "SOCK")
   registerDoSNOW(cluster)
   
   # Start scraping function to extract historical results table.
   abstracts <- function(attr) {
      library(rvest)
      page <- read_html(attr)
      # Get coin name.
      names <- page %>% 
         html_nodes(".col-sm-4 .text-large") %>% 
         html_text(trim = TRUE) %>%
         replace(!nzchar(.), NA)
      # Get historical data.
      nodes <- page %>% 
         html_nodes("table") %>% 
         .[1] %>% 
         html_table(fill = TRUE)
      
      # Combine the two and normalise names.
      abstracts <- Reduce(rbind, nodes)
      abstracts$coinname <- names
      names(abstracts) <- c(
         "date", "open", "high", "low",
         "close", "volume", "market", "coin")
      
      return(abstracts)
   }
   
   # Progress bar
   pb <- txtProgressBar(max = length(names), style = 3)
   progress <- function(n) setTxtProgressBar(pb, n)
   opts <- list(progress = progress)
   
   
   # This took me ages to work out how to do nicely, but will combine data frames in parallel.
   results <- foreach(i = 1:length(names), .combine = rbind, 
                      .options.snow = opts) %dopar% {
                         abstracts(attr[i])
                      }
   close(pb)
   
   message("End downloading...")
   message("Mungging Start...")
   # Clean up 
   marketdata <- results %>% 
      drop_na() %>% 
      mutate(
         volume = as.numeric(gsub(",|-", "", volume)), 
         market = as.numeric(gsub(",|-", "", market)), 
         date = mdy(date), 
         open = as.numeric(open) %>% round(., 2), 
         close = as.numeric(close) %>% round(., 2), 
         high = as.numeric(high) %>% round(., 2),  
         low = as.numeric(low) %>% round(., 2),  
         coin = as.factor(coin), 
         name = stringr::str_extract_all(coin, "^[^\\(]+") %>% str_trim(), 
         symbol = stringr::str_extract_all(coin, "\\(([^()]+)\\)") %>% 
            gsub("\\(([^()]+)\\)", "\\1", x =. ) %>% 
            as.factor()
      ) 
   message("Mungging done!")
   ## Stats on the data
   if(query.EURexchange){ 
      message("Query EUR exchange...")
      rate <- fromJSON("https://api.fixer.io/latest?base=USD")
      eur <- rate$rates$EUR
      message("Query EUR done!")
   }else{ eur <- 0.83535 }  
   marketdata <- marketdata %>%
      mutate(
         eur_open = (open * eur) %>% round(., 2),
         eur_close = (close * eur) %>% round(., 2),
         variance = ((eur_close - eur_open) / eur_close)
      )
   
   # Stop the amazing parallel processing power
   stopCluster(cluster)
   message("Cluster Stop")
   print(proc.time() - ptm)
   
   return(marketdata)
}

# t <- download_crypto_data_byName(
#    cpu = 3, start = 20170901, end = 20170905,
#    names= c( "bitcoin", "ethereum",
#              "bitcoin-cash",  "litecoin", "ripple") )
