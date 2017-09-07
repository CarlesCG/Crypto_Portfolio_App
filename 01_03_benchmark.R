# This script will scrape all of the market close data each day for all the cryptocurrencies listed on CMC.
# Load dependencies.
library(jsonlite)
library(tidyverse)
library(doSNOW)
library(doParallel)
library(lubridate)
library(stringr)
library(rvest)

## Define parameters.
# range <- 1:50
# start <- 20170101
# end <- 20170905
# cpu <- 4

benchmark <- function(cpu, range, start, end) {
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
      symbol,
      "/historical-data/?start=", 
      start, 
      "&end=", 
      end)
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
   
   # This took me ages to work out how to do nicely, but will combine data frames in parallel.
   # Progress bar
   pb <- txtProgressBar(max = max(range), style = 3)
   progress <- function(n) setTxtProgressBar(pb, n)
   opts <- list(progress = progress)
   
   results <- foreach(i = range, .combine = rbind, .options.snow = opts) %dopar% {
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
   
   # write.csv(marketdata, file)
   print(proc.time() - ptm)
   return(marketdata)
}
# t <- benchmark(cpu = 4, range = 1:5, start = 20170101, end = 20170905)

library(microbenchmark)
timming <- microbenchmark(
   CPU_1 = benchmark(cpu = 1, range = 1:50, start = 20170101, end = 20170905), 
   CPU_2 = benchmark(cpu = 2, range = 1:50, start = 20170101, end = 20170905), 
   CPU_3 = benchmark(cpu = 3, range = 1:50, start = 20170101, end = 20170905), 
   CPU_4 = benchmark(cpu = 4, range = 1:50, start = 20170101, end = 20170905), 
   times = 20 
   )

timming
ggplot2::autoplot(timming)
sessionInfo <- sessionInfo()
save(list = c('timming', 'sessionInfo'),
     file = "./data/benchmark_paralellCPU.RData")