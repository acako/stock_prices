library(httr)
library(rjson)
headers = c(
  `Upgrade-Insecure-Requests` = '1'
)

params = list(
  `datatype` = 'json'
)

res <- httr::GET(url = 'https://financialmodelingprep.com/api/v3/stock/list?apikey=81419012a8f4bf777c342c25e2ddaf77', query = params)

ticker_list <- lapply(content(res),'[[','symbol')
tickers <- data.frame(symbols=unlist(ticker_list))

write.csv(tickers,'tickers.csv')
