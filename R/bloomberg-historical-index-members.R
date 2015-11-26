#' Get historical index tickers to avoid surviourship bias
#' The function works for one equity index at a time
#' @param index a character string specifying the index ticker
#' @param field specifying the output instrument identification
#' @param startdate a character string specifying the start date
#' @param enddate a character string specifying the end date
#' @param freq a character string specifying time frequency
#' @details The field variable has two options: "ticker" or "isin" 
#' enabling the function to return the desired instrument identification.
#' 
#' The frequency variable is set using the standard Bloomberg inputs such 
#' as "MONTHLY", "DAILY" etc.
#' @export

bloomberg_historical_index_members <- function(index = "SPX",
                                               id = "ticker",
                                               start.date = "2014-01-01",
                                               end.date = "",
                                               freq = "MONTHLY") {
  
  blpConnect()  # connect to Bloomberg
  
  start.date <- as.Date(start.date)
  
  if(end.date != "") end.date <- as.Date(end.date)
  
  field.df <- data.frame(ticker = "COMPOSITE_EXCH_CODE",
                         isin = "ID_ISIN",
                         stringsAsFactors = FALSE)
  
  index.prices <- bloomberg_historical_data(tickers = index,
                                            type = "Index",
                                            fields = "PX_LAST",
                                            freq = freq,
                                            start.date = start.date,
                                            end.date = end.date)
  
  dates <- index(index.prices)
  
  if(freq == "QUARTERLY") dates <- as.Date(index(index.prices), frac = 1)
  if(freq == "MONTHLY") dates   <- as.Date(index(index.prices), frac = 1)
  
  hist.index.tickers.temp <- vector("list", length(dates))
  
  print(paste("Finding historical tickers for", index, "on...", sep = " "))
  
  for(i in 1:length(dates)) {
    
    cat(paste(dates[i]), "\n")
    
    bbg.overrides <- structure(gsub("[[:punct:]]","",dates[i]),
                               names = "END_DATE_OVERRIDE")
    
    bbg.data <- bds(securities = paste(index, "Index"),
                    fields = "INDX_MWEIGHT_HIST",
                    overrides = bbg.overrides)
    
    # this ensures that index members' ticker codes are converted to composite ticker codes or
    # ISIN values replace Bloomberg tickers
    field.data <- bdp(securities = paste(bbg.data[, "Index Member"], "Equity"),
                      fields = field.df[, field])
    
    if(field == "ticker") {
      
      hist.index.tickers.temp[[i]] <- paste(substr(bbg.data$`Index Member`,
                                                   1,
                                                   nchar(bbg.data$`Index Member`) - 2),
                                            field.data[, 1],
                                            sep = "")
      
    }
    
    if(field == "isin") {
      
      hist.index.tickers.temp[[i]] <- field.data[, 1]
      
    }
    
  }
  
  unique.tickers <- unique(unlist(hist.index.tickers.temp))
  
  hist.index.tickers <- xts(matrix(NA,
                                   nrow = length(dates),
                                   ncol = length(unique.tickers),
                                   dimnames = list(NULL, unique.tickers)),
                            order.by = dates)
  
  for(i in 1:length(dates)) {
    
    index.pos <- match(hist.index.tickers.temp[[i]], unique.tickers)
    
    hist.index.tickers[i, index.pos] <- 1
    
  }
  
  return(hist.index.tickers)
  
}