#' Function getting historical Bloomberg data
#' @param tickers character vector specifying the securities
#' @param type a character string specifying instrument type
#'             price point extraction
#' @param fields character vector specifying data fields
#'               price point extraction (see details)
#' @param freq a character string specifying time frequency
#' @param currency a character string specifying the currency
#' @details Historical data on monthly and quarterly frequency
#' are exported with Index Class 'yearmon' and 'yearqtr'
#' respectively as tickers across various markets do have the
#' same month end dates. Using this methodology ensure that
#' data is aligned properly.
#' @export

bloomberg_historical_data <- function(tickers = "GS US",
                                      type = "Equity",
                                      fields = "PX_LAST",
                                      freq = "DAILY",
                                      start.date = "2014-01-01",
                                      end.date = "",
                                      currency = NULL,
                                      non.trading.days = "NON_TRADING_WEEKDAYS",
                                      non.trading.days.fill = "NIL_VALUE",
                                      calendar.type = "CALENDAR",
                                      pricing = "PRICING_OPTION_PRICE",
                                      follow.dpdf = TRUE,
                                      override.fields = NULL,
                                      override.values = NULL) {
  
  blpConnect()  # connect to Bloomberg
  
  # concatenate tickers and security type                
  tickers.type <- paste(tickers, type, sep = " ")
  
  option.names <- c("periodicitySelection",
                    "nonTradingDayFillOption",
                    "nonTradingDayFillMethod",
                    "periodicityAdjustment",
                    "adjustmentFollowDPDF",
                    "pricingOption")
  
  option.values <- c(freq,
                     non.trading.days,
                     non.trading.days.fill,
                     calendar.type,
                     follow.dpdf,
                     pricing,
                     currency)
  
  start.date <- as.Date(start.date)
  
  if(end.date != "") end.date <- as.Date(end.date)
  
  if(is.null(override.values) & is.null(override.fields)) {
    
    bbg.overrides <- NULL
    
  } else {
    
    bbg.overrides <- structure(override.values,
                               names = override.values)
    
  }
  
  if(!is.null(currency)) {
    
    bbg.options <- structure(option.values,
                              names = c(option.names, "currency"))
    
  } else {
    
    bbg.options <- structure(option.values,
                         names = option.names)
    
  }
  
  # data handling if calendar.type variable is set to "CALENDAR" or "ACTUAL"
  if(calendar.type == "CALENDAR" | calendar.type == "ACTUAL") {
    
    bbg.data <- bdh(securities = tickers.type,
                    fields = fields,
                    start.date = start.date,
                    end.date = end.date,
                    options = bbg.options,
                    overrides = bbg.overrides)
    
    # nested if statements to create date vector for later xts object creation
    if(length(tickers) > 1) {
      
      temp.data <- as.data.frame(rbindlist(bbg.data))
      
      bbg.dates <- temp.data$date
      
    } else {
      
      bbg.dates <- bbg.data$date
      
    }
    
    if(freq == "MONTHLY") {
      
      dates <- unique(as.yearmon(bbg.dates))
      
    } else
      
      if(freq == "QUARTERLY") {
        
        dates <- unique(as.yearqtr(bbg.dates))
        
      } else {
        
        dates <- unique(bbg.dates)
        
      }
    
    dates <- sort(dates)
    
    # adjust Bloomberg output data for one ticker and one field
    if(length(fields) == 1 & length(tickers) == 1) {
      
      # create xts object based on Bloomberg data and date vector
      adj.data <- xts(as.matrix(bbg.data[, -which(names(bbg.data) == "date")],
                                nrow = length(dates),
                                ncol = length(fields)),
                      order.by = dates)
      
      colnames(adj.data) <- fields
      
    }
    
    # adjust Bloomberg output data for one ticker and multiple fields
    if(length(fields) > 1 & length(tickers) == 1) {
      
      adj.data <- xts(as.matrix(bbg.data[, -which(names(bbg.data) == "date")],
                                nrow = length(dates),
                                ncol = length(fields)),
                      order.by = dates)
      
    }
    
    # adjust Bloomberg output data for multiple tickers and one field
    if(length(fields) == 1 & length(tickers) > 1) {
      
      adj.data <- xts(matrix(NA,
                             nrow = length(dates),
                             ncol = length(tickers),
                             dimnames = list(NULL, tickers)),
                      order.by = dates)
      
      for(i in 1:length(tickers)) {
        
        nobs <- nrow(bbg.data[[i]])
        
        if(freq == "MONTHLY" & nobs > 0) {
          
          ticker.dates <- as.yearmon(bbg.data[[i]]$date)
          
        } else
          
          if(freq == "QUARTERLY" & nobs > 0) {
            
            ticker.dates <- as.yearqtr(bbg.data[[i]]$date)
            
          } else {
            
            ticker.dates <- bbg.data[[i]]$date
            
          }
        
        dates.match <- match(ticker.dates, dates)
        
        if(length(dates.match) > 0) {
          
          adj.data[dates.match, i] <- as.numeric(bbg.data[[i]][, fields])
          
          
        }
        
      }
      
    }
    
    if(length(fields) > 1 & length(tickers) > 1) {
      
      adj.data <- array(NA,
                        dim = c(length(dates),
                                length(tickers),
                                length(fields)),
                        dimnames = list(as.character(dates),
                                        tickers,
                                        fields))
      
      for(i in 1:length(tickers)) {
        
        nobs <- nrow(bbg.data[[i]])
        
        if(freq == "MONTHLY" & length(ticker.pos) > 0) {
          
          ticker.dates <- as.yearmon(bbg.data[[i]]$date)
          
        } else
          
          if(freq == "QUARTERLY" & length(ticker.pos) > 0) {
            
            ticker.dates <- as.yearqtr(bbg.data[[i]]$date)
            
          } else {
            
            ticker.dates <- bbg.data[[i]]$date
            
          }
        
        dates.match <- match(ticker.dates, dates)
        
        if(length(dates.match) > 0) {
          
          adj.data[dates.match, i, ] <- as.matrix(bbg.data[[i]][, fields])
          
        }
        
      }
      
    }
    
  }
  
  # data handling if calendar.type variable is set to "FISCAL"
  if(calendar.type == "FISCAL") {
    
    adj.data <- vector("list", length(tickers))
    
    names(adj.data) <- tickers
    
    primary.period <- bdp(securities = tickers.type,
                          fields = "PRIMARY_PERIODICITY")
    
    primary.period <- ifelse(str_detect(primary.period[, 1],
                                        "\\bQuarterly\\b"),
                             "QUARTERLY",
                             ifelse(str_detect(primary.period[, 1],
                                               "\\bSemi-Annual\\b"),
                                    "SEMI_ANNUALLY",
                                    "YEARLY"))
    
    for(i in 1:length(tickers)) {
      
      period.sel.pos <- which(option.names == "periodicitySelection")
      
      bbg.options[period.sel.pos] <- primary.period[i]
      
      bbg.data <- bdh(securities = tickers.type[i],
                      fields = fields,
                      start.date = start.date,
                      end.date = end.date,
                      options = bbg.options,
                      overrides = bbg.overrides)
      
      if(nrow(bbg.data) > 0 & sum(duplicated(bbg.data$date)) == 0) {
        
        dates <- unique(bbg.data$date)
        
        col.nas <- apply(is.na(bbg.data), 2, sum)
        
        col.nas.number <- which(col.nas == nrow(bbg.data))
        
        if(length(col.nas.number) > 0) {
          
          bbg.data[, col.nas.number] <- NA
          
        }
        
        adj.data[[i]] <- xts(as.matrix(bbg.data[, fields],
                                       nrow = length(dates),
                                       ncol = ncol(bbg.data) - 1),
                             order.by = dates,
                             dimnames = list(NULL, fields))
        
      } else {
        
        adj.data[[i]] <- NA
        
      } 
      
    }  # end of tickers loop
    
  }
  
  return(adj.data)
  
}