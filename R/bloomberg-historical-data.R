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
                    start.date = as.Date(start.date),
                    end.date = end.date,
                    options = bbg.options,
                    overrides = bbg.overrides)
    
    # nested if statements to create date vector for later xts object creation
    if(is.list(bbg.data)) {
      
      bbg.data <- do.call(rbind, bbg.data)
      
      # reminder! change rownames through regular expression
      
    }
    
    if(freq == "MONTHLY") {
      
      dates <- unique(as.yearmon(as.Date(bbg.data[, "date"])))
      
    } else
      
      if(freq == "QUARTERLY") {
        
        dates <- unique(as.yearqtr(as.Date(bbg.data[, "date"])))
        
      } else {
        
        dates <- as.Date(unique(bbg.data[, "date"]), format = "%Y-%m-%d")
        
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
        
        ticker.pos <- which(tickers.type[i] == bbg.data[, "ticker"])
        
        if(freq == "MONTHLY" & length(ticker.pos) > 0) {
          
          ticker.dates <- as.yearmon(as.Date(bbg.data[ticker.pos, "date"]))
          
        } else
          
          if(freq == "QUARTERLY" & length(ticker.pos) > 0) {
            
            ticker.dates <- as.yearqtr(as.Date(bbg.data[ticker.pos, "date"]))
            
          } else {
            
            ticker.dates <- as.Date(bbg.data[ticker.pos, "date"])
            
          }
        
        dates.match <- match(ticker.dates, dates)
        
        if(length(dates.match) > 0) {
          
          adj.data[dates.match, i] <- as.numeric(bbg.data[ticker.pos, fields])
          
          
        }
        
      }
      
      # special case for monthly data observations across active and
      # inactive tickers. Answer from Bloomberg support:
      # "Our programmers have indicated that at this time
      #  we are unable to support a trading calendar for 
      #  all acquired tickers, so the acquired tickers will 
      #  not respect holidays. We apologize for any inconvenience."
      
      if(type == "Equity") {
        
        active.tickers <- bdp(conn = conn,
                              securities = tickers.type,
                              fields = "TRADE_STATUS")
        
        if(freq == "MONTHLY" & any(active.tickers[, "TRADE_STATUS"] == FALSE) |
           freq == "QUARTERLY" & any(active.tickers[, "TRADE_STATUS"] == FALSE)) {
          
          inactive <- which(active.tickers[, "TRADE_STATUS"] == FALSE)
          
          adj.data[, inactive] <- na.locf(adj.data[, inactive],
                                          maxgap = 2,
                                          fromLast = TRUE)
          
          # find first active ticker
          actv.pos <- which(active.tickers[, "TRADE_STATUS"] == TRUE)[1]
          
          ticker.pos <- which(bbg.data[, "ticker"] == tickers.type[actv.pos])
          
          if(freq == "MONTHLY") {
            
            actv.dates <- as.yearmon(as.Date(bbg.data[ticker.pos, "date"]))
            
          } else
            
            if(freq == "QUARTERLY") {
              
              dates <- as.yearqtr(as.Date(bbg.data[ticker.pos, "date"]))
              
            } else {
              
              dates <- as.Date(bbg.data[ticker.pos, "date"], format = "%Y-%m-%d")
              
            }
          
          true.dates <- match(actv.dates, dates)
          
          adj.data <- adj.data[true.dates,, ]
          
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
        
        ticker.pos <- which(tickers.type[i] == bbg.data[, "ticker"])
        
        if(freq == "MONTHLY" & length(ticker.pos) > 0) {
          
          ticker.dates <- as.yearmon(as.Date(bbg.data[ticker.pos, "date"]))
          
        } else
          
          if(freq == "QUARTERLY" & length(ticker.pos) > 0) {
            
            ticker.dates <- as.yearqtr(as.Date(bbg.data[ticker.pos, "date"]))
            
          } else {
            
            ticker.dates <- as.Date(bbg.data[ticker.pos, "date"])
            
          }
        
        dates.match <- match(ticker.dates, dates)
        
        if(length(dates.match) > 0) {
          
          temp.bbg <- bbg.data[ticker.pos, -1:-2]
          
          adj.data[dates.match, i, ] <- data.matrix(temp.bbg)
          
          
        }
        
      }
      
      # special case for monthly data observations across active and
      # inactive tickers. Answer from Bloomberg support:
      # "Our programmers have indicated that at this time
      #  we are unable to support a trading calendar for 
      #  all acquired tickers, so the acquired tickers will 
      #  not respect holidays. We apologize for any inconvenience."
      
      if(type == "Equity") {
        
        active.tickers <- bdp(conn = conn,
                              securities = tickers.type,
                              fields = "TRADE_STATUS")
        
        if(freq == "MONTHLY" & any(active.tickers[, "TRADE_STATUS"] == FALSE) |
           freq == "QUARTERLY" & any(active.tickers[, "TRADE_STATUS"] == FALSE)) {
          
          inactive <- which(active.tickers[, "TRADE_STATUS"] == FALSE)
          
          for(i in 1:length(fields)) {
            
            temp.data <- xts(adj.data[, inactive, i], order.by = dates)
            
            adj.data[, inactive, i] <- na.locf(temp.data,
                                               maxgap = 2,
                                               fromLast = TRUE)
            
          }
          
          # find first active ticker
          actv.pos <- which(active.tickers[, "TRADE_STATUS"] == TRUE)[1]
          
          ticker.pos <- which(bbg.data[, "ticker"] == tickers.type[actv.pos])
          
          if(freq == "MONTHLY") {
            
            actv.dates <- as.yearmon(as.Date(bbg.data[ticker.pos, "date"]))
            
          } else
            
            if(freq == "QUARTERLY") {
              
              dates <- as.yearqtr(as.Date(bbg.data[ticker.pos, "date"]))
              
            } else {
              
              dates <- as.Date(bbg.data[ticker.pos, "date"], format = "%Y-%m-%d")
              
            }
          
          true.dates <- match(actv.dates, dates)
          
          adj.data <- adj.data[true.dates,, ]
          
        }
        
      }
      
    }
    
  }
  
  if(calendar.type == "FISCAL") {
    
    adj.data <- vector("list", length(tickers))
    
    names(adj.data) <- tickers
    
    primary.period <- bdp(conn = conn,
                          securities = tickers.type,
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
      
      option.values[period.sel.pos] <- primary.period[i]
      
      bbg.data <- bdh(conn = conn,
                      securities = tickers.type[i],
                      fields = fields,
                      start_date = startdate,
                      end_date = enddate,
                      option_names = option.names,
                      option_values = option.values,
                      override_fields = override.fields,
                      override_values = override.values)
      
      if(nrow(bbg.data) > 0 & sum(duplicated(bbg.data[, 1])) == 0) {
        
        dates <- as.Date(unique(bbg.data[, 1]), format = "%Y-%m-%d")
        
        stopifnot(class(dates) == "Date")
        
        col.nas <- apply(is.na(bbg.data), 2, sum)
        
        col.nas.number <- which(col.nas == nrow(bbg.data))
        
        if(length(col.nas.number) > 0) {
          
          bbg.data[, col.nas.number] <- NA
          
        }
        
        adj.data[[i]] <- xts(as.matrix(bbg.data[, 2:ncol(bbg.data)],
                                       nrow = length(dates),
                                       ncol = ncol(bbg.data) - 1),
                             order.by = dates,
                             dimnames = list(NULL, fields))
        
      } else {
        
        adj.data[[i]] <- NA
        
      } 
      
    }
    
  }
  
  blpDisconnect(conn)
  
  return(adj.data)
  
}