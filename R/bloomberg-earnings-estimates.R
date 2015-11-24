#' Function getting current Bloomberg estimates
#' @param tickers character vector specifying the securities
#' @param type a character string specifying instrument type
#' price point extraction
#' @param fields character vector specifying data fields
#' price point extraction (see details)
#' @param currency a character string specifying the currency
#' @details The function finds and implement the company's
#' primary periodicity so the company's estimates are either
#' quarterly, semi-annually or annually depending on the
#' available period.
#' @export

bloomberg_earnings_estimates <- function(tickers = "GS US",
                                         type = "Equity",
                                         fields = "BEST_EPS",
                                         currency = NULL) {
  
  blpConnect()
  
  tickers.type <- paste(tickers, type, sep = " ")
  
  earn.est <- bdp(securities = tickers.type,
                  fields = c("BEST_EEPS_CUR_QTR",
                             "BEST_EEPS_CUR_SEMI",
                             "BEST_EEPS_CUR_YR"))
  
  if(!is.null(currency)) {
    
    override.names <- c("EQY_FUND_CRNCY", "BEST_FPERIOD_OVERRIDE")
    
  } else {
    
    override.names <- "BEST_FPERIOD_OVERRIDE"
    
  }
  
  if(length(tickers) == 1) {
    
    fperiod <- ifelse(length(grep("QTR", colnames(earn.est))) > 0, "1FQ",
                      ifelse(length(grep("SEMI", colnames(earn.est)) > 0), "1FS", "1FY"))
    
    overrides <- structure(c(currency, fperiod),
                           names = override.names)

    adj.data <- bdp(securities = tickers.type,
                    fields = fields,
                    overrides = overrides)
    
    adj.data$PERIOD <- fperiod
    
    rownames(adj.data) <- tickers
    
  } else {
    
    # pre-allocate matrix for storing data
    adj.data <- matrix(NA,
                       nrow = length(tickers),
                       ncol = length(fields),
                       dimnames = list(tickers,
                                       fields))
    
    fperiod <- ifelse(!is.na(earn.est[, 1]), "1FQ",
                      ifelse(!is.na(earn.est[, 2]), "1FS", "1FY"))
    
    for(i in 1:length(tickers)) {
      
      bbg.data <- bdp(securities = tickers.type[i],
                      fields = fields,
                      overrides = structure(c(currency, fperiod[i]),
                                            names = override.names))
      
      adj.data[i, ] <- as.numeric(bbg.data)
      
    }
    
    adj.data <- as.data.frame(adj.data)
    
    adj.data$PERIOD <- fperiod
    
  }

  return(adj.data)
  
}