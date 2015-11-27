#' Function getting a single data point
#' @param tickers A character vector specifying the securities
#' @param type A character string specifying instrument type (optional)
#' @param fields A character vector specifying data fields
#' @param override.names character vector specifying override fields
#' @param override.values character vector specifying override values
#' @export
#' @author Peter Garnry

bloomberg_data_point <- function(tickers = "ADS GY",
                                 type = "Equity",
                                 fields = "PX_LAST",
                                 option.fields = NULL,
                                 option.values = NULL,
                                 override.fields = NULL,
                                 override.values = NULL) {
  
  blpConnect()  # connect to Bloomberg
  
  tickers.type <- paste(tickers, type, sep = " ")
  
  if(!is.null(option.fields) & !is.null(option.values)) {
    
    bbg.options <- structure(option.values,
                             names = option.fields)
    
  } else {
    
    bbg.options <- NULL
    
  }
  
  if(!is.null(override.fields) & !is.null(override.values)) {
    
    bbg.overrides <- structure(override.values,
                               names = override.fields)
    
  } else {
    
    bbg.overrides <- NULL
    
  }
  
  bbg.data <- bdp(securities = tickers.type,
                  fields = fields,
                  options = bbg.options,
                  overrides = bbg.overrides)
  
  return(bbg.data)
  
}