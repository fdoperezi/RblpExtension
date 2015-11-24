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
                                 option.names = NULL,
                                 option.values = NULL,
                                 override.names = NULL,
                                 override.values = NULL) {
  
  blpConnect()  # connect to Bloomberg
  
  if(is.null(type)) stop("Security type is missing")
    
  tickers <- paste(tickers, type, sep = " ")
  
  if(!is.null(option.names) & !is.null(option.values)) {
    
    options <- structure(option.values,
                         names = option.names)
    
  } else {
    
    options <- NULL
    
  }
  
  if(!is.null(override.names) & !is.null(override.values)) {
    
    overrides <- structure(override.values,
                           names = override.names)
    
  } else {
    
    overrides <- NULL
    
  }
  
  bbg.data <- bdp(securities = tickers,
                  fields = fields,
                  options = options,
                  overrides = overrides)
  
  return(bbg.data)
  
}