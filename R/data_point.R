#' Function getting a single data point
#' @param tickers           character vector specifying the securities
#' @param type              a character string specifying instrument type
#'                          price point extraction
#' @param fields            character vector specifying data fields
#'                          price point extraction (see details)
#' @param override_fields   character vector specifying override fields
#' @param override_values   character vector specifying override values
#' @autor Peter Garnry

DataPoint <- function(tickers = "ADS GY", type = "Equity", fields = "PX_LAST",
                      option.names = NULL, option.values = NULL,
                      override.names = NULL, override.values = NULL) {
  
  conn <- blpConnect()
  
  if(!is.null(type)) {
    
    tickers <- paste(tickers, type, sep = " ")
    
  }
  
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
                  overrides = overrides,
                  con = conn)
  
  blpDisconnect(conn)
  
  return(bbg.data)
  
}