#' Function loading all tickers of a specified index
#' @param index Bloomberg index ticker code
#' @export
#' @author Peter Garnry
#' @examples
#' IndexMembers(index = "SPX")

bloomberg_index_members <- function(index = "SPX") {
  
  conn <- blpConnect()
  
  index.count <- bdp(securities = paste(index, "Index"),
                     fields = "COUNT_INDEX_MEMBERS",
                     con = conn)
  
  if(index.count <= 2500) {
    
    index.members <- bds(securities = paste(index, "Index"),
                         fields = "INDX_MEMBERS",
                         con = conn)
    
  }
  
  if(index.count >= 2500 & index.count <= 5000) {
    
    index.members.1 <- bds(securities = paste(index, "Index"),
                         fields = "INDX_MEMBERS",
                         con = conn)
    
    index.members.2 <- bds(securities = paste(index, "Index"),
                           fields = "INDX_MEMBERS2",
                           con = conn)
    
    index.members <- rbind(index.members.1, index.members.2)
    
  }
  
  if(index.count > 5000) {
    
    index.members.1 <- bds(securities = paste(index, "Index"),
                           fields = "INDX_MEMBERS",
                           con = conn)
    
    index.members.2 <- bds(securities = paste(index, "Index"),
                           fields = "INDX_MEMBERS2",
                           con = conn)
    
    index.members.3 <- bds(securities = paste(index, "Index"),
                           fields = "INDX_MEMBERS3",
                           con = conn)
    
    index.members <- rbind(index.members.1, index.members.2, index.members.3)
    
  }
  
  blpDisconnect(conn)
  
  return(index.members[, ncol(index.members)])
  
}