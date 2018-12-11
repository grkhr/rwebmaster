#' Get popular keywords by shows and clicks for last 2 weeks
#'
#' Function to get popular keywords by shows and clicks for last 2 weeks
#' @param token Your API token, see \code{\link{https://tech.yandex.ru/oauth/}}
#' @param host_id Your host, take it from URL page of the project. example "http:www.site.ru:80"
#' @export
#' @importFrom httr POST
#' @importFrom httr GET
#' @importFrom httr stop_for_status
#' @importFrom httr content
#' @importFrom httr add_headers
#' @importFrom rjson toJSON
#' @importFrom rjson fromJSON
#' @examples
#' WebMasterExternal()

WebMasterPopular <- function(token = NULL, host_id = NULL)
{
  proc_start <- Sys.time()
  query <- "https://api.webmaster.yandex.net/v3/user/"
  answer <- GET(url=query, add_headers(Authorization=paste0("OAuth ",token)))
  dataRaw <- content(answer, "parsed", "application/json")
  user_id <- dataRaw$user_id
  
  query <- paste0("https://api.webmaster.yandex.net/v3/user/",user_id,"/hosts/")
  answer <- GET(url=query, add_headers(Authorization=paste0("OAuth ",token)))
  dataRaw <- content(answer, "parsed", "application/json")
  
  query <- paste0("https://api.webmaster.yandex.net/v3/user/",user_id,"/hosts/",host_id,"/search-queries/popular/?order_by=TOTAL_CLICKS&query_indicator=TOTAL_SHOWS&query_indicator=TOTAL_CLICKS&query_indicator=AVG_SHOW_POSITION&query_indicator=AVG_CLICK_POSITION")
  answer <- GET(url=query, add_headers(Authorization=paste0("OAuth ",token)))
  dataRaw <- content(answer, "parsed", "application/json")
  result <- data.frame(stringsAsFactors = F)
  if (length(dataRaw$queries) > 0)
  {
    column_names <- unlist(lapply(c(names(dataRaw$queries[[1]])), 
                                  function(x) return(x)))
    column_names <- c(column_names[1:2],unlist(lapply(c(names(dataRaw$queries[[1]]$indicators)), 
                                                      function(x) return(x))))
    
    rows <- lapply(dataRaw$queries, function(x) return(x))
    for (rows_i in 1:length(rows)) {
      if (is.null(rows[[rows_i]]$indicators$AVG_SHOW_POSITION)) rows[[rows_i]]$indicators$AVG_SHOW_POSITION <- NA
      if (is.null(rows[[rows_i]]$indicators$AVG_CLICK_POSITION)) rows[[rows_i]]$indicators$AVG_CLICK_POSITION <- NA
      result <- rbind(result, unlist(rows[[rows_i]]), stringsAsFactors = F)
    }
    colnames(result) <- column_names
  }
  
  res <- data.frame()
  res <- rbind(res,result)
  
  query <- paste0("https://api.webmaster.yandex.net/v3/user/",user_id,"/hosts/",host_id,"/search-queries/popular/?order_by=TOTAL_SHOWS&query_indicator=TOTAL_SHOWS&query_indicator=TOTAL_CLICKS&query_indicator=AVG_SHOW_POSITION&query_indicator=AVG_CLICK_POSITION")
  answer <- GET(url=query, add_headers(Authorization=paste0("OAuth ",token)))
  dataRaw <- content(answer, "parsed", "application/json")
  result <- data.frame(stringsAsFactors = F)
  if (length(dataRaw$queries) > 0)
  {
    column_names <- unlist(lapply(c(names(dataRaw$queries[[1]])), 
                                  function(x) return(x)))
    column_names <- c(column_names[1:2],unlist(lapply(c(names(dataRaw$queries[[1]]$indicators)), 
                                                      function(x) return(x))))
    
    rows <- lapply(dataRaw$queries, function(x) return(x))
    for (rows_i in 1:length(rows)) {
      if (is.null(rows[[rows_i]]$indicators$AVG_SHOW_POSITION)) rows[[rows_i]]$indicators$AVG_SHOW_POSITION <- 0
      if (is.null(rows[[rows_i]]$indicators$AVG_CLICK_POSITION)) rows[[rows_i]]$indicators$AVG_CLICK_POSITION <- 0
      result <- rbind(result, unlist(rows[[rows_i]]), stringsAsFactors = F)
    }
    colnames(result) <- column_names
  }
  
  result <- rbind(result,res)
  result <- result[!duplicated(result, by = "query_id"),]
  packageStartupMessage("Processed ",length(result$query_id)," rows", appendLF = T)
  total_work_time <- round(difftime(Sys.time(), proc_start , units ="secs"),0)
  packageStartupMessage(paste0("Total time: ",total_work_time, " sec."))
  return(result)
}