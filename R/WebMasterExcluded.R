#' Get excluded links
#'
#' Function to excluded links
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
#' WebMasterExcluded()

WebMasterExcluded <- function(token = NULL, host_id = NULL)
{
  proc_start <- Sys.time()
  query <- "https://api.webmaster.yandex.net/v3/user/"
  answer <- GET(url=query, add_headers(Authorization=paste0("OAuth ",token)))
  dataRaw <- content(answer, "parsed", "application/json")
  user_id <- dataRaw$user_id
  
  query <- paste0("https://api.webmaster.yandex.net/v3/user/",user_id,"/hosts/")
  answer <- GET(url=query, add_headers(Authorization=paste0("OAuth ",token)))
  dataRaw <- content(answer, "parsed", "application/json")
  
  query <- paste0("https://api.webmaster.yandex.net/v3/user/",user_id,"/hosts/",host_id,"/summary/")
  answer <- GET(url=query, add_headers(Authorization=paste0("OAuth ",token)))
  dataRaw <- content(answer, "parsed", "application/json")
  result <- data.frame(stringsAsFactors = F)
  if (length(dataRaw[1]) > 0)
  {
    dataRaw$site_problems <- NULL
    column_names <- unlist(lapply(c(names(dataRaw)), 
                                  function(x) return(x)))
    
    rows <- lapply(dataRaw, function(x) return(x))
    for (rows_i in 1:length(rows[1])) {
      result <- rbind(result, unlist(rows), stringsAsFactors = F)
    }
    colnames(result) <- column_names
  }
  result <- cbind(result,host_id)
  total_work_time <- round(difftime(Sys.time(), proc_start , units ="secs"),0)
  packageStartupMessage(paste0("Total time: ",total_work_time, " sec."))
  return(result)
}