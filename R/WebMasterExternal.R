#' Get external links
#'
#' Function to get external links
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

WebMasterExternal <- function(token = NULL, host_id = NULL)
{
  proc_start <- Sys.time()
  query <- "https://api.webmaster.yandex.net/v3/user/"
  answer <- GET(url=query, add_headers(Authorization=paste0("OAuth ",token)))
  dataRaw <- content(answer, "parsed", "application/json")
  user_id <- dataRaw$user_id
  
  query <- paste0("https://api.webmaster.yandex.net/v3/user/",user_id,"/hosts/")
  answer <- GET(url=query, add_headers(Authorization=paste0("OAuth ",token)))
  dataRaw <- content(answer, "parsed", "application/json")
  
  #внешние ссылки
  query <- paste0("https://api.webmaster.yandex.net/v3/user/",user_id,"/hosts/",host_id,"/links/external/samples/")
  answer <- GET(url=query, add_headers(Authorization=paste0("OAuth ",token)))
  dataRaw <- content(answer, "parsed", "application/json")
  offset = 0
  limit = 100
  totalItems <- dataRaw$count
  page <- floor(totalItems/limit)
  result <- data.frame(stringsAsFactors = F)
  for (p in 0:page) 
  {
    offset = p*limit
    query <- paste0("https://api.webmaster.yandex.net/v3/user/",user_id,"/hosts/",host_id,"/links/external/samples/?offset=",offset,"&limit=",limit)
    answer <- GET(url=query, add_headers(Authorization=paste0("OAuth ",token)))
    dataRaw <- content(answer, "parsed", "application/json")
    if (length(dataRaw$links) > 0)
    {
      column_names <- unlist(lapply(c(names(dataRaw$links[[1]])), 
                                    function(x) return(x)))
      
      rows <- lapply(dataRaw$links, function(x) return(x))
      for (rows_i in 1:length(rows)) {
        result <- rbind(result, unlist(rows[[rows_i]]), stringsAsFactors = F)
      }
      colnames(result) <- column_names
    }
    packageStartupMessage("Processed ",length(result$source_url)," rows", appendLF = T)
    
  }
  
  total_work_time <- round(difftime(Sys.time(), proc_start , units ="secs"),0)
  packageStartupMessage(paste0("Total time: ",total_work_time, " sec."))
  return(result)
}