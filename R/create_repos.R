#' @title Find Created Repositories of a Github User
#' @description Find (or retrieve) details of all repositories created by a specified Github user
#' @param user character, Github user name
#' @return data.frame
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  created.repos('tidyverse')
#'  }
#' }
#' @export 
#' @importFrom httr content GET
#' @importFrom plyr ldply
created.repos<-function(user){
  list0=httr::content(httr::GET(sprintf('https://api.github.com/search/repositories?q=user:%s',user)))$items
plyr::ldply(list0,.fun=function(y) as.data.frame(y[-which(sapply(y,is.null))],stringsAsFactors = FALSE))
}