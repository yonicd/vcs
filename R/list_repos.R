#' @title Find Created Repositories of a Github User
#' @description Find (or retrieve) details of all repositories created by a specified Github user
#' @param user character, Github user name
#' @param vcs character, choose which version control system to search (github, bitbucket), Default: 'github'
#' @param fields character, fields of repository details, Default='full_name'
#' @details The fields to choose from are detailed in the following link for github
#'  \url{https://developer.github.com/v3/search/#search-repositories}, for bitbucket 
#'  \url{https://developer.atlassian.com/bitbucket/api/2/reference/resource/repositories}
#' @return list
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  list_repos('tidyverse',vcs='github',fields='full_name')
#'  list_repos('metrumrg',vcs='bitbucket',fields='full_name')
#'  }
#' }
#' @export 
#' @importFrom httr content GET
#' @importFrom plyr ldply
list_repos<-function(user, vcs='github',fields='full_name'){
  switch(vcs,
         github={
           list0=httr::content(httr::GET(sprintf('https://api.github.com/search/repositories?q=user:%s',user)))$items
           lout=list0
           nm=unlist(sapply(list0,function(x) x['full_name']))
           names(lout)=nm
           if(!all(fields=='all')) lout=sapply(list0,function(x) x[fields])
           if(!is.null(dim(lout))) lout=as.data.frame(t(lout),stringsAsFactors = FALSE)
           lout
         },
         bitbucket={
           list0=httr::content(httr::GET(sprintf('https://bitbucket.org/!api/2.0/repositories/%s',user)))$value
           lout=list0
           nm=unlist(sapply(list0,function(x) x['full_name']))
           names(lout)=nm
           if(!all(fields=='all')) lout=sapply(list0,function(x) x[fields])
           if(!is.null(dim(lout))) lout=as.data.frame(t(lout),stringsAsFactors = FALSE)
           lout
         })

}