#' @title Inspect directory structure of version control system repository
#' @description Inspect directory structure of git/bitbuck/svn repository head
#' @param path character, Path to root directory of git/svn repository or a name of a github/bitbucket repository, Default: setwd()
#' @param branch character, alias of the branch in the repository, Default: 'master'
#' @param subdir chracter, subdirectory of repository/branch to query, if '.' then only the root directory will be queried, Default: NULL
#' @param vcs character, choose which version control system to search (github, bitbucket, git (head of checkout repo), svn), Default: 'github'
#' @param full.names boolean, If TRUE, the directory path is prepended to the file names to give a relative file path. 
#' If FALSE, the file names (rather than paths) are returned, Default: TRUE
#' @return character
#' @export
#' @importFrom tools file_path_as_absolute
#' @importFrom httr http_error GET content
#' @examples 
#' #github
#'   ls_remote('tidyverse/glue')
#'   #'named_args' branch
#'     ls_remote('tidyverse/glue',branch='named_args')
#'   #'named_args' branch only 'src' subdirectory
#'     ls_remote('tidyverse/glue',branch='named_args',subdir='src')
#'  #bitbucket
#'    #'master' branch
#'     ls_remote('metrumrg/qapply',vcs='bitbucket')
#' 
ls_remote <- function(path=getwd(),branch='master',subdir=NULL,vcs='github',full.names=FALSE){
  this_wd <- getwd()
  switch(vcs,
         github={
           uri_git <- sprintf('https://github.com/%s.git',path)
           chk_git <- httr::http_error(uri_git)
           if(chk_git) stop(sprintf("repo: %s not found", uri_git))
           tr <- httr::content(httr::GET(sprintf('https://api.github.com/repos/%s/git/trees/%s%s',path,branch,'?recursive=1')))$tree
           s <- sapply(tr,function(x) if(x$mode!='040000') x$path)
           s <- unlist(s)
           if(!is.null(subdir)){
             if(subdir=='.'){
               s=s[!grepl('/',s)]
             }else{
               s=grep(paste0('^',subdir,'/'),s,value=TRUE)   
             }
           }
           if(full.names) s=sprintf('https://raw.githubusercontent.com/%s/%s/%s',path,branch,s) 
           pathout <- s
         },
         bitbucket={
           uri_bit <- sprintf('https://bitbucket.org/!api/1.0/repositories/%s/directory/%s',path,branch)
           chk_bit <- httr::http_error(uri_bit)
           if(chk_bit) stop(sprintf("repo: %s not found", chk_bit))
           s <- unlist(httr::content(httr::GET(uri_bit))$value)[-1]
           if(!is.null(subdir)){
               if(subdir=='.'){
                 s=s[!grepl('/',s)]
               }else{
                 s=grep(paste0('^',subdir,'/'),s,value=TRUE)   
               }
             }
           if(full.names) s=sprintf('https://bitbucket.org/%s/raw/%s/%s',path,branch,s) 
           pathout <- s
         },
         git={
           setwd(tools::file_path_as_absolute(path))
           s <- system('git ls-tree -r HEAD --name-only',intern=TRUE)
           if(!is.null(subdir)){
             if(subdir=='.'){
               s=s[!grepl('/',s)]
             }else{
               s=grep(paste0('^',subdir,'/'),s,value=TRUE)   
             }
           }
           pathout <- s
         },
         svn={
           chk_svn<-TRUE
           uri_svn <- sprintf('svn info %s',path)
           chk_svn <- length(suppressWarnings(x<-system(uri_svn,intern = TRUE)))>0
           if(chk_svn) stop(sprintf("repo: %s not found", uri))
           td <- tempdir()
           system(sprintf('svn co %s --depth empty %s',path,td)) 
           setwd(td)
           pathout<-system("svn ls -R | grep -v '/$'",intern = TRUE)
           unlink(td)
         }
  )
  setwd(this_wd)
  pathout
}