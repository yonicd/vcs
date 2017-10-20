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
ls_remote <- function(path=getwd(),branch='master',subdir=NULL,vcs='github', full.names=FALSE){
  this_wd <- getwd()
  switch(vcs,
         github={
           
           myPAT <- Sys.getenv('GITHUB_PAT')
           
           uri_git <- sprintf('https://api.github.com/repos/%s',path)
           get_git <- sprintf('https://api.github.com/repos/%s/git/trees/%s%s',path,branch,'?recursive=1')
           
           if(nzchar(myPAT)){
             uri_git <- sprintf('%s?access_token=%s',uri_git,Sys.getenv('GITHUB_PAT'))
             get_git <- sprintf('%s&access_token=%s',get_git,Sys.getenv('GITHUB_PAT'))
             }
           
           chk_git <- httr::http_error(uri_git)
           
           if(chk_git) stop(sprintf("repo: https://github.com/%s not found", path))

           tr <- httr::content(httr::GET(get_git))$tree
           s <- sapply(tr,function(x) if(x$mode!='040000') x$path)
           s <- unlist(s)
           if(!is.null(subdir)){
             if(subdir=='.'){
               s=s[!grepl('/',s)]
             }else{
               s=grep(paste0('^',subdir,'(.*?)/'),s,value=TRUE)   
             }
           }
           
           if(full.names){ 
             
              raw_git <- sprintf('https://raw.githubusercontent.com/%s/%s/%s',path,branch,s) 
             
              if(nzchar(myPAT)){
               dlPAT <- gsub('^(.*?)\\?','',httr::content(httr::GET(sprintf('https://api.github.com/repos/%s/contents/%s?access_token=%s',path,s[1],myPAT)))$download_url)
               raw_git <- sprintf('%s?%s',raw_git,dlPAT)
              }
           
              s <- raw_git
           }
           
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
                 s=grep(paste0('^',subdir,'(.*?)/'),s,value=TRUE)   
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
               s=grep(paste0('^',subdir,'(.*?)/'),s,value=TRUE)   
             }
           }
           pathout <- s
         },
         svn={
           
           chk_svn<-FALSE
           
           if(dir.exists(path)){
             newdir=FALSE
             td <- tools::file_path_as_absolute(path)
           }else{
             newdir=TRUE
             td <- tempdir()  
           }
           
           uri_svn <- sprintf('svn info %s',path)
           chk_svn <- length(suppressWarnings(x<-system(uri_svn,intern = TRUE)))==0
           if(chk_svn) stop(sprintf("repo: %s not found", uri_svn))
           
           if(newdir) system(sprintf('svn co %s --depth empty %s',path,td)) 
           setwd(td)
           
           if(is.null(subdir)) subdir=''
           
             if(subdir=='.'){
               s<-system("svn ls | grep -v '/$'",intern = TRUE)
             }else{
               s<-system(sprintf("svn ls -R | grep '^%s'",subdir),intern = TRUE)
               s<-s[!grepl('/$',s)]
             }
           if(full.names){
            s <- file.path(ifelse(newdir,path,td),s)
           }
           pathout <- s
           setwd(this_wd)
           if(newdir) unlink(td)
         }
  )
  setwd(this_wd)
  pathout
}