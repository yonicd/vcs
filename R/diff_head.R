#' @title Query the difference in files between current fetch and the head of a repository
#' @description Query the difference in files between current fetch and the head of a repository, 
#' assuming that there is either a .git or .svn folder in the path. 
#' @param path character, Path to root directory of git/svn repository, Default: '.'
#' @param vcs character, choose which version control system to search (git, svn), Default: 'git'
#' @param pattern   character, string containing a regular expression, Default: NULL
#' @param ...       arguments passed to grep
#' @return character
#' @export
diff_head=function(path='.',vcs='git',pattern=NULL,...){
  h=ls_remote(path,vcs = vcs)
  hd=h[!h%in%list.files(path,recursive = TRUE)]
  if(!is.null(pattern)){
    args<-list(...)
    args$value=TRUE
    args$pattern=pattern
    args$x=hd
    hd=do.call(grep,args)
  } 
  hd
}