#' @title List files of a github repository subdirectory
#' @description List full raw paths to files in a github repository subdirectory
#' @param repo character, Repository address in the format username/repo
#' @param branch character, Branch of repo, Default: 'master' 
#' @param subdir character, subdirectory within repo that contains the files to list
#' @param full.names boolean, If TRUE, the directory path is prepended to the file names to give a relative file path. 
#' If FALSE, the file names (rather than paths) are returned, Default: TRUE
#' @param recursive boolean, Should the listing recurse into directories?, Default: FALSE
#' @importFrom httr content GET
#' @examples 
#' l=ls_github('tidyverse/ggplot2')
#' l
#' #geom-boxplot.r
#' readLines(l[42])
#' @export
ls_github=function(repo,branch='master',subdir=NULL,full.names=FALSE,recursive=FALSE){

r=''
if(recursive) r='?recursive=1'  
tr=httr::content(httr::GET(sprintf('https://api.github.com/repos/%s/git/trees/%s%s',repo,branch,r)))$tree
s=sapply(tr,function(x) if(x$mode!='040000') x$path)

if(!is.null(subdir)){
  s=grep(paste0('^',subdir,'/'),s,value=TRUE)
  g=grep(sprintf('(/.*){%s,}',min(nchar(gsub('[^//]','',s)))+1),s)
  if(length(g)>0) if(!recursive) s=s[-g]
}else{
  s=unlist(s)
} 
if(full.names){
  sprintf('https://raw.githubusercontent.com/%s/%s/%s',repo,branch,s) 
}else{
  s
}
}
