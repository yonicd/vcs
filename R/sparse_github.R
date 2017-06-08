#' @title Create a sparse git repository
#' @description Create a git repository that fetches only specified subdirectories and file types
#' @param repo_url character, repository url path
#' @param repo character, path on local disk that repository is cloned to
#' @param dirs character, vector of repository subdirectories to fetch
#' @param create boolean, create a new git clone?, Default: TRUE
#' @param append boolean, append new lines to sparse-checkout file, Default: TRUE
#' @param remote character, alias of the remote Default: 'origin'
#' @param branch character, alias of the branch, Default: 'master'
#' @return nothing
#' @examples
#' \donttest{
#' repo_url='https://github.com/tidyverse/ggplot2.git'
#' repo='ggplot2-sparse'
#' dirs=c('data-raw/*.csv','man/*.Rd')
#' #create new sparse clone
#' sparse_github(repo_url,repo,dirs)
#' 
#' #update sparse-checkout definitions (appends to current list)
#' sparse_github(repo_url,repo,dirs=c('man/macros/*.Rd'),create=FALSE,append=TRUE)
#' }
#' @export
sparse_github<-function(repo_url,repo,dirs,create=TRUE,append=TRUE,remote='origin',branch='master'){
  
  thisDir=getwd()
  if(!dir.exists(repo)&!dir.exists(file.path(dirname(getwd()),repo))) dir.create(repo)
  if(thisDir!=file.path(dirname(getwd()),repo)) setwd(repo)
  
  if(create){ 
    
    # New repository
    
    system('git init')
    system(sprintf('git remote add -f %s %s',remote,repo_url))
    system('git config core.sparsecheckout true')
    cat(dirs,file = '.git/info/sparse-checkout',sep = '\n')
    system(sprintf('git pull %s %s',remote,branch))
    system(sprintf('git branch --set-upstream-to=origin/%s master',branch))
    
  }else{ 
    
    # Existing repository
    
    system('git config core.sparsecheckout true')
    
    if(append){
      
      dirs_append=dirs[which(!dirs%in%readLines('.git/info/sparse-checkout'))]
      if(length(dirs_append)>0) cat(dirs_append,file = '.git/info/sparse-checkout',sep = '\n',append = append)
      
    }else{
      
      cat(dirs,file = '.git/info/sparse-checkout',sep = '\n')  
      
    }
    
    system('git read-tree -mu HEAD')
    
  }
  
  setwd(thisDir)
}
