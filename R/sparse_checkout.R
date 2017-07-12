#' @title Create a sparse git repository
#' @description Create a git repository from a version control system that fetches only specified subdirectories and file types, 
#' can be used for github and bitbucket.
#' @param repo_url character, repository url path
#' @param dest.dir character, path on local disk that repository is cloned to
#' @param queries character, vector of repository subdirectories to fetch
#' @param vcs character, choose which version control system to checkout files from (git, svn) ,  Default: 'git'
#' @param create boolean, create a new git clone?, Default: TRUE
#' @param append boolean, append new lines to sparse-checkout file, Default: TRUE
#' @param remote character, alias of the remote Default: 'origin'
#' @param branch character, alias of the branch, Default: 'master'
#' @param depth integer, depth of revisions to checkout, Default: 1
#' @param svn.set.depth character, parameter passed to svn update that controls the depth folders are updated to (see details) Default: 'immediates'
#' @details Since bitbucket and github are both git version control systems only two options are needed for vcs, passing those values to vcs will default back to git.
#' The parameters (create,remote, branch,depth) are used only when vcs=='git'. The possible values for svn.set.depth are c('files','immediates','infinity').
#' For further documentaion see \url{http://svnbook.red-bean.com/en/1.7/svn.advanced.sparsedirs.html}.
#' @return nothing
#' @examples
#' \donttest{
#' repo_url='https://github.com/tidyverse/ggplot2.git'
#' dest.dir='ggplot2-sparse'
#' queries=c('data-raw/*.csv','man/*.Rd')
#' #create new sparse clone
#' sparse_checkout(repo_url,dest.dir,queries)
#' 
#' #update sparse-checkout definitions (appends to current list)
#' sparse_checkout(repo_url,dest.dir,queries=c('man/macros/*.Rd'),create=FALSE,append=TRUE)
#' }
#' @export
sparse_checkout<-function(repo_url, dest.dir, queries, vcs='git', create=TRUE, append=TRUE, remote='origin', branch='master', depth=Inf, svn.set.depth='immediates'){
  thisDir=getwd()
  switch(vcs,
         svn={
           chk_svn<-FALSE
           
           if(dir.exists(dest.dir)){
             
             newdir=FALSE
             dest.dir <- normalizePath(path.expand(dest.dir),winslash = '/',mustWork = TRUE)
             
           }else{
             
             newdir <- TRUE
             dir.create(dest.dir)
             file.create(file.path(dest.dir,'.svn_sparse'))
             
           }
           
           uri_svn <- sprintf('svn info %s',repo_url)
           
           chk_svn <- length(suppressWarnings(x<-system(uri_svn,intern = TRUE)))==0
           
           if(chk_svn) stop(sprintf("repo: %s not found", uri_svn))
           
           if(newdir) system(sprintf('svn co %s --depth empty %s',repo_url,dest.dir)) 
           
           setwd(dest.dir)
           
           if(append){
             
             dirs_append <- queries[which(!queries%in%readLines('.svn_sparse'))]
             
             if(length(dirs_append)>0) cat(dirs_append,file = '.svn_sparse',sep = '\n',append = append)
             
            }else{
 
              #reset the root directory
              system(sprintf('svn update --set-depth empty'))
              
              cat(queries,file = '.svn_sparse',sep = '\n')
            }
           
           queries <- readLines('.svn_sparse')
           
           file.ext <- rep(NA,length(queries))
           
           #remove trailer slashes
           queries <- gsub('/$','',queries)
           
           #identify subdirectories, extensions and convert glob wildcards to rx
           for(i in 1:length(queries)){
             if(grepl('[.]',basename(queries[i]))){
               file.ext[i]<-gsub('[*]','(.*?)',queries[i])
               queries[i]<-dirname(queries[i])
             }else{
               file.ext[i]<-paste0(queries[i],'/$')
             }
           }
           
           #create sequence of subdirectories for svn empty
           to_empty<-unique(do.call('c',lapply(strsplit(queries,'/'),function(x){
             sapply(1:length(x),function(y) paste(x[1:y],collapse = '/')) 
           })))
           
           #initialize only missing directories
           to_empty <- to_empty[!dir.exists(to_empty)]
           
           if(length(to_empty)>0) system(sprintf('svn update --set-depth empty %s',paste(to_empty,collapse=' ')))
           
           #retrieve recursive remote ls
           s <- system("svn ls -R",intern = TRUE)
           
           #filter for queries
           s1<-do.call('c',sapply(file.ext,grep,x=s,value=TRUE,USE.NAMES = FALSE,simplify = FALSE))
           
           #update svn with query files and subdirectories
           system(sprintf('svn update --set-depth %s %s',svn.set.depth,paste(s1,collapse=' ')))
         },
         {
           
           if(!dir.exists(dest.dir)&!dir.exists(file.path(dirname(getwd()),dest.dir))) dir.create(dest.dir)
           if(thisDir!=file.path(dirname(getwd()),dest.dir)) setwd(dest.dir)
           
           if(create){ 
             if(!'.git'%in%dir(all.files = TRUE)){
               # New repository
               system('git init')
               system(sprintf('git remote add -f %s %s',remote,repo_url))
             }
             system('git config core.sparsecheckout true')
             cat(queries,file = '.git/info/sparse-checkout',sep = '\n')
             fetch=sprintf("git fetch %s", remote)
             if(depth!=Inf) sprintf("%s --depth %s", fetch, depth)
             system(fetch)
             system(sprintf("git merge %s/%s", remote,branch))
             system(sprintf('git branch --set-upstream-to=origin/%s master',branch))
             
           }else{ 
             
             # Existing repository
             
             system('git config core.sparsecheckout true')
             
             if(append){
               
               dirs_append=queries[which(!queries%in%readLines('.git/info/sparse-checkout'))]
               if(length(dirs_append)>0) cat(dirs_append,file = '.git/info/sparse-checkout',sep = '\n',append = append)
               
             }else{
               
               cat(queries,file = '.git/info/sparse-checkout',sep = '\n')  
               
             }
             
             system('git read-tree -mu HEAD')
             
           }
         })
  
  setwd(thisDir)
}
