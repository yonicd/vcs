#' @title list.dir for git and svn repositories
#' @description list parent directories of svn repositores within a path
#' @param path character, path to seach for svn repositories
#' @param vcs character, vector of what vcs systems to look for, Default: 'git'
#' @return character
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  find.remote('/data')
#'  }
#' }
#' @rdname find.remote
#' @export 

find.remote<-function(path,vcs=c('git','svn')){
  
x<-dir(path,pattern = sprintf('[.](%s)$',paste0(vcs,collapse='|')),
       all.files = TRUE,include.dirs = TRUE,recursive = TRUE,full.names = TRUE)

diff=1000
while(diff>0){
x.now<-x
for(idx in 1:length(x)){
  if(idx>length(x)) break
  idx.rm<-which(grepl(dirname(x[idx]),x[-idx]))
  if(length(idx.rm)>0) x<-x[-idx.rm]
}
diff<-length(x.now)-length(x)
}

ret <- data.frame(vcs=gsub('^[.]{1}','',basename(x)),dir=dirname(x),remote = NA,stringsAsFactors = FALSE)

if(nrow(ret)>0){
  for(i in 1:nrow(ret)){
    ret$remote[i] <- query_remote(ret$vcs[i],ret$dir[i])  
  }  
}

ret

}

query_remote <- function(x,y){
  
  if(x=='git'){
    this <- sprintf('cat %s/.git/config | grep url',y)
  }
  
  if(x=='svn'){
    this <- sprintf("svn info %s | grep URL",y)
  }
  
  gsub('^(.*?)[=:]\\s*','',system(this,intern = TRUE)[1])
}