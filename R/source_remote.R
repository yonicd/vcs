#' @title Source File on a Remote Repository
#' @description Evaluate script directly from remote repository 
#' including reading data and nested sources.
#' @param repo character, username/repo_name
#' @param branch character, alias of the branch in the repository, Default: 'master'
#' @param subdir character, subdirectory of repository
#' @param r.file character, file name to source
#' @param vcs character, choose which version control system to search (github, bitbucket), Default: 'github'
#' @param flag boolean, checks to see if the file is a nested file Default: TRUE
#' @examples 
#' \donttest{
#' repo='stan-dev/example-models'
#' subdir='ARM/Ch.10'
#' r.file='10.4_LackOfOverlapWhenTreat.AssignmentIsUnknown.R'
#' source_remote(repo,subdir,r.file)
#' }
#' @export
source_remote=function(repo,subdir,r.file,branch='master',vcs='github',flag=TRUE){
  
  if(vcs=='bitbucket') path=file.path('https://bitbucket.org',repo,'raw',branch,subdir)
  if(vcs=='github') path=file.path('https://raw.githubusercontent.com',repo,branch,subdir)
  
  filepath=file.path(path,r.file)
  
  #Read R code ----  
  r.code=readLines(filepath)
  #strsplit(gsub('\\r','',RCurl::getURL(code.loc)[1]),'\\n')[[1]]
  
  #Rewrite paths for source and read commands to url path ----
  for(i in which(grepl('read|source',r.code))) r.code[i]=setwd_remote(r.code[i],repo,subdir,vcs,'stan')
  stan.find=which(grepl('stan\\(',r.code))
  to.unlink=rep(NA,length(stan.find))
  
  #Find the names of the objects that the stan calls are saved to ----
  keep.files=gsub(' ','',unlist(lapply(strsplit(r.code[which(grepl('stan\\(',r.code))],'<-'),'[',1)))
  
  # Comment out print calls ----
  r.code=gsub('print','#print',r.code)
  r.code=gsub('pairs','#pairs',r.code)  
  if(length(keep.files)>0){
    for(i in 1:length(keep.files)){
      comment.out=r.code[grep(keep.files[i],r.code)[!grepl('#|<-|=',r.code[grep(keep.files[i],r.code)])]]
      r.code[grep(keep.files[i],r.code)[!grepl('#|<-|=',r.code[grep(keep.files[i],r.code)])]]=paste0('#',comment.out)
    }
  }
  
  #Download the stan file to a temp file and change the call to stan from a text object to a connection ----
  if(length(stan.find)>0){
    for(i in 1:length(stan.find)){
      x=c(as.numeric(gregexpr('\\"',r.code[stan.find[i]])[[1]]),as.numeric(gregexpr("\\'",r.code[stan.find[i]])[[1]]))
      x=x[x!=-1]
      file.name=basename(substr(r.code[stan.find[i]],x[1]+1,x[2]-1))
      #eval(parse(text=paste0(file.name,' <- tempfile()')))
      #eval(parse(text=sprintf("download.file('%s/%s',%s,quiet = T,method='curl')",path,file.name,file.name)))
      
      ls.path=ls_remote(path=repo,branch = branch,subdir=subdir,vcs = vcs,full.names = TRUE)
      find.file=match(file.name,basename(ls.path))
      if(length(find.file)==0) stop(sprintf('%s not found in %s/%s'),file.name,repo,subdir)
      down.address=ls.path[find.file]
      eval(parse(text=sprintf("%s<-tempfile(fileext = tools::file_ext('%s'))",file.name,file.name)))
      
      eval(parse(text=sprintf("download.file('%s',%s,quiet = T,method='curl')",
                              down.address,file.name))
      )
      
      to.unlink[i]=file.name
      r.code[stan.find[i]]=gsub(substr(r.code[stan.find[i]],x[1],x[2]),
                                strip.path(substr(r.code[stan.find[i]],x[1]+1,x[2]-1)),
                                r.code[stan.find[i]])
    }
  }
  
  #Evaluate new code ----
  eval(parse(text=r.code))
  
  #Unlink temp stan files ----
  junk=sapply(to.unlink[!is.na(to.unlink)],unlink)
  
  #Return objects (conditional if call is nested or not) ----
  if(flag){ret.obj=keep.files}else{ret.obj=ls(pattern = '[^(flag|r.code|keep.files)]')}
  list.out <- sapply(ls()[ls()%in%ret.obj], function(x) get(x))
  
  return(list.out)
  #End of function ----
}

strip.path=function(y){
  str=strsplit(y,'[\\/]')[[1]]
  str[length(str)]
}