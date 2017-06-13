#' @title Source File on a Remote Repository
#' @description Evaluate script directly from remote repository 
#' including reading data and nested sources.
#' @param repo character, username/repo_name
#' @param branch character, alias of the branch in the repository, Default: 'master'
#' @param subdir character, subdirectory of repository
#' @param r.file character, file name to source
#' @param tokens character, vector of functions names that will be have a path appended to them
#' @param vcs character, choose which version control system to search (github, bitbucket), Default: 'github'
#' @param flag boolean, checks to see if the file is a nested file Default: TRUE
#' @examples 
#' \donttest{
#' repo='stan-dev/example-models'
#' subdir='ARM/Ch.10'
#' r.file='10.4_LackOfOverlapWhenTreat.AssignmentIsUnknown.R'
#' source_remote(repo,subdir,r.file,tokens='stan')
#' }
#' @export
source_remote=function(repo,subdir,r.file,tokens=NULL,branch='master',vcs='github',flag=TRUE){
  
  vcsdir <- file.path(tempdir(),'vcs')
  if(!dir.exists(vcsdir)) dir.create(vcsdir)
  on.exit({unlink(file.path(tempdir(),'vcs'))},add = TRUE)
  
  if(vcs=='bitbucket') path=file.path('https://bitbucket.org',repo,'raw',branch,subdir)
  if(vcs=='github') path=file.path('https://raw.githubusercontent.com',repo,branch,subdir)
  
  filepath=file.path(path,r.file)
  
  #Read R code ----  
  r.code=readLines(filepath)
  #strsplit(gsub('\\r','',RCurl::getURL(code.loc)[1]),'\\n')[[1]]
  
  tokens.str=paste0(tokens,'\\(')
  #tokens<-c('read','source',tokens.str)
  tokens_base<-c('read','source')
  
  #Rewrite paths for source and read commands to url path ----
  for(i in grep(paste0(tokens_base,collapse='|'),r.code)) r.code[i]=setwd_remote(r.code[i],repo,branch,subdir,vcs,vcsdir=vcsdir,vcsenv=environment(),tokens_add=tokens)
  token.find=which(grepl(tokens.str,r.code))
  to.unlink=rep(NA,length(token.find))
  
  #Find the names of the objects that the token calls are saved to ----
  keep.files=gsub(' ','',unlist(lapply(strsplit(r.code[which(grepl(tokens.str,r.code))],'<-'),'[',1)))
  
  # Comment out print calls ----
  r.code=gsub('print','#print',r.code)
  r.code=gsub('pairs','#pairs',r.code)  
  if(length(keep.files)>0){
    for(i in 1:length(keep.files)){
      comment.out=r.code[grep(keep.files[i],r.code)[!grepl('#|<-|=',r.code[grep(keep.files[i],r.code)])]]
      r.code[grep(keep.files[i],r.code)[!grepl('#|<-|=',r.code[grep(keep.files[i],r.code)])]]=paste0('#',comment.out)
    }
  }
  
  #Download the token files to a temp.file and change the call to a connection ----
  if(length(token.find)>0){
    for(i in 1:length(token.find)){
      
      r.code[token.find[i]]=gsub('\\"',"'",r.code[token.find[i]])
      str.old=regmatches(r.code[token.find[i]],regexpr("\\'(.*?)\\'", r.code[token.find[i]]))
      str.change=gsub("[\\']",'',str.old[1])
      file.name=basename(str.change)
      
      ls.path=ls_remote(path=repo,branch = branch,
                        subdir=ifelse(!is.null(dirname(str.change)),subdir),
                        vcs = vcs,full.names = TRUE)
      
      find.file=match(file.name,basename(ls.path))
      if(length(find.file)==0) stop(sprintf('%s not found in %s/%s'),file.name,repo,subdir)
      down.address=ls.path[find.file]
      eval(parse(text=sprintf("%s<-tempfile(pattern = 'VCS_',tmpdir = vcsdir,fileext = tools::file_ext('%s'))",file.name,file.name)))
      eval(parse(text=sprintf("download.file('%s',%s,quiet = T,method='curl')",down.address,file.name)))
      message(str.change,' downloaded from ',down.address,' and placed in tempfile')
      to.unlink[i]=file.name
      r.code[token.find[i]]=gsub(str.old,file.name,r.code[token.find[i]])
    }
  }
  
  #Evaluate new code ----
  eval(parse(text=r.code))
  
  #Unlink temp token files ----
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