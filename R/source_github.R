#' @title FUNCTION_TITLE
#' @description FUNCTION_DESCRIPTION
#' @param repo PARAM_DESCRIPTION
#' @param subdir PARAM_DESCRIPTION
#' @param r.file PARAM_DESCRIPTION
#' @param flag PARAM_DESCRIPTION, Default: TRUE
#' @examples 
#' repo='stan-dev/example-models'
#' subdir='ARM/Ch.10'
#' r.file='10.6_IVinaRegressionFramework.R'
#' source_github(repo,subdir,r.file)
#' @export
source_github=function(repo,subdir,r.file,flag=TRUE){

  #Read R code ----  
  r.code=readLines(code.loc)
  #strsplit(gsub('\\r','',RCurl::getURL(code.loc)[1]),'\\n')[[1]]
  
  #Rewrite paths for source and read commands to url path ----
  for(i in which(grepl('read|source',r.code))) r.code[i]=setwd_github(r.code[i])
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
      eval(parse(text=paste0(file.name,' <- tempfile()')))
      loc.file=paste0('"',dat.loc,file.name,'"')
      eval(parse(text=paste0('download.file(',loc.file,',',file.name,',quiet = T,method="curl")')))
      to.unlink[i]=file.name
      r.code[stan.find[i]]=gsub(substr(r.code[stan.find[i]],x[1],x[2]),strip.path(substr(r.code[stan.find[i]],x[1]+1,x[2]-1)),r.code[stan.find[i]])
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