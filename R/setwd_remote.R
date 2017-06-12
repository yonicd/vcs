#' @title Set the working directory to a Github repository URL address
#' @description Appends to read and source calls URL path.
#' @param r.script.raw character, script to append remote path.
#' @param path character, path to append
#' @param tokens character, vectors of functions names that will be have a path appended to them.
#' @export
setwd_remote=function(r.script.raw,repo,branch='master',subdir,vcs='github',tokens=NULL){

  #read file from path
    #r.script.raw=readLines(filepath)
  #locate empty and comments lines
    rm.Idx=grepl('^\\s+#|^#|^$',r.script.raw)
  #create working copy without those lines
    r.script=r.script.raw[!rm.Idx]
  #replace all dbl quotes with single quotes
    r.script=gsub('\\"',"'",r.script)
  #update keys with user added
    keys=paste0(sprintf('\\b%s\\b',c(tokens)),collapse='|')
  #locate lines with keys
    keyInd=grep(keys,r.script)
  #save them to str.key
    str.key=grep(keys,r.script,value=TRUE)
  #extract object names
    str.old=regmatches(str.key,regexpr("\\'(.*?)\\'",str.key))
    
    str.change=basename(gsub("[\\']",'',str.old[1]))
    
    dat.loc=file.path(repo,subdir)
    
  if(grepl('source',str.key)){ 
    r.script.new=sprintf("list2env(source_remote(repo='%s',subdir='%s',r.file='%s',flag=FALSE),envir = environment())",
                         repo,subdir,str.change)
  }else{
    # r.script.new=mapply(function(x,str.old){
    #   str.change=basename(gsub("[\\']",'',str.old[1]))
    #   str.new=file.path(path,str.change)
    #   ls.path=ls_remote(path=repo,branch = branch,subdir=subdir,vcs = vcs,full.names = TRUE)
    #   find.file=match(str.change,basename(ls.path))
    #   file.name=gsub('\\s+<-.*$|\\s+=.*$','',x)
    #   eval(parse(text=paste0(file.name,' <<- tempfile()')))
    #   eval(parse(text=sprintf("download.file('%s',%s,quiet = TRUE,method='curl')",str.new,file.name)))
    #   gsub(str.old[1],file.name,x)      
    # },x=str.key,str.old=str.old,USE.NAMES = F)
    
    
    ls.path=ls_remote(path=repo,branch = branch,subdir=subdir,vcs = vcs,full.names = TRUE)
    find.file=match(str.change,basename(ls.path))
    if(length(find.file)==0) stop(sprintf('%s not found in %s/%s'),str.change,repo,subdir)
    down.address=ls.path[find.file]
    file.name=gsub('\\s+<-.*$|\\s+=.*$','',r.script.raw)
    eval(parse(text=sprintf("%s<<-tempfile(pattern = 'VCS_',fileext = tools::file_ext('%s'))",file.name,str.change)))
    #eval(parse(text=sprintf("assign(file.name,envir = parent.frame(),%s)",file.name)))
    eval(parse(text=sprintf("download.file('%s',%s,quiet = T,method='curl')",down.address,file.name)))
    message(str.change,' downloaded from ',down.address,' and placed in tempfile')
    r.script.new=gsub(str.old,file.name,r.script)    
    
  }
  
  r.script[keyInd]=r.script.new
  
  r.script
}