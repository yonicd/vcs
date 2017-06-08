#' @title Set the working directory to a Github repository URL address
#' @description Appends to read and source calls URL path.
#' @param repo character, username/repo_name
#' @param branch character, branch of repository, Default: 'master'
#' @param subdir character, subdirectory of repository
#' @param r.file character, file name to source
#' @param vcs character, choose which version control system to search (github, bitbucket), Default: 'github'
#' @param tokens character, vectors of functions names that will be have a path appended to them.
#' @export
setwd_remote=function(repo,branch='master',subdir,r.file,vcs='github',tokens='stan'){

  if(vcs=='bitbucket') path=file.path('https://bitbucket.org',repo,'raw',branch,subdir)
  if(vcs=='github') path=file.path('https://raw.githubusercontent.com',repo,branch,subdir)

  filepath=file.path(path,r.file)
  
  #read file from path
    r.script.raw=readLines(filepath)
  #locate empty and comments lines
    rm.Idx=which(grepl('^\\s+#|^#|^$',r.script.raw))
  #create working copy without those lines
    r.script=r.script.raw[-rm.Idx]
  #replace all dbl quotes with single quotes
    r.script=gsub('\\"',"'",r.script)
  #update keys with user added
    keys=paste0(sprintf('\\b%s\\b',c('read','source',tokens)),collapse='|')
  #locate lines with keys
    keyInd=grep(keys,r.script)
  #save them to str.key
    str.key=grep(keys,r.script,value=T)
  #extract object names
    str.old=regmatches(y,regexpr("\\'(.*?)\\'",str.key))

  if(grepl('source',str.key)){ 
    r.script.new=sprintf("list2env(source_remote(path,%s,flag=FALSE),envir = environment())",str.old)
  }else{
    r.script.new=mapply(function(x,str.old){
      str.change=basename(gsub("[\\']",'',str.old[1]))
      str.new=file.path(path,str.change)
      ls.path=ls_remote(repo=repo,branch = branch,subdir=subdir,vcs = vcs,full.names = TRUE)
      find.file=match(str.change,basename(ls.path))
      file.name=gsub('\\s+<-.*$|\\s+=.*$','',x)
      #eval(parse(text=paste0(file.name,' <<- tempfile()')))
      #eval(parse(text=sprintf("download.file('%s,%s,quiet = TRUE,method='curl')",str.new,file.name)))
      gsub(str.old[1],file.name,x)      
    },x=str.key,str.old=str.old,USE.NAMES = F)
    
  }
  
  r.script[keyInd]=r.script.new
  
  r.script
}