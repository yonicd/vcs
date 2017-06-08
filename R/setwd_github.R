#' @title Set the working directory to a Github repository URL address
#' @description Appends to read and source calls URL path.
#' @param path URL address
#' @export
setwd_github=function(repo,subdir,r.file,add_keywords='stan'){

  path=file.path('https://raw.githubusercontent.com',repo)
  if(!is.null(subdir)) path=file.path(path,'master',subdir)
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
    keys=paste0(sprintf('\\b%s\\b',c('read','source',add_keywords)),collapse='|')
  #locate lines with keys
    keyInd=grep(keys,r.script)
  #save them to str.key
    str.key=grep(keys,r.script,value=T)
  #extract object names
    str.old=regmatches(y,regexpr("\\'(.*?)\\'",str.key))

  if(grepl('source',str.key)){ 
    r.script.new=sprintf("list2env(source_github(path,%s,flag=FALSE),envir = environment())",str.old)
  }else{
    r.script.new=mapply(function(x,str.old){
      str.change=basename(gsub("[\\']",'',str.old[1]))
      str.new=file.path(path,str.change)
      ls.path=ls_github(repo,subdir)
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