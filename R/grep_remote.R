#' @title Recursive grep
#' @description Recursive call to grep in R
#' @param pattern   character, string containing a regular expression
#' @param path      character, path to search, see details
#' @param recursive logical, Should the listing recurse into directories? passed to list.files, Default: FALSE
#' @param padding   integer, number of rows to return in addition to the query line, Default: 5
#' @param ...       arguments passed to grep
#' @return grepr(value = FALSE) returns a vector of the indices of the elements of x 
#' that yielded a match (or not, for invert = TRUE. 
#' This will be an integer vector unless the input is a long vector, when it will be a double vector.
#' grepr(value = TRUE) returns a character vector containing the selected elements of x 
#' (after coercion, preserving names but no other attributes).
#' @details
#' if path is a character then the pattern is checked against a location on the local machine, 
#' if path is a list with arguments containing params for ls_remote
#' then the pattern is checked on the specified remote repository (repository must be public).
#' In this case of the recursive parameter takes over-rides over the list parameter.
#' @examples 
#' grepr(pattern = 'gsub',path = '.',value=TRUE,recursive = TRUE)
#' thisrepo='metrumresearchgroup/vcs'
#' \donttest{
#' remotepath=list(path=thisrepo,subdir='R',vcs='github')
#' grepr(pattern = 'importFrom',path = remotepath)
#' grepr(pattern = 'importFrom',path = remotepath,value=TRUE)
#' grepr(pattern = 'importFrom',path = remotepath,value=TRUE,padding=3)
#' grepr(pattern = 'tags$script',
#'       path = list(path = 'timelyportfolio/vueR',subdir='R|inst',vcs='github'),
#' padding=3,value=TRUE,fixed=TRUE)}
#' @export
#' @importFrom utils head tail
grepr=function(pattern,path,recursive=FALSE,padding=0,...){
  grepVars=list(...)
  list2env(grepVars,envir = environment())
  
  if(is.character(path)) fl=list.files(path,recursive = recursive,full.names = TRUE)
  vcs='git'
  
  if(is.list(path)){
    path$full.names=TRUE
    fl=do.call(ls_remote,path)
    vcs=path$vcs
  } 

  out=sapply(fl,function(x){
    args=grepVars
    args$pattern=pattern
    if(is.null(args$value)) args$value=FALSE
    if(vcs=='svn'){
      args$x=system(sprintf('svn cat %s',x),intern = TRUE)
    }else{
      args$x=readLines(x,warn = FALSE) 
    }
    
    
    if(padding>0&args$value){
      args0<-args
      args0$value=FALSE
      g0=do.call(grep,args0)
      if(length(g0)>0){
        g=g0
        if(length(g0)>1){
          rmIdx=which(utils::tail(g0,-1)-utils::head(g0,-1)<=padding)
          if(length(rmIdx)>0) g=g0[-c(rmIdx+1)]
        } 
        gdx=sapply(g,function(x,pad,nmax) seq(from=pmax(1,x-pad),to=pmin(nmax,x+pad)),pad=padding,nmax=length(args$x))
        out=unique(unlist(sapply(gdx,function(i){
              ifelse(i%in%g0,sprintf('[row %s]: %s',i,args$x[i]),sprintf('row %s: %s',i,args$x[i]))
            })))
        
      }
    }else{
      do.call('grep',args)
    } 
  })
  
  out[sapply(out,length)>0]
  
}
