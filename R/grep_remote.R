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
#' if path is a list with arguments containing params for ls_github(repo,subdir) 
#' then the pattern is checked on the specified github repository (repository must be public).
#' In this case of the recursive parameter takes over-rides over the list parameter.
#' @examples 
#' grepr(pattern = 'gsub',path = '.',value=TRUE,recursive = T)
#' grepr(pattern = 'importFrom',path = list(repo='yonicd/ciderhouse',subdir='R'),value=TRUE)
#' @export
#' 
grepr=function(pattern,path,recursive=FALSE,padding=0,...){
  grepVars=list(...)
  list2env(grepVars,envir = environment())
  if(is.character(path)) fl=list.files(path,recursive = recursive,full.names = TRUE)
  if(is.list(path)){
    path$full.names=TRUE
    fl=do.call(ls_remote,path)
  } 

  out=sapply(fl,function(x){
    args=grepVars
    args$pattern=pattern
    args$x=readLines(x,warn = FALSE)
    
    if(padding>0){
      g=grep(args$pattern,args$x)
      if(length(g)>0){
        gdx=sapply(g,function(x,pad,nmax) seq(from=pmax(1,x-pad),to=pmin(nmax,x+pad)),pad=padding,nmax=length(args$x))
        apply(gdx,2,function(i) data.frame(row=i,result=args$x[i],stringsAsFactors = FALSE))
      }
    }else{
      do.call('grep',args)
    } 
  })
  out[sapply(out,length)>0]
  
}