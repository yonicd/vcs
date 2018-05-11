#' @title Recursive gsub
#' @description Recursive call to gsub in R
#' @param pattern   character, string containing a regular expression
#' @param replacement   character, replacement pattern
#' @param path      character, path to search, see details
#' @param recursive logical, Should the listing recurse into directories?
#' passed to list.files, Default: FALSE
#' @param test logical, Should the gsub be tested to console or 
#' replace on disk, Default: TRUE
#' @param ...       arguments passed to gsub
#' @return character
#' @examples 
#' 
#' dir.create(file.path(tempdir(), 'gsubr/d'),recursive = TRUE)
#' 
#' path_up <- tempfile(pattern = 'gsubr-', 
#' fileext = '.txt', 
#' tmpdir = file.path(tempdir(), 'gsubr'))
#' 
#' path_down <- tempfile(pattern = 'gsubr-', 
#' fileext = '.txt', 
#' tmpdir = file.path(tempdir(), 'gsubr/d'))
#' 
#' cat('this is a test',sep='\n',file = path_up)
#' 
#' cat('this is a test in the subdir',sep='\n',file = path_down)
#' 
#' list.files(tempdir(),recursive = TRUE)
#' 
#' gsubr(pattern = '\\bis\\b', 
#' replacement = 'was', 
#' path = tempdir(),
#' recursive = TRUE)
#' 
#' gsubr(pattern = '\\bis\\b', 
#' replacement = 'was', 
#' path = tempdir(),
#' recursive = TRUE,
#' test = FALSE)
#' 
#' readLines(path_up)
#' readLines(path_down)
#' 
#' unlink(file.path(tempdir(),'gsubr'),recursive = TRUE)
#' 
#' @export
gsubr=function(pattern, replacement, path,recursive=FALSE, test = TRUE, ...){
  
  gsub_vars <- list(...)
  
  fl <- list.files(path,
                   recursive = recursive,
                   full.names = TRUE)

  out <- sapply(fl,function(x){
    
    args <- gsub_vars
    
    args$pattern <- pattern
    
    args$replacement <- replacement
    
    args$x <- readLines(x, warn = FALSE)
    
    ret <- do.call('gsub',args)
    
    if(test){
      
      grep_args <- args
      
      grep_args$replacement <- NULL
      
      idx <- do.call('grep',grep_args)
      
      sprintf('[line %s] %s',idx,ret[idx])
      
    }else{
      
      cat(ret, file = x, sep = '\n')
    }
      
  })

  out <- out[sapply(out,function(x) length(x)>0 )]
  
  if(test){
    if(length(out)>0){
      return(out)
    }else{
      return(NULL)
    }
  }

}
