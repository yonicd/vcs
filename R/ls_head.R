#' @title Inspect tree structure of repository head
#' @description Inspect tree structure of git/bitbuck/svn repository head
#' @param path character, Path to root directory of git repository or a name of a github/bitbucket repository, Default: setwd()
#' @param branch character, alias of the github/bitbucket branch to retrieve directory structure, Default: 'master'
#' @details 
#' By default path assumes a local address, if path is a valid repository name eg 'tidyverse/glue' 
#' then the pattern is checked on the specified pulic github repository
#' @return data.frame
#' @export
#' @importFrom tools file_path_as_absolute
#' @importFrom httr http_error GET content
#' @examples 
#' ls_head('tidyverse/glue') #github ('master' branch)
#' ls_head('tidyverse/glue',branch='named_args') #github ('named_args' branch)
#' ls_head('metrumrg/qapply') #bitbucket ('master' branch)
#' 
ls_head <- function(path=getwd(),branch='master'){
  this_wd <- getwd()
  if(!dir.exists(path)){
    uri_git <- sprintf('https://github.com/%s.git',path)
    uri_bit <- sprintf('https://bitbucket.org/!api/1.0/repositories/%s/directory/%s',path,branch)
    
    
    chk_git<-httr::http_error(uri_git)
    chk_bit<-httr::http_error(uri_bit)
    chk_svn<-TRUE
    
    if(chk_git&chk_bit){
      uri_svn <- sprintf('svn info %s',path)
      chk_svn <- length(suppressWarnings(x<-system(uri_svn,intern = TRUE)))>0
      if(chk_svn) stop(sprintf("repo: %s not found", uri))
    }
    
    if(!chk_git){
    tr <- httr::content(httr::GET(sprintf('https://api.github.com/repos/%s/git/trees/%s%s',path,branch,'?recursive=1')))$tree
    s <- sapply(tr,function(x) if(x$mode!='040000') x$path)
    pathout <- unlist(s)
    }
    
    if(!chk_bit)  pathout <- unlist(httr::content(httr::GET(uri_bit))$value)[-1]
    
    if(!chk_svn){
      td <- tempdir()
      system(sprintf('svn co %s --depth empty %s',path,td)) 
      setwd(td)
      pathout<-system("svn ls -R | grep -v '/$'",intern = TRUE)
      unlink(td)
    }
    
  }else{
    setwd(tools::file_path_as_absolute(path))
    pathout <- system('git ls-tree -r HEAD --name-only',intern=TRUE)
  }
  setwd(this_wd)
  pathout
}