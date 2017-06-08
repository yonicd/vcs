#' @title Visually inspect structure of git repository before initial fetch
#' @description Visually inpsect the directory structure of it before cloning/fetching/pulling
#' @param path character, Path to root directory of git repository or a name of a github/bitbucket repository, Default: setwd()
#' @param branch character, alias of the github/bitbucket branch to retrieve directory structure, Default: 'master'
#' @param layout character, Layout of d3Tree output collapse, cartesian, radial Default: 'collapse'
#' @param ... parameters to pass to d3tree
#' @details 
#' By default path assumes a local address, if path is a valid repository name eg 'tidyverse/glue' 
#' then the pattern is checked on the specified pulic github repository
#' @return data.frame
#' @export
#' @importFrom tools file_path_as_absolute
#' @importFrom d3Tree d3tree df2tree
#' @importFrom plyr rbind.fill
#' @importFrom htmltools html_print
#' @importFrom httr http_error GET content
#' @examples 
#' show_repo('tidyverse/glue')
#' show_repo('tidyverse/glue',gh_branch='named_args')
#' @seealso
#'  \code{\link[d3Tree]{d3tree}}
show_repo <- function(path=getwd(),branch='master',showTree=TRUE,layout='collapse',...){
  this_wd <- getwd()
  if(!dir.exists(path)){
    uri_git <- sprintf('https://github.com/%s.git',path)
    uri_bit <- sprintf('https://bitbucket.org/!api/1.0/repositories/%s/directory/%s',path,branch)
    chk_git<-httr::http_error(uri_git)
    chk_bit<-httr::http_error(uri_bit)
    if(chk_git&chk_bit) stop(sprintf("repo: %s not found", uri))
    if(!chk_git){
    tr <- httr::content(httr::GET(sprintf('https://api.github.com/repos/%s/git/trees/%s%s',path,branch,'?recursive=1')))$tree
    s <- sapply(tr,function(x) if(x$mode!='040000') x$path)
    pathout <- unlist(s)
    }
    if(!chk_bit)  pathout <- unlist(httr::content(httr::GET(uri_bit))$value)[-1]
    
  }else{
    setwd(tools::file_path_as_absolute(path))
    pathout <- system('git ls-tree -r HEAD --name-only',intern=TRUE)    
  }

  x <- lapply(strsplit(pathout, "/"), function(z) as.data.frame(t(z)))
  x <- plyr::rbind.fill(x)
  x$depth <- apply(x,1,function(y) sum(!is.na(y)))
  setwd(this_wd)
  if(showTree){
    htmltools::html_print(d3Tree::d3tree(
      list(root = d3Tree::df2tree(rootname='repo',struct=x),layout = layout),...)
    )
    invisible(x)
  }else{
    pathout
  }
}