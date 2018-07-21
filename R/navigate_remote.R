#' @title Visually inspect structure of git repository before initial fetch
#' @description Visually inpsect the directory structure of it before cloning/fetching/pulling
#' @param path character, Path to root directory of git repository or a name of a github/bitbucket repository, Default: setwd()
#' @param branch character, alias of the github/bitbucket branch to retrieve directory structure, Default: 'master'
#' @param vcs character, choose which version control system to search (github, bitbucket, git (head of checkout repo), svn), Default: 'github'
#' @param show_tree boolean, show jsTree widget output in viewer, Default: TRUE
#' @details 
#' By default path assumes a local address, if path is a valid repository name eg 'tidyverse/glue' 
#' then the pattern is checked on the specified pulic github repository
#' @return widget
#' @export
#' @importFrom jsTree jsTree
#' @examples 
#' navigate_remote('tidyverse/glue')
#' navigate_remote('tidyverse/glue',branch='named_args')
#' @seealso
#'  \code{\link[jsTree]{jsTree}}
navigate_remote <- function(path=getwd(), branch='master', vcs='github', show_tree = TRUE){

  tree <- jsTree::jsTree$new(ls_remote(path = path,branch = branch,vcs = vcs))

  tree$add_vcs(remote_repo = path,vcs = vcs,remote_branch = branch)

  if(show_tree){
    tree$show()
  }else{
    invisible(tree)
  }
    
}