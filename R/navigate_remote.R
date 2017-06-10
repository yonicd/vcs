#' @title Visually inspect structure of git repository before initial fetch
#' @description Visually inpsect the directory structure of it before cloning/fetching/pulling
#' @param path character, Path to root directory of git repository or a name of a github/bitbucket repository, Default: setwd()
#' @param branch character, alias of the github/bitbucket branch to retrieve directory structure, Default: 'master'
#' @param vcs character, choose which version control system to search (github, bitbucket, git (head of checkout repo), svn), Default: 'github'
#' @param output character, htmlwidget function to send the output of the \code{\link{ls_remote}}, 
#' options to visualize are jsTree and d3Tree.
#' @param output.opts parameters to pass to the output function
#' @details 
#' By default path assumes a local address, if path is a valid repository name eg 'tidyverse/glue' 
#' then the pattern is checked on the specified pulic github repository
#' @return data.frame
#' @export
#' @importFrom tools file_path_as_absolute
#' @importFrom d3Tree d3tree df2tree
#' @importFrom jsTree jsTree
#' @importFrom plyr rbind.fill
#' @importFrom htmltools html_print
#' @importFrom httr http_error GET content
#' @examples 
#' navigate_remote('tidyverse/glue')
#' navigate_remote('tidyverse/glue',branch='named_args')
#' @seealso
#'  \code{\link[d3Tree]{d3tree}}
#'  \code{\link[jsTree]{jsTree}}
navigate_remote <- function(path=getwd(), branch='master', vcs='github', output='jsTree', output.opts=list()){
 
  pathout <- ls_remote(path = path,branch = branch,vcs = vcs)
  tree.args=list()
  switch(output,
         jsTree={
           tree.args$obj = pathout
           tree.args$remote_repo = ifelse(vcs%in%c('github','git'),path,NULL)
           tree.args=c(tree.args,output.opts)
           do.call(jsTree::jsTree,tree.args)
         },
         d3Tree={
           x <- lapply(strsplit(pathout, "/"), function(z) as.data.frame(t(z)))
           x <- plyr::rbind.fill(x)
           x$depth <- apply(x,1,function(y) sum(!is.na(y)))
           if(is.null(output.opts$layout)) output.opts$layout='collapse'
           tree.args=output.opts
           tree.args$data=list(root = d3Tree::df2tree(rootname='repo',struct=x),
                               layout = output.opts$layout)
           tree.args$layout<-NULL
           do.call(d3Tree::d3tree,tree.args)
           }
         
         )
}