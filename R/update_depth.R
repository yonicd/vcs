#' @title Update depth definitions of checked out repository
#' @description Update the settings of a repository that was checked out with a depth parameter
#' @param path character, path on local disk that repository is cloned to
#' @param action character, apply one of 'deepen','shallow-since','shallow-exclude','unshallow'
#' @param action_input parameter needed for the action (see details)
#' @details 
#' action_input for each action: [class of input]:
#' 'deepen': integer
#' specifies the number of commits from the current shallow boundary instead of from the tip of each remote branch history.
#'
#' 'shallow-since': date (%Y-%m-%d)
#'  Deepen or shorten the history of a shallow repository to include all reachable commits after a given date.
#'
#' 'shallow-exclude': integer
#'  Deepen or shorten the history of a shallow repository to exclude commits reachable from a specified remote branch or tag.
#'
#' 'unshallow': boolean
#'  If the source repository is complete, convert a shallow repository to a complete one, removing all the limitations imposed by shallow repositories.
#'   
#' @return nothing
#' @export
update_depth<-function(path,action,action_input){
  
  thisDir=getwd()
  if(thisDir!=file.path(dirname(getwd()),path)) setwd(path)

  if(action!='unshallow'){
    system(sprintf("git fetch --%s=%s --update-shallow", action, action_input)) 
  }else{
    system(sprintf("git fetch --%s", action))
  }
  
  setwd(thisDir)
}
