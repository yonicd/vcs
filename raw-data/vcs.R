vcs <- R6Class("vcs",
                  public = list(
                    repo=NULL,
                    branch=NULL,
                    vcs=NULL,
                    remote=NULL,
                    initialize = function(repo=NA,branch='master',vcs='github',remote='origin') {
                      self$repo   <- repo
                      self$remote <- remote
                      self$branch <- branch
                      self$vcs    <- vcs
                      self$check()
                    },
                    set_branch = function(val) {self$branch <- val},
                    set_vcs = function(val) {self$vcs <- val},
                    check = function(){
                      chekcat <- switch(self$vcs,
                             github={
                               uri_git <- sprintf('https://github.com/%s.git',self$repo)
                               if(httr::http_error(uri_git)){
                                 sprintf("repo: %s not found", uri_git)
                               }else{
                                 sprintf("repo: %s found", uri_git)
                               } 
                             },
                             bitbucket={
                               uri_bit <- sprintf('https://bitbucket.org/!api/1.0/repositories/%s/directory/%s',self$repo,self$branch)
                               if(httr::http_error(uri_bit)){
                                 sprintf("repo: %s not found", self$repo) 
                               }else{
                                 sprintf("repo: %s found", self$repo)
                               }
                             },
                             git={
                               uri_git <- tools::file_path_as_absolute(self$repo)
                               if(!dir.exists(file.path(uri_git,'.git'))){
                                 sprintf("repo: %s not found", uri_git) 
                               }else{
                                 thiswd=getwd()
                                 setwd(uri_git)
                                 system('git status')
                                 setwd(thiswd)
                               }
                             },
                             svn={
                               uri_svn <- system(sprintf('svn info %s',self$repo),intern=TRUE)
                             })
                      cat(chekcat)
                    }
                  )
)
