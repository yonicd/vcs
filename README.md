# vcs 

vcs is a R package that allows users to gain control over their remote repositories on a version control system from within R. 

## Installation
```r
devtools::install_github('metrumresearchgroup/vcs')
``` 

## Functionalities that do not need a clone
  - **grepr**: run recursive grep directly on remote branches
  - **list_repos**: list repositories of a user in a version control repository
  - **ls_remote**: list files on remote branches on a version control repository
  - **navigate_remote**: visualize the structure of a remote branch prior to cloning/forking
  - **setwd_remote**: replace inline script from local path to remote path
  - **source_remote**: source script on remote branches, works for reading in data on remote and nested sourcing

## Functionalities that clone or update a cloned repository
  - **diff_head**: query the difference in files between current fetch and the HEAD of a repository
  - **sparse_checkout**: create a sparse checkout of a repository on github or bitbucket
  - **update_depth**: if the git repository was checked out with a depth setting, use this function to update or cancel the depth setting
