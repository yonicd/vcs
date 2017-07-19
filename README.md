# vcs 

vcs is a R package that allows users to gain control over their remote repositories on a version control system from within R. 

## Installation
```r
devtools::install_github('metrumresearchgroup/vcs')
``` 

## Command Line Utilities

### Functionalities that do not need a clone
  - **grepr**: run recursive grep directly on local paths and remote branches
  - **list_repos**: list repositories of a user in a version control repository
  - **ls_remote**: list files on remote branches on a version control repository
  - **navigate_remote**: visualize the structure of a remote branch prior to cloning/forking
  - **setwd_remote**: replace inline script from local path to remote path
  - **source_remote**: source script on remote branches, works for reading in data on remote and nested sourcing

### Functionalities that clone or update a cloned repository
  - **diff_head**: query the difference in files between current fetch and the HEAD of a repository
  - **sparse_checkout**: create a sparse checkout of a repository on github/bitbucket/svn
  - **update_depth**: if the git repository was checked out with a depth setting, use this function to update or cancel the depth setting

## GUI

### Interactive recursive grep

  - Run recursive grep directly on remote branches of Github/Bitbucket/SVN and inspect the results using an interactive UI.
  - Preview* text files in the branch without needing to checkout (Github/BitBucket only)
  - The search term inputed will be used in the preview in a search textbox to highlight the term in document. The user can then continue and use the search box to search for other terms int he text. 

![](https://github.com/yonicd/jsTree/blob/master/Miscellaneous/jstree_vcs_grepr.gif?raw=true)

### Sasha

Alexa for R!

Let Sasha help you control your version control repositories both on your checkouts and on the master. Just point Sasha to a root path where the repositories are set and the rest is done for you. 

  - Toggle between local checkouts and remote repos
  
  - Local
  
    - Auto recognition of svn and git parent folders
    - Checks to see what the current (sparse) checkout is compared to HEAD
    - Users can update the definitions of the (sparse) checkout through the GUI
    
  - Remote
    - Navigate to any online repository on Github, Bitbucket or SVN
    - View the structure of a branch in the repo
    - Preview* text files in the branch without needing to checkout (Github/BitBucket only)
    - Search within previewed file for text
    - Select files to create (sparse) checkout and check them out to local path of choice

![](https://github.com/yonicd/vcs/blob/master/Miscellaneous/sasha.gif?raw=true)

### *Limitations

  Limitations of the the internal viewer of RStudio Preview and how it limits the functionality of the [jsTree](https://github.com/metrumresearchgroup/jsTree) package (that is the engine of the VCS GUI): 

  - **mac OS** internal viewer does not allow for user defined context menus. So you can't right click on a file in the tree and preview, you need to use the preview button above the tree. This forces you to preview the first file it finds with a filled checkbox.
  - **windows OS** internal viewer does not allow to read files from online so no preview.
  