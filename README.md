# Remote 

remote is a R package that allows users to gain control over their remote repositories from within R. 

## Functionalities that do not need a clone
  - **grepr**: run recursive grep directly on remote branches
  - **ls_github**: list files on remote branches on github and return their raw addresses
  - **setwd_github**: set the working directory to a remote branch to run script directly on it, including reading data and nested sourcing.
  - **source_github**: source script on remote branches
  - **show_repo**: visualize the structure of a remote branch prior to cloning/forking
  - **ls_head**: return directory structure of remote branch prior to cloning/forking

## grepr
Function that applies a recursive grep in R to search in entire directory tree on a local path or in github, returns grep by file that matches pattern. 

### Local path
```r
  grepr(pattern = 'gsub',path = '.',value=TRUE,recursive = T)

$`./R/makeImport.R`
[1] "  names(s1)=gsub('\\\\\\\\b','',names(s1))"                                                           
[2] "      y=gsub('[\\\\\",(]','',unlist(stringr::str_extract_all(x,pattern=paste0(nm,'(.*?)\\\\('))))"    
[3] "    out=gsub('\\\\$.*','',out)"                                                                       
[4] "      ret=paste0(\"#' @importFrom \",gsub('::',' ',nm),gsub(nm,'',paste(unique(out),collapse = ' ')))"
[5] "        if(length(out)>=cut) ret=paste0(\"#' @import \",gsub('::','',nm))"                            
[6] "    pkgN=gsub(':.*','',pkg)"                                                                          
[7] "    ret=paste0('importFrom(',gsub('::',',',pkg),')')"                                                 
[8] "          sprintf('import(%s)',unique(gsub(':.*','',x))) "                                            
[9] "          paste0('importFrom(',gsub('::',',',grep(x,pkg,value=T)),')')"                               
[10] "    ret=unique(gsub('::(.*?)$','',unlist(pkg)))"                                                      

$`./R/runStanGit.R`
[1] "    #strsplit(gsub('\\\\r','',RCurl::getURL(code.loc)[1]),'\\\\n')[[1]]"                                                                            
[2] "  keep.files=gsub(' ','',unlist(lapply(strsplit(r.code[which(grepl('stan\\\\(',r.code))],'<-'),'[',1)))"                                            
[3] "  r.code=gsub('print','#print',r.code)"                                                                                                             
[4] "  r.code=gsub('pairs','#pairs',r.code)  "                                                                                                           
[5] "      r.code[stan.find[i]]=gsub(substr(r.code[stan.find[i]],x[1],x[2]),strip.path(substr(r.code[stan.find[i]],x[1]+1,x[2]-1)),r.code[stan.find[i]])"

$`./R/setwdURL.R`
[1] "  urlPath=gsub(r.file,'',path)"                         
[2] "  url.loc=gsub('master(.*?)$','',path)"                 
[3] "  dat.loc=gsub(paste0(url.loc,'master/'),'',urlPath)"   
[4] "  r.script=gsub('\\\\s+','',r.script)"                  
[5] "  str.change=basename(gsub('[\\\\\"]','',str.old))"     
[6] "      file.name=gsub(' ','',strsplit(x,'<-|=')[[1]][1])"
[7] "      gsub(str.old,file.name,x) 
```

### Github

```r
  grepr(pattern = 'importFrom',path = c('yonicd/ciderhouse','R'),value=TRUE)
$`R/grepr.R`
[1] "#' grepr(pattern = 'importFrom',path = c(repo='yonicd/ciderhouse',subdir='R'),value=TRUE)"

$`R/importAddin.R`
[1] "#' @importFrom rstudioapi getActiveDocumentContext"

$`R/ls_github.R`
[1] "#' @importFrom httr content GET"

$`R/makeImport.R`
[1] "#' @param cut integer number of functions to write as importFrom until switches to import"            
[2] "#' @importFrom stringr str_extract_all"                                                               
[3] "#' @importFrom utils installed.packages"                                                              
[4] "      ret=paste0(\"#' @importFrom \",gsub('::',' ',nm),gsub(nm,'',paste(unique(out),collapse = ' ')))"
[5] "    ret=paste0('importFrom(',gsub('::',',',pkg),')')"                                                 
[6] "          paste0('importFrom(',gsub('::',',',grep(x,pkg,value=T)),')')"                               

$`R/runStanGit.R`
[1] "#' @importFrom RCurl getURL"
```

## ls_github
Return raw paths to files in github directory. 

```r
#head of master branch
ls_github(repo='yonicd/ciderhouse')
[1] "https://raw.githubusercontent.com/yonicd/ciderhouse/master/DESCRIPTION"
[2] "https://raw.githubusercontent.com/yonicd/ciderhouse/master/NAMESPACE"  
[3] "https://raw.githubusercontent.com/yonicd/ciderhouse/master/README.md"  
[4] "https://raw.githubusercontent.com/yonicd/ciderhouse/master/misc.Rproj" 

#subdirectory R
ls_github(repo='yonicd/ciderhouse',subdir='R')
[1] "https://raw.githubusercontent.com/yonicd/ciderhouse/master/R/grepr.R"        
[2] "https://raw.githubusercontent.com/yonicd/ciderhouse/master/R/importAddin.R"    
[3] "https://raw.githubusercontent.com/yonicd/ciderhouse/master/R/listFilesGithub.R"
[4] "https://raw.githubusercontent.com/yonicd/ciderhouse/master/R/makeImport.R"     
[5] "https://raw.githubusercontent.com/yonicd/ciderhouse/master/R/runStanGit.R"     
[6] "https://raw.githubusercontent.com/yonicd/ciderhouse/master/R/setwdURL.R"

#recursive call
ls_github(repo='yonicd/ciderhouse',subdir='inst',recursive = FALSE) #returns nothing
> character(0)
ls_github(repo='yonicd/ciderhouse',subdir='inst',recursive = TRUE)
[1] "https://raw.githubusercontent.com/yonicd/ciderhouse/master/inst/rstudio/addins.dcf"
```

### Using ls_github with readLines

```r
readLines(ls_github(repo='yonicd/ciderhouse',subdir='man')[2])
 [1] "% Generated by roxygen2: do not edit by hand"                            
 [2] "% Please edit documentation in R/ls_github.R"                            
 [3] "\\name{ls_github}"                                                       
 [4] "\\alias{ls_github}"                                                      
 [5] "\\title{List files of a github repository subdirectory}"                 
 [6] "\\usage{"                                                                
 [7] "ls_github(repo, subdir = NULL, recursive = FALSE)"                       
 [8] "}"                                                                       
 [9] "\\arguments{"                                                            
[10] "\\item{repo}{character Repository address in the format username/repo}"  
[11] ""                                                                        
[12] "\\item{subdir}{subdirectory within repo that contains the files to list}"
[13] "}"                                                                       
[14] "\\description{"                                                          
[15] "List full raw paths to files in a github repository subdirectory"        
[16] "}"                                                                       
[17] "\\examples{"                                                             
[18] "l=ls_github('tidyverse/ggplot2')"                                        
[19] "l"                                                                       
[20] "#geom-boxplot.r"                                                         
[21] "readLines(l[42])"                                                        
[22] "}"  
```
