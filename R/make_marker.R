make_marker <- function(pattern,x){

  rstudioapi::callFun("sourceMarkers",
                    name = pattern,
                    markers = unlist(lapply(names(x),function(nx){
                      
                      if(!httr::http_error(nx)){
                        
                        new_nx <- file.path(tempdir(),basename(nx))
                        
                        cat(
                          readLines(nx),
                          file = new_nx,
                          sep='\n'
                          )
                      
                      }
                        
                      lapply(x[[nx]],function(xx){
                        r <- as.numeric(gsub('\\D','',gsub(':(.*?)$','',xx)))
                        l <- gsub('^(.*?):','',xx)
                        list(type='info',file=new_nx,line=r,column=1,message=l) 
                      })
                    }),recursive = FALSE),
                    basePath = getwd(),
                    autoSelect = "none")
}