#' @title Collapse lines of piped calls to single lines 
#' @description When parsing lines of code this will collapse piped lines into a single one
#' @param pattern character, symbols to collapse on Default: '\%[[:punct:]]\%|[+]'
#' @param x character, lines of script to run on
#' @param comment_tokens character, tokens to comment out collapsed lines e.g. 'plot', 'print' Default: NULL
#' @return character
#' @examples 
#' \dontrun{
#'  str1<-'iris%>%
#'  dplyr::mutate(Sepal.Length*2)%>%
#'  plot()'
#'  x<-strsplit(str1,'\\n')[[1]]
#'  collapse_pipe(x=x,comment_tokens = 'plot')
#' }
#' @rdname collapse_pipe
#' @export 

collapse_pipe=function(pattern='%[[:punct:]]%|[+]',x,comment_tokens=NULL){
ind=grep(pattern,x)
if(length(ind)==0) return(NULL)
x1<-cbind(s=cumsum(c(2,tail(ind,-1)-head(ind,-1))>1),ind)
x2<-split(x1[,-1],x1[,1])
for(i in 1:length(x2)){
  if(length(x2[[i]])>1){
    chkInd=c(x2[[i]],tail(x2[[i]],1)+1)
    x[head(x2[[i]],1)]=paste0(x[chkInd],collapse = '')
    x[head(x2[[i]],1)]=gsub('NA$','',x[head(x2[[i]],1)])
    x[tail(chkInd,-1)]=''
    
  }
  x[x2[[i]][1]]=gsub(' ','',x[x2[[i]][1]])
  if(!is.null(comment_tokens)){
    if(grepl(paste0(sprintf('\\b%s\\b',comment_tokens),collapse='|'),x[x2[[i]][1]])){
      x[x2[[i]][1]]=sprintf('#%s',x[x2[[i]][1]])
    } 
  }
}
x[nchar(x)>0]

}
