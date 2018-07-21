#' @export
ghe_raw_token <- function(path,file,PAT = Sys.getenv('GHE_PAT')){
  
  dl_git <- sprintf('https://ghe.metrumrg.com/api/v3/repos/%s/contents/%s',path,file)
  
  x1 <- httr::GET(dl_git,
                  httr::add_headers(
                    Authorization = sprintf('token %s',PAT)
                  )
  )
  
  gsub('^(.*?)\\?','',httr::content(x1)$download_url) 
}