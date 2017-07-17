#' @title vcs UI
#' @description Shiny gadget for controlling, navigating and maintaining version control directories on local and remote repositories
#' @param rootpath root path to look for version control directories, Default: getwd()
#' @return nothing
#' @details Version control systems that can be accessed are git, github, bitbucket and svn. Throught this gadget you can do full and sparse checkout and update current vcs directory. 
#' @examples 
#' \dontrun{
#' if(interactive()){
#'  alexa(getwd())
#'  }
#' }
#' @seealso 
#'  \code{\link[jsonlite]{fromJSON}}

#'  \code{\link[jsTree]{jsTreeOutput}},\code{\link[jsTree]{renderJsTree}}

#'  \code{\link[miniUI]{miniPage}},\code{\link[miniUI]{gadgetTitleBar}},\code{\link[miniUI]{miniTitleBarButton}},\code{\link[miniUI]{miniContentPanel}}

#'  \code{\link[shiny]{sidebarLayout}},\code{\link[shiny]{sidebarPanel}},\code{\link[shiny]{radioButtons}},\code{\link[shiny]{conditionalPanel}},\code{\link[shiny]{wellPanel}},\code{\link[shiny]{textInput}},\code{\link[shiny]{uiOutput}},\code{\link[shiny]{actionButton}},\code{\link[shiny]{reactiveValues}},\code{\link[shiny]{observeEvent}},\code{\link[shiny]{renderPrint}},\code{\link[shiny]{eventReactive}},\code{\link[shiny]{isolate}},\code{\link[shiny]{renderUI}},\code{\link[shiny]{verbatimTextOutput}},\code{\link[shiny]{selectInput}},\code{\link[shiny]{stopApp}},\code{\link[shiny]{runGadget}},\code{\link[shiny]{paneViewer}}

#'  \code{\link[vcs]{find.remote}},\code{\link[vcs]{diff_head}},\code{\link[vcs]{navigate_remote}},\code{\link[vcs]{sparse_checkout}}
#' @rdname alexa
#' @export 
#' @importFrom jsonlite fromJSON
#' @importFrom jsTree jsTreeOutput renderJsTree
#' @importFrom miniUI miniPage gadgetTitleBar miniTitleBarButton miniContentPanel
#' @importFrom shiny sidebarLayout sidebarPanel radioButtons conditionalPanel wellPanel textInput uiOutput actionButton reactiveValues observeEvent renderPrint eventReactive isolate renderUI verbatimTextOutput selectInput stopApp runGadget paneViewer
alexa<-function(rootpath=getwd()){
ui <- miniUI::miniPage(
  miniUI::gadgetTitleBar('Alexa',
                         left = miniUI::miniTitleBarButton(inputId = "qt","Quit",primary = TRUE),
                         right=NULL),
  miniUI::miniContentPanel(
    shiny::sidebarLayout(sidebarPanel = 
                           shiny::sidebarPanel(
                             shiny::radioButtons(inputId = 'dirType',label = 'Source',choices = c('Local','Remote'),selected = 'Local',inline = TRUE),
                             shiny::conditionalPanel("input.dirType=='Local'",{
                               shiny::wellPanel(
                                 shiny::textInput(inputId = 'localPath',label = 'Root Directory',placeholder = 'Enter Root Directory',value = rootpath),
                                 shiny::uiOutput('localDir'))  
      }),
      shiny::conditionalPanel("input.dirType=='Remote'",{
        shiny::wellPanel(
          shiny::textInput(inputId='remotePath',label = 'Remote Repository'),
          shiny::radioButtons(inputId = 'vcs',label = 'Remote Version Control Sytem',choices = c('git','github','bitbucket','svn'),selected = 'github',inline = TRUE),
          shiny::actionButton('queryRepo','Query Remote Repo'),
          shiny::textInput('dirOutput','',placeholder = 'Path of Checkout'),
          shiny::actionButton('createRepo','Create Sparse Checkout'))
      }),
      shiny::uiOutput('chosen')
    ),
    mainPanel = shiny::mainPanel(
      jsTree::jsTreeOutput(outputId = 'tree',width = '100%',height = 800)                  
    ))
  

))

server <- function(input, output,session) {
  
  network <- shiny::reactiveValues()
  
  shiny::observeEvent(input$tree_update,{
    current_selection<-input$tree_update$.current_tree
    if(!is.null(current_selection)) network$tree <- jsonlite::fromJSON(current_selection)
  })
  
  shiny::observeEvent(network$tree,{
    output$results <- shiny::renderPrint({
      str.out=''
      if(length(network$tree)>0) str.out=network$tree
      return(str.out)
    })    
  })
  
  shiny::observeEvent(c(input$f1,input$queryRepo),{
    output$results <- shiny::renderPrint({
      str.out=''
      return(str.out)
    })    
  })
  
  remote.current<-shiny::eventReactive(input$localPath,{
    root<-shiny::isolate(input$localPath)
    find.remote(root) 
  })
  
  shiny::observeEvent(c(remote.current(),input$dirType,input$queryRepo),{
    
    output$tree <- switch(input$dirType,
                          Local={
                            jsTree::renderJsTree({
                              vcs_type='git'
                              rc<-remote.current()
                              vcs_type=rc[rc$dir==input$f1,'vcs']
                              opts=NULL
                              if(!is.null(input$f1)){
                              if(dir.exists(input$f1)){
                                opts=list(nodestate=diff_head(input$f1,vcs=vcs_type,show = FALSE))
                              }
                              navigate_remote(input$f1,vcs=vcs_type,output.opts = opts)
                              }
                            })
                          },
                          Remote={
                            jsTree::renderJsTree({
                              if(nchar(isolate(input$remotePath))>0){
                                shiny::isolate(navigate_remote(input$remotePath,vcs=input$vcs))
                              }
                            })
                          })
    
  })
  
  shiny::observeEvent(c(input$queryRepo,input$dirType),{
    
  })
  
  
  shiny::observeEvent(c(input$createRepo),{
    if(!is.null(input$f1)){
      f2<-gsub(sprintf('%s/%s',input$f1,'master'),'',network$tree)
      if(length(f2)>0){
        if(dir.exists(sprintf('%s/.git',input$dirOutput))){
          sparse_checkout(repo_url = sprintf('https://github.com/%s.git',input$f1),
                               queries = f2,
                               dest.dir = input$dirOutput,
                               create = FALSE,
                               append = FALSE)
        }else{
          sparse_checkout(repo_url = sprintf('https://github.com/%s.git',input$f1),
                               queries = f2,
                               dest.dir = input$dirOutput,
                               create = TRUE)
        }
      } 
    }
  })
  
  output$chosen=shiny::renderUI({
    shiny::verbatimTextOutput(outputId = "results")
  })
  
  output$localDir<-shiny::renderUI({
    rc<-remote.current()
    shiny::selectInput(inputId = 'f1',
                       label = 'Choose Local Repository',
                       choices = rc$dir,
                       selected = rc$dir[1])
  })
  
  shiny::observeEvent(input$qt,{shiny::stopApp()})
}

shiny::runGadget(ui, server, viewer = shiny::paneViewer(minHeight = 450))
}