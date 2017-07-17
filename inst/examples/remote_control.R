library(shiny)
server <- function(input, output,session) {
  
  network <- reactiveValues()
  
  observeEvent(input$tree_update,{
    current_selection<-input$tree_update$.current_tree
    if(!is.null(current_selection)) network$tree <- jsonlite::fromJSON(current_selection)
  })
  
  observeEvent(network$tree,{
    output$results <- renderPrint({
      str.out=''
      if(length(network$tree)>0) str.out=network$tree
      return(str.out)
    })    
  })
  
  observeEvent(c(input$f1,input$queryRepo),{
    output$results <- renderPrint({
      str.out=''
      return(str.out)
    })    
  })
  
  remote.current<-eventReactive(input$localPath,{
    root<-isolate(input$localPath)
    vcs::find.remote(root) 
  })

  observeEvent(c(remote.current(),input$dirType,input$queryRepo),{
    
    output$tree <- switch(input$dirType,
                          Local={
                            jsTree::renderJsTree({
                              vcs_type='git'
                              rc<-remote.current()
                              if(input$f1%in%rc$dir){
                                vcs_type=rc[rc$dir==input$f1,'vcs']
                              } 
                              obj=vcs::ls_remote(input$f1,vcs = vcs_type)
                              opts=NULL
                              if(dir.exists(input$f1)) opts=list(nodestate=vcs::diff_head(input$f1,vcs=vcs_type,show = FALSE))
                              vcs::navigate_remote(input$f1,vcs=vcs_type,output.opts = opts)
                            })
                            },
                            Remote={
                              jsTree::renderJsTree({
                                if(nchar(isolate(input$remotePath))>0){
                                isolate(vcs::navigate_remote(input$remotePath,vcs=input$vcs))
                                }
                                })
                            })
    
  })
  
  observeEvent(c(input$queryRepo,input$dirType),{
    
  })
  
  
  observeEvent(c(input$createRepo),{
    if(!is.null(input$f1)){
      f2<-gsub(sprintf('%s/%s',input$f1,'master'),'',network$tree)
      if(length(f2)>0){
        if(dir.exists(sprintf('%s/.git',input$dirOutput))){
          vcs::sparse_checkout(repo_url = sprintf('https://github.com/%s.git',input$f1),
                               queries = f2,
                               dest.dir = input$dirOutput,
                               create = FALSE,
                               append = FALSE)
        }else{
          vcs::sparse_checkout(repo_url = sprintf('https://github.com/%s.git',input$f1),
                               queries = f2,
                               dest.dir = input$dirOutput,
                               create = TRUE)
        }
      } 
    }
  })
  
  output$chosen=renderUI({
    verbatimTextOutput(outputId = "results")
  })
  
  output$localDir<-renderUI({
    rc<-remote.current()
    selectInput(inputId = 'f1',
                label = 'Choose Local Repository',
                choices = rc$dir,
                selected = rc$dir[1])
  })
  
}

ui <- fluidPage(
  sidebarLayout(sidebarPanel = 
  sidebarPanel(
    radioButtons(inputId = 'dirType',label = 'Source',choices = c('Local','Remote'),selected = 'Local',inline = TRUE),
    conditionalPanel("input.dirType=='Local'",{
      wellPanel(
        textInput(inputId = 'localPath',label = 'Root Directory',value = '/data'),
        uiOutput('localDir'))  
    }),
    conditionalPanel("input.dirType=='Remote'",{
      wellPanel(
        textInput(inputId='remotePath',label = 'Remote Repository'),
        radioButtons(inputId = 'vcs',label = 'Remote Version Control Sytem',choices = c('git','github','bitbucket','svn'),selected = 'github',inline = TRUE),
        actionButton('queryRepo','Query Remote Repo'),
        textInput('dirOutput','',placeholder = 'Path of Checkout'),
        actionButton('createRepo','Create Sparse Checkout'))
    }),
    uiOutput('chosen')
  ),
                mainPanel = mainPanel(
                  
                  jsTree::jsTreeOutput(outputId = 'tree',width = '100%',height = 800)                  
                ))
  

)

shinyApp(ui = ui, server = server)

