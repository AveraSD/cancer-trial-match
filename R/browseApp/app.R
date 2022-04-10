library(shiny)
library(reactable)
library(reactablefmtr)
library(bslib)
library(readxl)
library(here)
library(mongolite)
library(jsonlite)
library(httr)
library(glue)
library(tidyverse)


# source global file
source(here("R", "browseApp", "global.R")) 


##### UI ######
ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ), 
  navbarPage(
    title = "TrialMatch", 
    
    # BROWSE
    tabPanel("Browse",
             actionButton("collapse_btn_browse", "Collapse All"), 
             reactableOutput("browsetable")),
    
    # MATCH
    tabPanel("Match", 
             reactableOutput("matchtable")),
    
    theme = bs_theme(version = 5, 
                     bootswatch = "cosmo",
                     primary = "#246725")
    
  )
)



##### SERVER ######
server <- function(input, output,session) {
  
  ##### BROWSE ########
  # main display table for BROWSE
  output$browsetable <- renderReactable({
     display_browse_db # from panel_browse.R
  })

  # collapse button
  observeEvent(input$collapse_btn_browse, {
     updateReactable("browsetable", expanded = FALSE)
  })
  
  ##### MATCH ########
  # main display table for match
  output$matchtable <- renderReactable({
     display_match_gen # from panel_match.R
  })
  
}


##### APP ######
shinyApp(ui, server)
