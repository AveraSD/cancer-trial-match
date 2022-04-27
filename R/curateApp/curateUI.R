## ui side utils

##### Panel 1: NCT ID + query trial
input_form <- fluidRow(
  
  # note: columns should add upto 12
  wellPanel(
    fluidRow(
      column(4, 
             textInput("info_NCT", 
                       "NCT Trial ID")),
      column(4, 
             selectInput("info_jit", 
                         "Just in time trial offered by", 
                         c("Tempus", "Caris", "Optimal", "Not JIT"))),
      column(4, 
             textInput("info_trial_name", 
                       "Name of the trial")),
      br()
    ),
    
    actionButton("submit", "SEARCH")
  ),
  uiOutput("firsthalf"),
  
  # Query Info display
  br(),
  br(),
  br(),
  br(),
  
  h5(strong("General Information")),
  wellPanel(
    reactableOutput("responses")
  ),
  
  br(),
  br(),
  
  # Arm info Display
  h5(strong("Arm Information")),
  wellPanel(
    #br(),
    reactableOutput("armsOnly")
  )
)

##### Panel 2: Disease
dise_form <- fluidPage(
  fluidRow(
    uiOutput("secondhalf")
  )
)

secondhalfUI <- fluidPage(
  fluidRow(
    shinyjs::useShinyjs(),
    br(),
    
    div(style = "margin-top: 20px;"),
    
    column(2, actionButton("disDis", "Move to Biomarker")),
    br(),
    
    div(style = "margin-top: 20px;"),

    ### Hold status for the trial at the site 
    br(),
    column(3, selectInput("trHold", 
                          "Please choose the trial status for the site:",
                          choices = c("open", "on hold", "closed"))),
    column(9, textInput("disSum", 
                        "Please enter an overall disease summary")),
    br(),
    
    ### Text Summary 
    div(style = "margin-top: 20px;"),
    br(),
    
    ### Individual disease code + selection
    h5("Please choose each disease you wish to record"), 
    h6(em("Choose levels according to Oncotree with as much detail as possible")),
    br(),
    
    radioGroupButtons(
      inputId = "certir",
      choices = c("include", 
                  "exclude"),
      justified = F
    ),
    
    wellPanel(
      fluidRow(
        column(9,
               #style = "display: inline-block;",
               selectInput("dise", "Tissue Site", choices = unique(oncotree$level_1), multiple = FALSE)
        ),
        column(4,
               selectInput("lev2", "Level2", choices = "", selected = "", multiple = FALSE)
        ),
        column(4,
               #style = "display: inline-block;",
               selectInput("lev3", "Level3", choices = "", selected = "", multiple = FALSE)
        ),
        column(4,
               selectInput("lev4", "Level4", choices = "", selected = "", multiple = FALSE)
        ),
        column(4,
               selectInput("lev5", "Level5", choices = "", selected = "")
        ),
        column(4,
               selectInput("lev6", "Level6", choices = "", selected = "")
        ),
        column(4,
               selectInput("lev7", "Level7", choices = "", selected = "")
        )
      ),
      
      actionButton(inputId = "addDis",label = "ADD")
    ),
    
    # display chosen disease
    br(),
    div(style = "margin-top: 40px;"),
    h5("TABLE: Cohort level biomarker information"),
    div(style = "margin-top: 20px;"),
    
    DTOutput(outputId = "dt_dise",
                 width = "100%"),
    br(),
    br(),
    div(style = "margin-top: 50px;")
  )
)


##### Panel 3: Biomarker
biom_form <- fluidPage(
  uiOutput("bioDis")
  )

biom_display <- fluidPage(
  div(
    class = "container",
    br(),
    div(
      shiny::actionButton(
        inputId = "bioMrk",
        label = "Move to Documents",
        class = "btn-secondary"),
      br(),
    ), 
  ),
   
    # add arm information
    div(style = "margin-top: 20px;"),
    h5("Please select a cohort arm to add line of therapy and arm recruitement status"),
    
    div(
      class = "container",
      style = "margin-top: 10px;",
      DT::dataTableOutput(outputId = "dt_table_arm", 
                          width = "100%")
    ),
    
    div(style = "margin-top: 60px;"),
    h5("TABLE: Cohort level information"),

    div(
      class = "container",
      style = "margin-top: 10px;",
      DT::DTOutput(outputId = "dt_table_arm_display",
                   width = "100%"),
      br()
    ),

    # add biomarker
    div(
      style = "margin-top: 40px;",
      shiny::actionButton(
        inputId = "add_allBio",
        label = "Add common biomarker",
        icon = shiny::icon("plus"),
        class = "btn-primary")
    ),

  div(style = "margin-top: 20px;"),
  h5("Please select a cohort arm to add corresponding biomarker(s)"),

  div(
    class = "container",
    style = "margin-top: 10px;",
    DT::dataTableOutput(outputId = "dt_table",
                        width = "100%")
  ),

  div(style = "margin-top: 60px;"),
  h5("TABLE: Cohort level biomarker information"),

  div(
    class = "container",
    style = "margin-top: 10px;",
    DT::DTOutput(outputId = "dt_biomark",
                 width = "100%"),
    br()
  ),
 
  shiny::includeScript(here("R","curateApp","script.js"))
  
)

##### Panel 4: Documentation
doc_form <- fluidPage(
  uiOutput("DisDoc")
)

docuOut <- fluidPage(
  
  div(style = "margin-top: 40px;"),
  actionButton("move_brow", "Move to Browser"),
  br(),
  br(),

  # add link to trial documentation
  textInput(inputId = "doc", 
            label = "Please add link to (site) trial documentation"),
  br(),
  h5("Link added: "), 
  textOutput("doc_link")
)

##### Panel 5: View Trial
# display for the browser tab
dis_form <- fluidPage(
  uiOutput("DisBrow")
)


browserOut <- fluidPage(
  br(),
  column(2, actionButton("confirm1", "CONFIRM")),
  
 # query specific populate 
 reactableOutput("displayBio")
)
