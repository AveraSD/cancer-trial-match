library(shiny)
library(reactable)
library(bslib)
library(readxl)
library(here)
library(mongolite)
library(jsonlite)
library(httr)
library(glue)
library(tidyverse)
library(shinyWidgets)
library(shinyFiles)
library(DT)
library(shinyjs)
library(config)
library(data.table)

# NCT03037385

# source necessary files
source(here("R", "curateApp", "curateGlobal.R")) 
source(here("R", "curateApp", "queryNCT.R"))
#source(here("R", "curateApp", "nct_to_json.R"))
source(here("R", "curateApp", "curateUI.R"))
source(here("R", "curateApp", "curateServer.R"))
source(here("R", "curateApp", "add_trials.R"))

##### UI ######
ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "style.css")
  ), 
  useShinyjs(),
  navbarPage(
    title = "TrialCurate", 
    
    tabPanel("Add Trial",
    
    tabsetPanel(
      id = 'inNav',
      
      # Curate Query
      tabPanel("NCT ID", 
               input_form),
      
      # Disease 
      tabPanel("Disease",
               dise_form),
      
      # Biomarker 
      tabPanel("Biomarker",
               biom_form),
      
      # Documentation
      tabPanel("Documents",
               doc_form),
      
      # View trial before submitting to database
      tabPanel("View Trial",
               dis_form )
      
    )),
    # tabPanel(
    #   "Browse Trials"
    # ),
  
    theme = bs_theme(version = 5, 
                     bootswatch = "cosmo",
                     primary = "#246725")
    
  )
)


# Define server logic 
server <- function(input, output, session) {
  
  #updateSelectizeInput(session, 'var', choices = allVar, server = TRUE)
  #updateSelectizeInput(session, 'gene1', choices = allgenes, server = TRUE)
  ##### Panel 1: NCT ID + query trial
  
  # action after clicking submit
  displatAPI <- eventReactive(input$submit, {
    output$firsthalf <- renderUI({firsthalfUI })
    
    # query NCT
    outAPI <- queryNct(input$info_NCT)
  })
  
  # Display the Query Information Table
  output$responses <- renderReactable({
    info <- displatAPI() %>% 
      select(!(arm)) %>% 
      select(!(criteria)) %>% 
      select(!(link))
    infoquery <- tibble(Information = row.names(t(info)) %>% 
                         str_replace_all("_", " ") %>% 
                         str_to_title(), 
                       Details = t(info))

    reactable(infoquery, 
              resizable = TRUE, 
              rownames = FALSE,
              columns = list(
                Information = colDef(width = 200),
                Details = colDef(minWidth = 800)
              ),
              bordered = TRUE, 
              #striped = TRUE, 
              pagination = FALSE)  
  })
  
  # Display the Arm table 
  output$armsOnly <- renderReactable({
    disAd$armDf <- displatAPI()[[11]][[1]]
    armquery <- displatAPI()[[11]][[1]]
    reactable(armquery, 
              compact = TRUE,
              bordered = TRUE, 
              #striped = TRUE, 
              columns = list(
                cohortlabel = colDef(name = "Cohort Label"),
                drug = colDef(name = "Drug(s)"),
                arm_type = colDef(name = "Arm Type")
              ))
  })
  
  
  ##### Panel 2: Disease
  # TABLE 1: Open the Disease Table and display the UI on Next Hit 
  observeEvent(input$next1,{
    updateTabsetPanel(session, "inNav",selected = "Disease")
    output$secondhalf = renderUI({
      secondhalfUI
    })
  })
  
  # disease dropdown selection
  observeEvent(
    input$dise,
    updateSelectInput(session, "lev2", "Level2", 
                      #choices = oncotree$level_2[oncotree$level_1==input$dise]))
  choices = oncotree_addrows1$level_2[oncotree_addrows1$level_1==input$dise]))
  
  observeEvent(
    input$lev2,
    updateSelectInput(session, "lev3", "Level3",
                      choices = oncotree_addrows1$level_3[oncotree_addrows1$level_2==input$lev2 & oncotree_addrows1$level_1==input$dise]))
  
  observeEvent(
    input$lev3,
    updateSelectInput(session, "lev4", "Level4",
                      choices = oncotree_addrows1$level_4[oncotree_addrows1$level_3==input$lev3 & oncotree_addrows1$level_2==input$lev2 & oncotree_addrows1$level_1==input$dise]))
  
  observeEvent(
    input$lev4,
    updateSelectInput(session, "lev5", "Level5",
                      choices = oncotree_addrows1$level_5[oncotree_addrows1$level_4==input$lev4 & oncotree_addrows1$level_3==input$lev3 & oncotree_addrows1$level_2==input$lev2 & oncotree_addrows1$level_1==input$dise]))
  
  
  observeEvent(
    input$lev5,
    updateSelectInput(session, "lev6", "Level6", 
                      choices = oncotree_addrows1$level_6[oncotree_addrows1$level_5==input$lev5 & oncotree_addrows1$level_4==input$lev4 & oncotree_addrows1$level_3==input$lev3 & oncotree_addrows1$level_2==input$lev2 & oncotree_addrows1$level_1==input$dise]))
  
  observeEvent(
    input$lev6,
    updateSelectInput(session, "lev7", "Level7", 
                      choices = oncotree_addrows1$level_7[oncotree_addrows1$level_6==input$lev6 & oncotree_addrows1$level_5==input$lev5 & oncotree_addrows1$level_4==input$lev4 & oncotree_addrows1$level_3==input$lev3 & oncotree_addrows1$level_2==input$lev2 & oncotree_addrows1$level_1==input$dise]))
  
  
  # event for clearing the disease table 
  
  observeEvent(input$clr_Dis,{
    disAd$indisAd = tibble()
  })
  
  
  # TABLE A: Display the selected Disease and selection 
  observeEvent(input$addDis, {
    allInputDise <- c(input$dise,input$lev2,input$lev3,input$lev4,input$lev5,input$lev6,input$lev7)
    lastInput <- allInputDise[allInputDise != "NA"]
    lenlast <- length(lastInput)
    
    addDisBtn <- tibble(code = lastInput[lenlast], 
                        selection = input$certir)
    
    disAd$indisAd <- disAd$indisAd %>% bind_rows(addDisBtn)
    output$dt_dise <- renderDT({
      datatable(disAd$indisAd, 
                filter = 'none', 
                selection = 'none', 
                options = list(dom = 't'))
    })
  })
  
  
  ##### Panel 3: Biomarker
  # Open the Biomarker Tab and display the UI on Move to Biomarker
  observeEvent(input$disDis,{
    updateTabsetPanel(session, "inNav", selected = "Biomarker")
    output$bioDis <- renderUI({
      biom_display
    })
  })
  
  ## ADD LoT and Status
  # observe and open modal to add common LoT and Status for all cohort arms
  observeEvent(input$add_allArmLotStatus,{
    output$TEXTA_arminfo <- renderText({
      "Enter Line of Therapy and recruitment status"
    })
    modal_arminfo(lineTx = "", armStatus = "")
    disAd$add_or_edit_arminfo <- 1
    
  })
  
  # TABLE 1: table shows cohort arms to enter line of therapy and arm status
  output$dt_table_arm <- renderDataTable({
    butns_arminfo <- create_btns_arminfo(nrow(disAd$armDf))
    arm_info <- disAd$armDf %>% 
      rownames_to_column(var = "ArmID") %>% 
      bind_cols(tibble("armadd" = butns_arminfo)) 
    datatable(arm_info, 
              escape = F,
              selection ='single',
              rownames = FALSE,
              colnames = c('Arm #' = 'ArmID',
                           'Cohort(s)' = 'cohortlabel',
                           'Drugs(s)' = 'drug',
                           'Arm Type' = 'arm_type',
                           'Add Arm Info' = 'armadd'),
              options = list(processing = FALSE, 
                             dom = 't',pageLength = 15)
    )
  })
  
  
  
  # event for clearing arm selection 
  observeEvent(input$clr_Arm,{
    disAd$armDfInfo = tibble()
  })
  
  
  # TABLE A: when specific row is selected - add LoT & Status
  observeEvent(input$current_id, {
    if (!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "arminfo")) {
      selRow <- disAd$armDf[input$dt_table_arm_rows_selected, ]
      cohotLb <- selRow[["cohortlabel"]]
      output$TEXTA_arminfo <- renderText({
        cohotLb
      })
      modal_arminfo(lineTx = "", armStatus = "")
    }
  })
  
  observeEvent(input$final_edit_arminfo, {
    shiny::removeModal()
    armdf <- disAd$armDf %>% rownames_to_column(var = "ArmID")
    selarm <- armdf[input$dt_table_arm_rows_selected, ]
    cohortID <- selarm[["ArmID"]]
    cohort <- selarm[["cohortlabel"]]
    dt_row <- tibble(
      armID = cohortID,
      cohortlabel = cohort,
      lineTx = input$lineTx,
      armStatus = input$armStatus
    )
    disAd$armDfInfo <- disAd$armDfInfo %>% bind_rows(dt_row) %>% distinct()
    
   
    
   #disAd$armDfInfo <- inner_join(disAd$armDfInfo, dt_row, by = "cohortlabel")
    output$dt_table_arm_display <- renderDT({
      datatable(disAd$armDfInfo, 
                rownames = F,
                options = list(dom = 't'))
    })
    
    proxy <- dataTableProxy("dt_table_arm_display")
    proxy %>% selectRows(NULL)
  })
  
  # TABLE A: when (+Add common LoT & Status) is chosen
  observeEvent(input$final_edit_arminfo, {
    shiny::req(disAd$add_or_edit_arminfo == 1)
    shiny::removeModal()
    armdf <- disAd$armDf %>% rownames_to_column(var = "ArmID")
    nArm <- nrow(armdf)
    for(e in 1:nArm){
      dt_rowall <- tibble(
        armID = as.character(e),
        lineTx = input$lineTx,
        armStatus = input$armStatus,
        cohortlabel = as.character(armdf[e,2]))
      
      disAd$armDfInfo <- disAd$armDfInfo %>% bind_rows(dt_rowall) %>% distinct()
    }
    output$dt_table_arm_display <- renderDT({
      datatable(disAd$armDfInfo, 
                rownames = FALSE,
                colnames = c('Arm #' = 'armID',
                             'Cohort' = 'cohortlabel'),
                options = list(dom = 't',pageLength = 15))
    })
    
    proxy <- dataTableProxy("dt_table_arm_display")
    proxy %>% selectRows(NULL)
  })
  
  ## ADD biomarker
  # observe and open modal to add common biomarker to cohort arms
  observeEvent(input$add_allBio,{
    output$TEXTA <- renderText({
      "Enter biomarkers common to all cohort arms"
    })
    modal_biomarker(gene1 = "", typ = "", var = "", selec = "", func = "")
    disAd$add_or_edit <- 1
    
  })
  
  # TABLE 2: table shows cohort arms to enter corresponding biomarkers
  output$dt_table <- renderDataTable({
    butns_biomarker <- create_btns_biomarker(nrow(disAd$armDf))
    armterm <- disAd$armDf %>% 
      rownames_to_column(var = "ArmID") %>% 
      bind_cols(tibble("Buttons" = butns_biomarker))
    datatable(armterm, 
              escape = F,
              selection ='single',
              rownames = FALSE,
              colnames = c('Arm #' = 'ArmID',
                           'Cohort(s)' = 'cohortlabel',
                           'Drugs(s)' = 'drug',
                           'Arm Type' = 'arm_type',
                           'Add Biomarker' = 'Buttons'),
              options = list(processing = FALSE, 
                             dom = 't',pageLength = 15)
    )
  })
  
  #event for clearing Biomarker arm selection 
  observeEvent(input$clr_Bio,{
    disAd$dfAdd = tibble()
  })
  
  # TABLE B: when specific row is selected - add biomarker
  observeEvent(input$current_id,{
    if (!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "edit")) {
      selRow <- disAd$armDfInfo[input$dt_table_rows_selected,]
      cohotLb <- selRow[["cohortlabel"]]
      output$TEXTA <- renderText({
        cohotLb
      })
      modal_biomarker(gene1 = "", typ = "", var = "", selec = "", func = "")
    }
  })
  
  
  
  
  observeEvent(input$final_edit, {
    shiny::removeModal()
    armdf <- disAd$armDfInfo 
    #selarm <- armdf[input$dt_table_rows_selected,] # change to select by cohortlabel
    selarm <- armdf %>% filter(cohortlabel %in% armdf[input$dt_table_rows_selected, "cohortlabel"])
    armID <- selarm[["armID"]]
    cohortlabel <- selarm[["cohortlabel"]]
    lineTx <- selarm[["lineTx"]]
    armSt <- selarm[["armStatus"]]
    dt_row <- tibble(
      armID = armID,
      cohortlabel = cohortlabel,
      lineTx = lineTx,
      armStatus = armSt,
      Gene = input$gene1,
      Type = input$typ, 
      Variant = input$var,
      Selection = input$selec,
      Function = input$func)
    
    disAd$dfAdd <- disAd$dfAdd %>% bind_rows(dt_row) %>% distinct()
    
    output$dt_biomark <- renderDT({
      datatable(disAd$dfAdd, 
                rownames = F,
                options = list(dom = 't',pageLength = 15))
    })
    
    proxy <- dataTableProxy("dt_biomark")
    proxy %>% selectRows(NULL)
  })

  
  # TABLE B: when (+Add common biomarker) is chosen
  observeEvent(input$final_edit, {
    shiny::req(disAd$add_or_edit == 1)
    shiny::removeModal()
    armdf <- disAd$armDfInfo 
    nArm <- nrow(armdf)
    for(e in 1:nArm){
      dt_rowall <- tibble(
        armID = as.character(armdf[e, "armID"]),
        cohortlabel = as.character(armdf[e, "cohortlabel"]),
        lineTx = as.character(armdf[e, "lineTx"]),
        armStatus = as.character(armdf[e, "armStatus"]),
        Gene = input$gene1,
        Type = input$typ, 
        Variant = input$var, 
        Selection = input$selec, 
        Function = input$func)
      
      disAd$dfAdd <- disAd$dfAdd %>% bind_rows(dt_rowall) %>% distinct()
    }
    
    output$dt_biomark <- renderDT({
      datatable(disAd$dfAdd, 
                rownames = FALSE,
                options = list(dom = 't',pageLength = 15))
    })
    
    proxy <- dataTableProxy("dt_biomark")
    proxy %>% selectRows(NULL)
  })
  

  ### remove edit modal when close or submit button is clicked
  observeEvent(input$dismiss_modal, {
    shiny::removeModal()
  })
  
  observeEvent(input$dismiss_modal_arminfo, {
    shiny::removeModal()
  })
  
  

  ##### Panel 4: Documentation 
  # Open the Document Tab and display the UI on Update
  observeEvent(input$bioMrk,{
    updateTabsetPanel(session, "inNav", selected = "Documents")
    output$doc_link <- renderText({input$doc})
    output$DisDoc <- renderUI({
      docuOut 
    })
  })
  

  
  ##### Panel 5: View Trial
  observeEvent(input$move_brow,{
    updateTabsetPanel(session, "inNav", selected = "View Trial")
    output$DisBrow = renderUI({
      browserOut
    })
  })
  
  # Display the Query Information Table
  output$displayBio <- renderReactable({
    
    # save all the variables to their appropiate values 
    infoDis <- displatAPI() %>% 
      select(!(arm))
    
    # save the arm info from query output
    armTb <- left_join(disAd$armDf, disAd$armDfInfo, by = "cohortlabel")
    armTb <- armTb %>% rownames_to_column(var = "ArmID")
    armTb <- tibble(
      ArmID = armTb$ArmID,
      cohortlabel = armTb$cohortlabel,
      drug = armTb$drug,
      arm_type = armTb$arm_type,
      line_of_therapy = armTb$lineTx,
      arm_hold_status = armTb$armStatus
    )
    
    # save the disease info entered
    DisTab <- as_tibble(disAd$indisAd)

    # save the biomarker info entered
    
    bioMarkTb <- as_tibble(disAd$dfAdd)
    tb_add <- bioMarkTb %>%  mutate(summary = "") %>% mutate(
      summary = case_when(
        # Mutation variant based
        Gene != "Not available" & Type != "Not available" & Variant != "Not available" & Function != "Not available"~ paste(Gene, Variant, Function, .sep = " "),
        Gene != "Not available" & Type != "Not available" & Variant != "Not available" & Function == "Not available"~ paste(Gene, Variant, .sep = " "),
        
        # Expression, fusion, CNA, etc  
        Gene != "Not available" & Type != "Not available" & Variant == "Not available" & Function != "Not available" ~ paste(Gene, Type, Function, .sep = " "),
        Gene != "Not available" & Type != "Not available" & Variant == "Not available" & Function == "Not available" ~ paste(Gene, Type, .sep = " "),
        # without gene 
        Gene == "Not available" & Type != "Not available" & Variant == "Not available" ~ paste(Type, Function, .sep = " "),
        # when empty 
        Gene == "Not available" & Type == "Not available" & Variant == "Not available" & Function == "Not available" ~ paste("absent")
        
      )
    )
    tb_add = tb_add[,c(1:2,5:10)]
    
    # adding the biomarker tibble to the respective cohort 
    alltoAmBK = left_join(armTb, tb_add, by = c('ArmID' = 'armID'))
    #print(colnames(alltoAmBK))
    colnames(alltoAmBK) = c("ArmID", "cohortlabel", "drug" ,"arm_type" ,"line_of_therapy", "arm_hold_status",          
                            "cohort", "Gene" , "Type" , "Variant","Selection", "Function" ,"summary" )
    
    armForBioMk = alltoAmBK %>% group_by(ArmID, cohortlabel, drug ,arm_type ,line_of_therapy, arm_hold_status ) %>% nest()
    armForBioMk = setnames(armForBioMk, "data", "biomarker")


    # ----------------------------------------------------------------------------------------- # 
    # making the data tibble for each trial entry
    
    # final tibble to display  
    disBrw2 <<- tibble(
      info = tibble(NCT = input$info_NCT,
                    jit = input$info_jit,
                    trial_name = input$info_trial_name
      ),
      disease = tibble(summary = input$disSum,
                       details = list(DisTab)
      ),
      query = tibble(nct = input$info_NCT,
                     title = infoDis$title,
                     current_status = infoDis$current_status,
                     status_verif_date = infoDis$status_verif_date,
                     last_update_date = infoDis$last_update_date,
                     trial_hold_status = input$trHold,
                     sponsor = infoDis$sponsor,
                     brief_summary = infoDis$brief_summary,
                     conditions = infoDis$conditiions,
                     type = infoDis$type,
                     phase = infoDis$phase,
                     arm = list(armForBioMk),
                     # docs = if(input$doc %>% is_empty()) {
                     #   docs = infoDis$link
                     # } else
                     # {
                     #   docs = glue("<a href=\\", input$doc, "\\", "target=\"_blank\">site-documentation</a>")
                     # },
                     docs = glue("<a href=\\", input$doc, "\\", "target=\"_blank\">site-documentation</a>"),
                     min_age = infoDis$min_age,
                     gender = infoDis$gender,
                     link = infoDis$link
      )
    )
    
    

    view_trial_table <- reactable(disBrw2 %>%
                                    unnest(c(info, disease, query)) %>%
                                    select(NCT:trial_name),
                                  resizable = TRUE,
                                  style = list(minWidth = 800),
                                  fullWidth = TRUE,
                                  defaultExpanded = TRUE,
                                  details = function(index) {
                                    
                                    tab <- disBrw2 %>%
                                      unnest(c(info, disease, query)) %>%
                                      select(-(NCT:nct), -arm)
                                    
                                    tab_arms <- disBrw2 %>%
                                      unnest(c(info, disease, query)) %>%
                                      select(arm) %>%
                                      unnest(arm) %>%
                                      select(-cohortlabel) %>%
                                      unnest(biomarker) %>%
                                      select(-c(ArmID, line_of_therapy, arm_hold_status))
                                    
                                    tab_disease <- disBrw2$disease %>% unnest(details)
                                    # tab_disease <- disBrw2 %>%
                                    #   unnest(c(info, disease, query)) %>%
                                    #   select(summary:details) %>%
                                    #   unnest(details)
                                    
                                    htmltools::div(style = "padding: 16px",
                                                   reactable(tab %>% t(),
                                                             #columns = c("Key", "Value"),
                                                             #rownames = FALSE,
                                                             outlined = TRUE,
                                                             pagination = FALSE),
                                                   reactable(tab_arms, outlined = TRUE),
                                                   reactable(tab_disease, outlined = TRUE)
                                    )
                                  }
    )
    
    disAd$rsdf <- disBrw2
    view_trial_table
    
               }
    )
  
  resetAll <- function() {
    reset("info_NCT")
    reset("info_jit")
    reset("info_trial_name")
    #firsthalfUI <- NULL
    reset("infoquery")
    
    #responses <- NULL
    reset("armsOnly")
    disAd$indisAd = tibble() # disease  
    disAd$armDf = tibble() # cohort 
    disAd$armDfInfo = tibble() # cohort + arm info
    #disAd$armDfBiomarker = tibble() # cohort + arm info + biomarker
    disAd$dfAdd = tibble() # cohort + biomarker
    add_or_edit = NULL  # confirming the button selection
    add_or_edit_arminfo = NULL
    disAd$rsdf = tibble() 
  }
  
  
  observeEvent(input$confirm1,{
    outSubmit()
    #disAd$allbrws = disAd$allbrws %>% dplyr::bind_rows(disAd$rsdf) 
    alert("Submitted successfully!")
    refresh()
    resetAll()
    
    #updateReactable("responses", data = NULL)
    #updateTabsetPanel(session = session, inputId = "inNav", selected = "NCT ID")
  })
  # observeEvent(input$final_confirm,{
  #   print(disAd$resultsdf)
  # })

  ### remove edit modal when close button is clicked or submit button
  shiny::observeEvent(input$final_cancel, {
    shiny::removeModal()
  })
}

# NCT03037385

##### APP ######
shinyApp(ui, server)
