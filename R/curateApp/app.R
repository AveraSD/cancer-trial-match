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

# NCT03037385

# source necessary files
source(here("R", "curateApp", "curateGlobal.R")) 
source(here("R", "curateApp", "queryNCT.R"))
source(here("R", "nct_to_json.R"))
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
  
  ##### Panel 1: NCT ID + query trial
  
  # action after clicking submit
  displatAPI <- eventReactive(input$submit, {
    output$firsthalf <- renderUI({firsthalfUI })
    
    # query NCT
    outAPI <- queryNct(input$info_NCT)
  })
  
  # Display the Query Information Table
  output$responses <- renderReactable({
    info = displatAPI() %>% 
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
  # Open the Disease Table and display the UI on Next Hit 
  observeEvent(input$next1,{
    updateTabsetPanel(session, "inNav",selected = "Disease")
    output$secondhalf = renderUI({
      seondhfUI
    })
  })
  
  # disease dropdown selection
  observeEvent(
    input$dise,
    updateSelectInput(session, "lev2", "Level2", 
                      choices = oncotree$level_2[oncotree$level_1==input$dise])
  )
  
  observeEvent(
    input$lev2,
    updateSelectInput(session, "lev3", "Level3",
                      choices = oncotree$level_3[oncotree$level_2==input$lev2 & oncotree$level_1==input$dise])
    
  )
  
  observeEvent(
    input$lev3,
    updateSelectInput(session, "lev4", "Level4",
                      choices = oncotree$level_4[oncotree$level_3==input$lev3 & oncotree$level_2==input$lev2 & oncotree$level_1==input$dise])
  )
  
  observeEvent(
    input$lev4,
    
    updateSelectInput(session, "lev5", "Level5",
                      choices = oncotree$level_5[oncotree$level_4==input$lev4 & oncotree$level_3==input$lev3 & oncotree$level_2==input$lev2 & oncotree$level_1==input$dise]))
  
  
  observeEvent(
    input$lev5,
    updateSelectInput(session, "lev6", "Level6", 
                      choices = oncotree$level_6[oncotree$level_5==input$lev5 & oncotree$level_4==input$lev4 & oncotree$level_3==input$lev3 & oncotree$level_2==input$lev2 & oncotree$level_1==input$dise]))
  
  observeEvent(
    input$lev6,
    updateSelectInput(session, "lev7", "Level7", 
                      choices = oncotree$level_7[oncotree$level_6==input$lev6 & oncotree$level_5==input$lev5 & oncotree$level_4==input$lev4 & oncotree$level_3==input$lev3 & oncotree$level_2==input$lev2 & oncotree$level_1==input$dise]))
  
  
  # Display the selected Disease and selection 
  observeEvent(input$addDis, {
    allInputDise = c(input$dise,input$lev2,input$lev3,input$lev4,input$lev5,input$lev6,input$lev7)
    lastInput = allInputDise[allInputDise != "NA"]
    lenlast = length(lastInput)
    
    addDisBtn <- tibble(code = lastInput[lenlast], 
                        selection = input$certir)
    
    disAd$indisAd = disAd$indisAd %>% bind_rows(addDisBtn)
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
    output$bioDis = renderUI({
      biom_display
    })
  })
  
  # table shows cohort arms to enter line of therapy and arm status
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
                             dom = 't')
              )
  })
  
  observeEvent(input$current_id, {
    if (!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "arminfo")) {
      selRow <- disAd$armDf[input$dt_table_arm_rows_selected, ]
      cohotLb = selRow[[1]]
      output$TEXTA_arminfo = renderText({
        cohotLb
      })
      modal_arminfo(lineTx = "", armStatus = "")
    }
  })
  
  observeEvent(input$final_edit_arminfo, {
    armdf <- disAd$armDf %>% rownames_to_column(var = "ArmID")
    selarm <- armdf[input$dt_table_arm_rows_selected, ]
    cohortID <- selarm[[1]]
    cohort <- selarm[[2]]
    dt_row <- tibble(
      armID = cohortID,
      cohortlabel = cohort,
      LineTherapy = input$lineTx,
      ArmStatus = input$armStatus
      )
    disAd$armDfInfo <- disAd$armDfInfo %>% bind_rows(dt_row)
   #disAd$armDfInfo <- inner_join(disAd$armDfInfo, dt_row, by = "cohortlabel")
    output$dt_table_arm_display <- renderDT({
      datatable(disAd$armDfInfo, 
                rownames = F,
                options = list(dom = 't'))
    })
  })
  
  # table showing list of cohort arms to be used for entering corresponding biomarkers
  output$dt_table <- renderDataTable({
    butns_biomarker <- create_btns_biomarker(nrow(disAd$armDfInfo))
    armterm <- disAd$armDfInfo %>% 
      #rownames_to_column(var = "ArmID") %>% 
      bind_cols(tibble("Buttons" = butns_biomarker))
    datatable(armterm, 
              escape = F,
              selection ='single',
              rownames = FALSE,
              colnames = c('Cohort(s)' = 'cohortlabel',
                           'Line of Therapy' = 'LineTherapy',
                           'Arm Status' = 'ArmStatus', 
                           'Add Biomarker' = 'Buttons'), 
              options = list(processing = FALSE, 
                             dom = 't')
    )
  })
  
  
  # Opening the dialogue box to the correct arm label 
  observeEvent(input$current_id,{
    if (!is.null(input$current_id) & stringr::str_detect(input$current_id, pattern = "edit")) {
      selRow <- disAd$armDfInfo[input$dt_table_rows_selected,]
      cohotLb = selRow[[1]]
      output$TEXTA = renderText({
        cohotLb
      })
      modal_biomarker(gene1 = "", typ = "", var = "", selec = "", func = "")
    }
  })
  
  # adding the biomarker info to the correct label - either by the name or the number 
  observeEvent(input$final_edit, {
    armdf <- disAd$armDfInfo %>% rownames_to_column(var = "ArmID")
    selarm <- armdf[input$dt_table_rows_selected,]
    cohortID <- selarm[["armID"]]
    cohort <- selarm[["Label"]]
    lineTx <- selarm[["LineTherapy"]]
    armSt <- selarm[["armStatus"]]
    dt_row <- tibble(
      armID = cohortID,
      Label = cohort,
      LineTherapy = lineTx,
      armStatus = armSt,
      Gene = input$gene1,
      Type = input$typ, 
      Variant = input$var,
      Selection = input$selec,
      Function = input$func)
    disAd$dfAdd <- disAd$dfAdd %>% bind_rows(dt_row)
    output$dt_biomark <- renderDT({
      datatable(disAd$dfAdd, 
                rownames = F,
                options = list(dom = 't'))
    })
  })
  
  
  # Opening the dialogue box to the correct  arm label 
  observeEvent(input$add_allBio,{
    output$TEXTA = renderText({
      "Enter biomarkers common to all cohort arms"
    })
    modal_biomarker(gene1 = "", typ = "", var = "", selec = "", func = "")
    disAd$add_or_edit <- 1
    
  })
  
  observeEvent(input$final_edit, {
    shiny::req(disAd$add_or_edit==1)
    armdf <- disAd$armDfInfo %>% rownames_to_column(var = "ArmID")
    nArm <- nrow(armdf)
    for(e in 1:nArm){
      dt_rowall <- tibble(
        armID = as.character(e),
        Label = as.character(armdf[e, "cohortlabel"]),
        LineTherapy = as.character(armdf[e, "LineTherapy"]),
        armStatus = as.character(armdf[e, "ArmStatus"]),
        Gene = input$gene1,
        Type = input$typ, 
        Variant = input$var, 
        Selection = input$selec, 
        Function = input$func)
      disAd$dfAdd <- disAd$dfAdd %>% 
        bind_rows(dt_rowall)
    }
    
    # table showing results after adding biomarkers to cohort arms
    output$dt_biomark <- renderDT({
      datatable(disAd$dfAdd, 
                rownames = FALSE,
                colnames = c('Arm #' = 'armID',
                             'Cohort' = 'Label'),
                options = list(dom = 't'))
    })
  })
  
  
  observeEvent(input$final_edit_arminfo, {
    shiny::req(disAd$add_or_edit==1)
    armdf <- disAd$armDfInfo %>% rownames_to_column(var = "ArmID")
    nArm <- nrow(armdf)
    for(e in 1:nArm){
      dt_rowall <- tibble(
        armID = as.character(e),
        LineTherapy = input$lineTx,
        armStatus = input$armStatus,
        Gene = input$gene1,
        Type = input$typ, 
        Variant = input$var, 
        Selection = input$selec, 
        Function = input$func, 
        Label = as.character(armdf[e,2]))
      disAd$dfAdd <- disAd$dfAdd %>% 
        bind_rows(dt_rowall)
    }
    
    # table showing results after adding biomarkers to cohort arms
    output$dt_biomark <- renderDT({
      datatable(disAd$dfAdd, 
                rownames = FALSE,
                colnames = c('Arm #' = 'armID',
                             'Cohort' = 'Label'),
                options = list(dom = 't'))
    })
  })
  

  ### remove edit modal when close button is clicked or submit button
  shiny::observeEvent(input$dismiss_modal, {
    shiny::removeModal()
  })
  
  shiny::observeEvent(input$dismiss_modal_arminfo, {
    shiny::removeModal()
  })
  
  ##### Panel 4: Documentation 
  volumes = getVolumes()
  observe({  
    shinyFileChoose(input, "Btn_chooseFile", roots = volumes, session = session)
    
    if(!is.null(input$Btn_chooseFile)){
      file_selected <- parseFilePaths(volumes, input$Btn_chooseFile)
      output$Path_to_file <- renderText(as.character(file_selected$datapath))
    }
  }) 
  
  # Open the Document Tab and display the UI on Update
  observeEvent(input$bioMrk,{
    updateTabsetPanel(session, "inNav", selected = "Documents")
    output$DisDoc = renderUI({
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
    infoDis = displatAPI() %>% 
      select(!(arm))
    
    # save the arm info from query output
    armTb <- inner_join(disAd$armDf, disAd$armDfInfo, by = "cohortlabel")
    armTb <-  armTb %>% rownames_to_column(var = "ArmID")
    armTb <- tibble(
      ArmID = armTb$ArmID,
      cohortlabel = armTb$cohortlabel,
      drug = armTb$drug,
      arm_type = armTb$arm_type,
      line_of_therapy = armTb$LineTherapy,
      arm_hold_status = armTb$ArmStatus
    )

    # save the disease info entered
    DisTab <- as_tibble(disAd$indisAd)

    armForBioMk <- armTb
    armForBioMk$biomarker <- as.list("NA")
    
    # save the biomarker info entered
    bioMarkTb <- as_tibble(disAd$dfAdd)
    for(bi in 1:nrow(armTb)){
      tb_add <- bioMarkTb %>% 
        filter(armID == bi) %>% 
        select(c(Gene, Type, Variant, Selection, Function)) %>% 
        rename("gene" = Gene, "type" = Type, "variant" = Variant, "selection" = Selection, "function" = Function) %>% 
        mutate(summary = "")
      armForBioMk$biomarker <- replace(armForBioMk$biomarker,
                                      armForBioMk$ArmID == bi,
                                      list(tb_add))
    }

    # final tibble to display 
    disBrw2 <- tibble(
      info = tibble(NCT = input$info_NCT,
                    jit = input$info_jit,
                    trial_name = input$info_trial_name
      ),
      disease = tibble(summary = input$disSum,
                       details = list(DisTab)
      ),
      query = tibble(nct = input$info_NCT,
                     title = "",
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
                     docs = "",
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
                       unnest(biomarker)

                     tab_disease <- disBrw2 %>%
                       unnest(c(info, disease, query)) %>%
                       select(summary:details) %>%
                       unnest(details)

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
    #reset("infoquery")
    #displatAPI() <- NULL
    #responses <- NULL
    #reset("armsOnly")
  }
  
  observeEvent(input$confrim1,{
    outSubmit()
    alert("Submitted successfully!")
    refresh()
    #resetAll()
    #updateReactable("responses", data = NULL)
    #updateTabsetPanel(session = session, inputId = "inNav", selected = "NCT ID")
  })
  
  
  observeEvent(input$final_confirm,{
    print(disAd$resultsdf)
  })

  ### remove edit modal when close button is clicked or submit button
  shiny::observeEvent(input$final_cancel, {
    shiny::removeModal()
  })
}

# NCT03037385

##### APP ######
shinyApp(ui, server)
