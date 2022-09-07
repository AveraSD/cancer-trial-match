## server side utils

# for additional info adding 
firsthalfUI <- fluidRow(
  br(),
  div(class = "container",
      br(),
      column(width = 4,
             offset = 8, 
             actionButton("next1", "Move to Disease",
                          class = "btn-warning"))
  )
)

disAd <- reactiveValues(
  indisAd = tibble(), # disease  
  armDf = tibble(), # cohort 
  armDfInfo = tibble(), # cohort + arm info
  #armDfBiomarker = tibble(), # cohort + arm info + biomarker
  dfAdd = tibble(), # cohort + biomarker
  add_or_edit = NULL,  # confirming the button selection
  add_or_edit_arminfo = NULL, 
  allbrws = tibble(), # all together
  #resultsdf = tibble(), # individual 
  rsdf = tibble() # individual 
)


# helper function to create buttons for the biomarker section
create_btns_biomarker <- function(x) {
  x %>%
    purrr::map_chr(~
                     paste0(
                       '<div class = "btn-group">
                   <button class="btn btn-default action-button btn-primary action_button btn-sm" id="edit_',
                       .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-plus"></i></button>
                   </div>'
                     ))
}

create_btns_arminfo <- function(x) {
  x %>%
    purrr::map_chr(~
                     paste0(
                       '<div class = "btn-group">
                   <button class="btn btn-default action-button btn-primary action_button btn-sm" id="arminfo_',
                   .x, '" type="button" onclick=get_id(this.id)><i class="fas fa-plus"></i></button>
                   </div>'
                     ))
}

# add arm info modal - keys and values
modal_arminfo <- function(lineTx, armStatus) {
  
  shiny::modalDialog(
    h4(textOutput("TEXTA_arminfo", container = span)),
    div(
      class = "text-center",
      div(
        style = "display: inline-block;",
        selectInput(
          inputId = "lineTx",
          label = "Line of Therapy",
          choices = c("Not available", 1, 2, 3, 4, 5, 6, 7, 8, 9, 10),
          multiple = F,
          width = "200px"
        )
      ),
      div(
        style = "display: inline-block;",
        selectInput(
          inputId = "armStatus",
          label = "Arm status for the site",
          choices = c("Not available", "open", "on hold", "closed"),
          multiple = F,
          width = "200px"
        )
      )
      
    ),
    size = "m",
    easyClose = TRUE,
    footer = div(
      class = "pull-right container",
      actionButton(
        inputId = "final_edit_arminfo",
        label = " Arm Information",
        icon = shiny::icon("plus"),
        class = "btn-primary"
      ),
      actionButton(
        inputId = "dismiss_modal_arminfo",
        label = "Close",
        class = "btn-danger"
      )
    )
  ) %>% showModal()
}



# biomarker modal - keys and values
modal_biomarker <- function(gene1, gene2, typ, var, selec, func) {

  modalDialog(
    h4(textOutput("TEXTA", container = span)),
    div(
      class = "text-center",
      shinyjs::useShinyjs(),
      div(
        style = "display: inline-block;",
        selectInput(
          inputId = "gene1",
          label = "Gene", 
          #choices = NULL,
          #choices = c("TP53", "BRAF", "PIK3CA", "KRAS"),
          choices = allgenes$x,
          multiple = F,
          #selected = NULL,
          #options = list(`actions-box` = TRUE,`live-search` = TRUE,size=10),
          width = "200px"
        )
      ),
      div(
        style = "display: inline-block;",
        selectizeInput(
          inputId = "gene2",
          label = "Gene2", 
          #choices = NULL,
          #choices = c("TP53", "BRAF", "PIK3CA", "KRAS"),
          choices = allgenes$x,
          multiple = F,
          #selected = NULL,
          #options = list(`actions-box` = TRUE,`live-search` = TRUE,size=10),
          width = "200px"
        )
      ),
      div(
        style = "display: inline-block;",
        selectInput(
          inputId = "typ",
          label = "Type",
          choices = c("Not available", "Mutation","Missense mut", "Frame Shift mut", "Splice site mut", "Wild-Type", "Amplification", "Deletion", "TMB", "MSI", "PD-L1", "Fusion", "RNA expr", "HRD", "MMR", "ER (IHC/FISH)", "PR (IHC/FISH)", "HER2 (IHC/FISH)"),
          multiple = F,
          width = "200px"
        )
      ),
      div(
        style = "display: inline-block;",
        selectizeInput(
          inputId = "var",
          label = "Variant",
          #choices = NULL,
          #choices = c("V600E", "G12C", "G12D", "R43H"),
          choices = allVar$x,
          multiple = F,
          width = "200px"
        )
      ),
      div(
        style = "display: inline-block;",
        selectInput(
          inputId = "selec",
          label = "selection",
          choices = c("include","exclude"),
          multiple = F,
          width = "200px"
        )
      ),
      div(
        style = "display: inline-block;",
        selectInput(
          inputId = "func",
          label = "function",
          choices = c("Not available", "high", "low", "positive", "negative", "unstable", "activating"),
          multiple = F,
          width = "200px"
        )
      )
      
    ),
    size = "m",
    easyClose = TRUE,
    footer = div(
      class = "pull-right container",
      actionButton(
        inputId = "final_edit",
        label = " Biomarker",
        icon = shiny::icon("plus"),
        class = "btn-primary"
      ),
      actionButton(
        inputId = "dismiss_modal",
        label = "Close",
        class = "btn-danger"
      )
    )
  ) %>% showModal()
}



# Browse table final confirmation dialogue box 
modal_confirm <- function(){
  modalDialog(
    title = "Final confirmation",
    div(
      h4(em(("Please confirm to add the curated trial information to the database"))),
    ),
    size = "m",
    easyClose = TRUE,
    footer = div(
      class = "pull-right container",
      actionButton(
        inputId = "final_confirm",
        label = " Confirm",
        icon = shiny::icon("check"),
        class = "btn-success"
      ),
      actionButton(
        inputId = "final_cancel",
        label = "Close",
        class = "btn-danger"
      )
    )
    
  ) %>% showModal()
}
