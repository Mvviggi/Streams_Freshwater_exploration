shiny::shinyUI(shiny::fluidPage(
  theme = bslib:: bs_theme(bootswatch = "darkly"),
  shinyjs::useShinyjs(),
  shinyWidgets::useShinydashboard(),
  shinyWidgets::useSweetAlert(),
  shiny::titlePanel("SAMPLE - A Shiny App"),
  tags$head(tags$style(HTML("/*
     .box {
      border-color: #092d74 !important;
     }
     .box-header {
      background-color: #092d74 !important;
     }
    */
    .bootstrap-select .btn {
      background-color: #ffffff !important;
      color: #2d2d2d !important;
    }
    .bootstrap-select .dropdown-menu > li.active > a {
      background-color: #6f6f6f;
    }
    .selectize-dropdown-content . option.active {
      background-color: #6f6f6f;
    }
    .selectize-input.not-full.has-items.disabled {
      background-color: #ffffff;
      color: #2d2d2d;
    }
    .box.box-solid {
      background-color: #1a1a1a;
      color: #ffffff;
    }
    .sweet-alert button.confirm {
    background-color: #DD4B39 !important;
    }
    .sweet-alert .sa-button-container .confirm {
      background-color: #DD4B39 !important;
    }
  "))),
  shiny::navlistPanel(
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        width = 12,
        title= "Main App",
        value= "tab1",
        shiny::fluidRow(
          
          #left column###
          shiny::column(
            width= 3,
            #User input  and submit butto9n
            
    
  )
  
        )
      )
    )
    
  )
  
))
  
