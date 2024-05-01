
# ui.R for SAMPLE shiny app (shiny-sms-track)

shiny::shinyUI(shiny::fluidPage(
  theme = bslib:: bs_theme(bootswatch = "darkly"),
  shinyjs::useShinyjs(),
  shinyWidgets::useShinydashboard(),
  shinyWidgets::useSweetAlert(),
  shiny::titlePanel("SAMPLE - A Shiny App"),
  tags$head(tags$style(HTML("
    /*
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
  shiny::navbarMenu(
    title = "Choose tab",
    shiny::navbarPage(
      shiny::sidebarPanel(
        width = 12,
        shiny::tabPanel(
          width = 12,
          title = "Lab Sample App",
          value = "tab1",
          shiny::fluidRow(
            
            # Left column ####
            shiny::column(
              width = 3,
              
              # User input and submit button
              shinydashboard::box(
                title = "Input",
                status = "primary",
                width = 12,
                solidHeader = TRUE,
                collapsible = TRUE,
                shiny::div(
                  id = "inputToggle",
                  # Drop-down menus for selecting sample classes
                  shiny::selectInput(
                    inputId = "labName",
                    label = "Lab Name:",
                    choices = ""
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "labServices",
                    label = "Lab Services:",
                    choices = "",
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE)
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "sampleClasses",
                    label = "Sample Classes:",
                    choices = "",
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE)
                  ),
                  shinyWidgets::pickerInput(
                    inputId = "domainIDs",
                    label = "Domain IDs:",
                    choices = "",
                    multiple = TRUE,
                    options = list(`actions-box` = TRUE)
                  ),
                  # Date range selection
                  shiny::dateRangeInput(
                    inputId = "dateRange",
                    label = "Date Range (Defaults to 90 days before today's return delay):"
                  ),
                  # Submit after date selected
                  shiny::actionButton(
                    inputId = "submit",
                    label = "Submit"
                  )
                )
              )
              
            ) 
            
          )
        )
      )
    ),
    
    
    # Right column ####
    shiny::column(
      width = 9,
      
      # Summary statistics
      shinydashboard::box(
        title = "Summary",
        status = "primary",
        width = 12,
        solidHeader = TRUE,
        collapsible = TRUE,
        shiny::fluidRow(
          shinydashboard::valueBoxOutput("countTotalRows", width = 4),
          shinydashboard::valueBoxOutput("countTotalMissing", width = 4),
          shinydashboard::valueBoxOutput("countTotalLate", width = 4)
        )
      ),
      
      # Section for ggplot graphs
      shinydashboard::box(
        title = "Graphs",
        status = "primary",
        width = 12,
        solidHeader = TRUE,
        collapsible = TRUE,
        shiny::fluidRow(
          shiny::column(
            width = 6,
            plotly::plotlyOutput(outputId = "plot1")
          ),
          shiny::column(
            width = 6,
            plotly::plotlyOutput(outputId = "plot2")
          )
        )
      ),
      
      # Section for data frame output
      shinydashboard::box(
        title = "Table",
        status = "primary",
        width = 12,
        solidHeader = TRUE,
        collapsible = TRUE,
        shinyjs::hidden(shiny::downloadButton("downloadCSV", "CSV")),
        shinyjs::hidden(shiny::downloadButton("downloadExcel", "Color Excel")),
        shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "datatable"))
      )
      
      
    ),
    
    shiny::sidebarPanel(
      width = 12,
      shiny::tabPanel(
        width = 12,
        title = "Instructions",
        value = "tab2",
        tagList(
          h1("Welcome to SAMPLE! (Shiny Application for Managing the Pipeline of Laboratory Entries)"),
          
          h2("Instructions for Using This App"),
          
          h3("Problem Statement"),
          p("NEON collects a wide variety of samples from different fields throughout the country, and it's crucial that these are properly tracked. This app was created to simplify and automate the process of tracking over 3,000 shipments per year, each containing multiple samples."),
          
          h3("How the App Works"),
          p("The app treats each shipped sample as being in a specific processing state: collection from the field, shipment to a lab, receipt by the lab, and return of data from the lab. The app's inputs, presented as dropdown menus, are populated by querying the Fulcrum API. Fulcrum is the app used to document collected samples."),
          
          h3("Inputs"),
          p("You provide inputs through a series of dropdown menus: Lab Name, Lab Services, Sample Classes, Domain IDs, and Date Range. After these are filled in order, the 'Submit' button initiates a series of queries to gather relevant sample information. The input options may seem complex, but they're populated automatically to avoid manual input."),
          
          h3("Data Queries"),
          p("Upon submission, the app queries the data in three steps: pulling all samples within the specified date range from each sample class, querying the sample endpoint data from NEON's Data Portal API to locate a sample, and querying the message admin in L0 PDR to obtain the exact upload timestamp. This process can take anywhere from 1 to 30 minutes, depending on the specifics of your request."),
          
          h3("Outputs"),
          p("The outputs are divided into three sections: summary statistics (e.g., total number of samples, number of samples with missing or late dates), bar graphs illustrating missing and late data by processing step (collect-ship, ship-receipt, receipt-return), and a detailed data table displaying information for each sample. You can download this table as a CSV or as a color-formatted Excel spreadsheet."),
          
          h3("Data Retrieval"),
          p("The input data is sourced from a Fulcrum data table (SELECT * FROM laboratories_cla WHERE track_lab = 'yes'), and the output data is retrieved from NEON's REST API, which queries the PDR for raw field data (L0 data). Data updates are managed by the responsible individuals. Please note that the data sources are for NEON employees only."),
          
          h3("Repository"),
          p("The code for this app is maintained at ", a("this GitHub repository", href = "https://github.com/NEONScience/shiny-sms-track"), ". The app was originally developed by a NEON intern in the summer of 2023. Only NEON employees may contribute to the code."),
          
          h3("Reporting Bugs"),
          p("If you encounter a bug, please report it ", a("here", href = "https://github.com/NEONScience/shiny-sms-track/issues"), ". Include as much detail as possible in your report, such as the input you used and the steps to reproduce the issue."),
          
          h3("Author Contact Information"),
          p("For more information, contact Zach Nickerson or Jim Coloso.")
        )
      )
    ),
    shiny::sidebarPanel(
      width = 12,
      shiny::tabPanel(
        width = 12,
        title = "Shipment tracking",
        value = "tab3",
        shiny::fluidRow(
          # Left column ####
          shiny::column(
            width = 3,
            # User use tab to track shipments by 
            shinydashboard::box(
              title = "Shipments",
              status = "primary",
              width = 12,
              solidHeader = TRUE,
              collapsible = TRUE,
              shiny::div(
                id = "inputToggle",
                # Insert the chain of custody ID number, tracking number or sample class
                shiny::selectInput(
                  inputId = "category",
                  label = "Choose category",
                  choices =c("Chain of Custody number", "tracking number"),
                  multiple= FALSE 
                ),
                shiny::textInput(
                  inputId = "number",
                  label = "Enter the COC or Carrier Tracking number")
                
              ),
              
              # Submit after date selected
              shiny::actionButton(
                inputId = "submit",
                label = "Submit"
              )
            )
            
          )
          
        )
      )
    )
  ),
  
  #end of left side panel with  all 3 tabs
  
  # Right column ####
  shiny::column(
    width = 9,
    
    # Summary statistics
    shinydashboard::box(
      title = "Summary",
      status = "primary",
      width = 12,
      solidHeader = TRUE,
      collapsible = TRUE,
      shiny::fluidRow(
        shinydashboard::valueBoxOutput("countTotalRows", width = 4),
        shinydashboard::valueBoxOutput("countTotalMissing", width = 4),
        shinydashboard::valueBoxOutput("countTotalLate", width = 4)
      )
    ),
    
    # Section for ggplot graphs
    shinydashboard::box(
      title = "Graphs",
      status = "primary",
      width = 12,
      solidHeader = TRUE,
      collapsible = TRUE,
      shiny::fluidRow(
        shiny::column(
          width = 6,
          plotly::plotlyOutput(outputId = "plot1")
        ),
        shiny::column(
          width = 6,
          plotly::plotlyOutput(outputId = "plot2")
        )
      )
    ),
    
    # Section for data frame output
    shinydashboard::box(
      title = "Table",
      status = "primary",
      width = 12,
      solidHeader = TRUE,
      collapsible = TRUE,
      shinyjs::hidden(shiny::downloadButton("downloadCSV", "CSV")),
      shinyjs::hidden(shiny::downloadButton("downloadExcel", "Color Excel")),
      shinycssloaders::withSpinner(DT::dataTableOutput(outputId = "datatable"))
    )
    
    
  ),
  
  shiny::mainPanel(
    shiny::uiOutput("tabContent")
    
  )
))








