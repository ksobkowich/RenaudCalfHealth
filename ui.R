dashboardPage(
  skin = "black-light",
  
  # Header ------------------------------------------------------------------
  dashboardHeader(
    title = tags$div(
      h2("Dairy Health Toolkit",
         style = "font-weight:900; color: #4facfe; margin: 0; text-align: center;"
      ),
      style = "width: 100%;"
    ),
    disable = FALSE
  ),
  
  # Side Menu ---------------------------------------------------------------
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home", tabName = "home", icon = icon("home")),
      menuItem("Diarrhea", tabName = "diarrhea", icon = icon("poop")),
      menuItem("BRD", tabName = "pneumonia", icon = icon("lungs")),
      menuItem("Baseline IgG", tabName = "igg", icon = icon("calculator"))
      
    )
  ),
  
  # Main Content ------------------------------------------------------------
  dashboardBody(
    includeCSS("styles.css"),
    includeScript("custom.js"),
    
    
    tabItems(
      
      # Home Tab ----------------------------------------------------------------
      tabItem(tabName = "home",
              h2("Welcome to the Calf Health Toolkit"),
              p("Select a health topic from the sidebar.")
      ),
      
      # Diarrhea Tab ------------------------------------------------------------
      tabItem(tabName = "diarrhea",
              h2("Production Losses Attributable to Diarrhea"),
              
              sidebarLayout(
                position = "right",
                
                sidebarPanel(
                  h4("Herd Parameters", style = "font-weight: bold;"),
                  numericInput("num_calvings", "Number of Calvings", min = 0, max = 1000, step = 1, value = general[["num_calvings"]]),
                  sliderInput("perc_hefers", "% Hefers", min = 0, max = 100, step = 1, value = general[["perc_hefers"]]),
                  
                  hr(),
                  h4("Diarrhea Prevalence", style = "font-weight: bold;"),
                  sliderInput("perc_short_diar", "% w/ short diarrhea", min = 0, max = 100, step = 1, value = short_diar[["perc_prev"]]),
                  sliderInput("perc_long_diar", "% w/ long diarrhea", min = 0, max = 100, step = 1, value = long_diar[["perc_prev"]]),
                  
                  hr(),
                  actionBttn("additional_controls_diar", "Additional Controls", style = "pill", size = "s", color = "default")
                ),
                
                mainPanel(
                  wellPanel(
                    div(
                      div(
                        class = "gradient-circle-outer",
                        div(
                          class = "gradient-circle-inner",
                          textOutput("diar_grand_total")
                        )
                      ),
                      h4("Total Herd Cost", style = "font-weight: bold;"),
                      style = "text-align: center;"
                    ),
                    
                    hr(),
                    div(
                      fluidRow(
                        column(6,
                               uiOutput("short_diar_total"),
                               h5("Short Diarrhea", style = "font-weight: bold; margin-bottom: 0px;"),
                               em("(1-4 days)", style = "color: #555;")
                        ),
                        div(class = "responsive-divider"),
                        column(6,
                               uiOutput("long_diar_total"),
                               h5("Long Diarrhea", style = "font-weight: bold; margin-bottom: 0px;"),
                               em("(> 4 days)", style = "color: #555;")
                        )
                      ),
                      style = "text-align: center;"
                    )
                  ),
                  
                  
                  # Cost Breakdowns ---------------------------------------------------------
                  wellPanel(
                    div(
                      style = "position: relative;",
                      h4("Cost Breakdown", style = "text-align: center; margin: 0;"),
                      div(
                        materialSwitch(
                          inputId = "diar_toggle",
                          label = "Details",
                          right = TRUE
                        ),
                        style = "position: absolute; top: 0; right: 0;"
                      )
                    ),
                    hr(),
                    DTOutput("diarTable")
                  )
                )
              )
      ),
      
      # Pneumonia Tab -----------------------------------------------------------
      tabItem(tabName = "pneumonia",
              h2("Production Losses Attributable to BRD"),
              
              sidebarLayout(
                position = "right",
                
                sidebarPanel(
                  h4("Herd Parameters", style = "font-weight: bold;"),
                  numericInput("num_calvings", "Number of Calvings", min = 0, max = 1000, step = 1, value = general[["num_calvings"]]),
                  sliderInput("perc_hefers", "% Hefers", min = 0, max = 100, step = 1, value = general[["perc_hefers"]]),
                  
                  hr(),
                  h4("Pneumonia Prevalence", style = "font-weight: bold;"),
                  sliderInput("perc_short_pna", "% w/ short pneumonia", min = 0, max = 100, step = 1, value = short_pna[["perc_prev"]]),
                  sliderInput("perc_long_pna", "% w/ long pneumonia", min = 0, max = 100, step = 1, value = long_pna[["perc_prev"]]),
                  
                  hr(),
                  actionBttn("additional_controls_pna", "Additional Controls", style = "pill", size = "s", color = "default")
                ),
                
                mainPanel(
                  wellPanel(
                    div(
                      div(
                        class = "gradient-circle-outer",
                        div(
                          class = "gradient-circle-inner",
                          textOutput("pna_grand_total")
                        )
                      ),
                      h4("Total Herd Cost", style = "font-weight: bold;"),
                      style = "text-align: center;"
                    ),
                    
                    hr(),
                    div(
                      fluidRow(
                        column(6,
                               uiOutput("short_pna_total"),
                               h5("Short Pneumonia", style = "font-weight: bold; margin-bottom: 0px;"),
                               em("(1-4 days)", style = "color: #555;")
                        ),
                        div(class = "responsive-divider"),
                        column(6,
                               uiOutput("long_pna_total"),
                               h5("Long Pneumonia", style = "font-weight: bold; margin-bottom: 0px;"),
                               em("(> 4 days)", style = "color: #555;")
                        )
                      ),
                      style = "text-align: center;"
                    )
                  ),
                  
                  
                  # Cost Breakdowns ---------------------------------------------------------
                  wellPanel(
                    div(
                      style = "position: relative;",
                      h4("Cost Breakdown", style = "text-align: center; margin: 0;"),
                      div(
                        materialSwitch(
                          inputId = "pna_toggle",
                          label = "Details",
                          right = TRUE
                        ),
                        style = "position: absolute; top: 0; right: 0;"
                      )
                    ),
                    hr(),
                    DTOutput("pnaTable")
                  )
                )
              )
      ),
      
      
      # IGG Tab -----------------------------------------------------------------
      tabItem(tabName = "igg",
              h2("Baseline Immunoglobulin G (IgG) Estimator"),
              h4("This calculator estimates what a cow’s serum IgG level would have been on day 1, based on a sample collected within 7 days after calving."),
              fluidRow(
                
                column(5,
                       wellPanel(
                         
                         h4("Individual Calculation", style = "text-align: center;"),
                         fluidRow(
                           column(4, offset = 1,
                                  numericInput("single_igg_value", "IgG value (g/L)", min = 0, max = 100, value = NULL)
                           ),
                           column(4, offset = 1,
                                  numericInput("single_igg_day", "Days after calving", min = 0, max = 7, step = 1, value = NULL)
                           )
                         ),
                         div(
                           actionButton("single_igg_submit_button", "Estimate", class = "submit_button"),
                           style = "text-align: right;"
                         ),
                         uiOutput("single_igg_result_ui"),
                         br(),
                         hr(),
                         br(),
                         h4("Batch Conversion", style = "text-align: center;"),
                         div(
                           tags$label(
                             `for` = "batch_igg_file_in",
                             "Import Herd IgG Values ",
                             actionButton("show_example_data", label = NULL, icon = icon("question-circle"),
                                          style = "padding: 0 4px; border: none; background: none; vertical-align: middle;")
                           ),
                           fileInput("batch_igg_file_in", label = NULL, accept = c(".csv", ".xlsx", ".xls"))
                         ),
                         uiOutput("batch_igg_column_select"),
                         uiOutput("batch_igg_submit_button_ui")
                       )
                ),
                
                column(7,
                       uiOutput("batch_igg_result_ui")
                )
                
              )
              
      )
      
    )
  )
)