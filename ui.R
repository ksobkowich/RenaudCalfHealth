dashboardPage(
  skin = "black-light",
  
  # Header ------------------------------------------------------------------
  dashboardHeader(
    title = tags$div(
      h2("Dairy Health Toolkit",
         style = "font-weight:900; color: #E51937; margin: 0; text-align: center;"
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
      menuItem("Baseline IgG", tabName = "igg", icon = icon("calculator")),
      menuItem("Baseline STP", tabName = "stp", icon = icon("calculator"))
      
    )
  ),
  
  # Main Content ------------------------------------------------------------
  dashboardBody(
    includeCSS("styles.css"),
    includeScript("custom.js"),
    
    
    tabItems(
      
      # Home Tab ----------------------------------------------------------------
      tabItem(
        tabName = "home",
        
        # --- HERO HEADER --------------------------------------------------------
        fluidRow(
          column(
            width = 12,
            div(
              style = "
          position: relative;
          height: 300px;
          border-radius: 12px;
          overflow: hidden;
          margin-bottom: 30px;
        ",
              
              # Background image
              tags$img(
                src = "https://dairyatguelph.ca/wp-content/uploads/2024/03/iStock-483289058.jpg",
                style = "
            width: 100%;
            height: 100%;
            object-fit: cover;
          ",
                alt = "Calf on a farm"
              ),
              
              # Dark overlay for text readability
              div(
                style = "
            position: absolute;
            top: 0; left: 0;
            width: 100%;
            height: 100%;
            background: linear-gradient(
              rgba(0,0,0,0.55),
              rgba(0,0,0,0.35)
            );
          "
              ),
              
              # Text overlay
              div(
                style = "
            position: absolute;
            bottom: 30px;
            left: 40px;
            color: white;
          ",
                h1(
                  "Dairy Health Toolkit",
                  style = "
              font-weight: 600;
              margin-bottom: 8px;
            "
                ),
                h4(
                  "Decision-support tools for calf health, productivity, and economic impact",
                  style = "
              font-weight: 400;
              max-width: 700px;
            "
                )
              )
            )
          )
        ),
        
        # --- MAIN CONTENT -------------------------------------------------------
        fluidRow(
          column(
            width = 9, offset = 1,
            p(
              "This toolkit provides interactive calculators and prediction tools to help quantify the health and prodcution loss impacts of common calf diseases, and support evidence-based decision-making.",
              style = "font-size: 16px; line-height: 1.6;"
            ),
            p(
              "Use the sidebar to explore individual health topics, and view detailed breakdowns.",
              style = "font-size: 16px; line-height: 1.6;"
            ),
            br(),
            tags$ul(
              tags$li("Production loss estimates of diarrhea and pneumonia"),
              tags$li("Customizable herd and management assumptions"),
              tags$li("IgG and STP prediction tools"),
              tags$li("Downloadable tables and results")
            )
          )
        ),
        
        br(),
        
        # --- GETTING STARTED ----------------------------------------------------
        fluidRow(
          column(
            width = 12,
            div(
              style = "
          background-color: #f7f7f7;
          padding: 25px;
          border-radius: 10px;
        ",
              h4("Features"),
              fluidRow(
                column(
                  4,
                  icon("chart-line", class = "text-primary"),
                  strong(" Disease Impact"),
                  p("Explore cost and productivity impacts of diarrhea and pneumonia.")
                ),
                column(
                  4,
                  icon("calculator", class = "text-primary"),
                  strong(" Predictive Tools"),
                  p("Estimate IgG and STP values using validated models.")
                ),
                column(
                  4,
                  icon("sliders-h", class = "text-primary"),
                  strong(" Customize Assumptions"),
                  p("Adjust herd-level values such as size, costs, and treatment parameters to tailor estimater.")
                )
              )
            )
          )
        ),
        br(), br(),
        
        fluidRow(
          column(
            width = 12,
            div(
              style = "
        border-top: 1px solid #e5e5e5;
        padding-top: 20px;
        margin-top: 30px;
        display: flex;
        align-items: center;
        gap: 20px;
      ",
              
              tags$img(
                src = "https://s3-eu-west-1.amazonaws.com/assets.in-part.com/1Qi2UyvATA2qYzxmJveI_Stacked_FullColour_WhiteBG.png",
                alt = "University of Guelph logo",
                style = "
          height: 45px;
          opacity: 0.9;
        "
              ),
              
              # Attribution text
              div(
                p(
                  "This toolkit was developed based on the collective work, expertise, and research contributions of numerous members of the Renaud Lab at the University of Guelph.",
                  style = "
            margin: 0;
            font-size: 14px;
            color: #666;
            line-height: 1.5;
          "
                )
              )
            )
          )
        )
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
                  sliderInput("perc_diar_total", "% w/ diarrhea", min = 0, max = 100, step = 1, value = diar[["perc_prev"]]),
                  
                  hr(),
                  actionBttn("additional_controls_diar", "Additional Controls", style = "pill", size = "s", color = "danger")
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
                  sliderInput("perc_pna_total", "% w/ pneumonia", min = 0, max = 100, step = 1, value = pna[["perc_prev"]]),
                  
                  hr(),
                  actionBttn("additional_controls_pna", "Additional Controls", style = "pill", size = "s", color = "danger")
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
                             actionButton("show_example_igg_data", label = NULL, icon = icon("question-circle"),
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
              
      ),
      
      # STP Tab -----------------------------------------------------------------
      tabItem(tabName = "stp",
              h2("Baseline Serum Total Protein (STP) Estimator"),
              h4("This calculator estimates what a cow’s STP level would have been on day 1, based on a sample collected within 7 days after calving."),
              fluidRow(
                
                column(5,
                       wellPanel(
                         
                         h4("Individual Calculation", style = "text-align: center;"),
                         fluidRow(
                           column(4, offset = 1,
                                  numericInput("single_stp_value", "STP value (g/L)", min = 0, max = 100, value = NULL)
                           ),
                           column(4, offset = 1,
                                  numericInput("single_stp_day", "Days after calving", min = 0, max = 7, step = 1, value = NULL)
                           ) 
                         ),
                         fluidRow(
                           column(10, offset = 1,
                                  radioButtons(
                                    inputId  = "single_stp_group",
                                    label    = "Colostrum group",
                                    choices  = c("Maternal" = "1", "Replacer" = "2", "Mixed" = "3"),
                                    selected = character(0),
                                    inline   = TRUE
                                  )
                           )
                         ),
                         div(
                           actionButton("single_stp_submit_button", "Estimate", class = "submit_button"),
                           style = "text-align: right;"
                         ),
                         uiOutput("single_stp_result_ui"),
                         br(),
                         hr(),
                         br(),
                         h4("Batch Conversion", style = "text-align: center;"),
                         div(
                           tags$label(
                             `for` = "batch_stp_file_in",
                             "Import Herd STP Values ",
                             actionButton("show_example_stp_data", label = NULL, icon = icon("question-circle"),
                                          style = "padding: 0 4px; border: none; background: none; vertical-align: middle;")
                           ),
                           fileInput("batch_stp_file_in", label = NULL, accept = c(".csv", ".xlsx", ".xls"))
                         ),
                         uiOutput("batch_stp_column_select"),
                         uiOutput("batch_stp_submit_button_ui")
                       )
                ),
                
                column(7,
                       uiOutput("batch_stp_result_ui")
                )
                
              )
              
      )
      
    )
  )
)