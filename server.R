function(input, output, session) {
  
  # Start from global values as a fallback
  general_values_rv  <- reactiveVal(general_values)
  diarrhea_values_rv <- reactiveVal(diarrhea_values)
  pna_values_rv      <- reactiveVal(pna_values)
  
  # On session start, try to fetch newest values once
  observeEvent(TRUE, {
    general_values_rv(  read_with_fallback(sheet_id, "General",   "data/general_values.rds") )
    diarrhea_values_rv( read_with_fallback(sheet_id, "Diarrhea",  "data/diarrhea_values.rds") )
    pna_values_rv(      read_with_fallback(sheet_id, "Pneumonia", "data/pna_values.rds") )
  }, once = TRUE)
  
  # Convert sheet dataframes -> named vectors (same structure as your globals)
  general_live <- reactive({
    gv <- general_values_rv()
    setNames(gv$value, gv$short_name)
  })
  
  short_diar_ref <- reactive({
    dv <- diarrhea_values_rv() %>% dplyr::filter(!is.na(short_name))
    setNames(dv$short_value, dv$short_name)
  })
  long_diar_ref <- reactive({
    dv <- diarrhea_values_rv() %>% dplyr::filter(!is.na(short_name))
    setNames(dv$long_value, dv$short_name)
  })
  
  short_pna_ref <- reactive({
    pv <- pna_values_rv() %>% dplyr::filter(!is.na(short_name))
    setNames(pv$short_value, pv$short_name)
  })
  long_pna_ref <- reactive({
    pv <- pna_values_rv() %>% dplyr::filter(!is.na(short_name))
    setNames(pv$long_value, pv$short_name)
  })
  
  # ------------------------------------------------------------------------
  # Initiate Reactive Values (your existing pattern)
  # ------------------------------------------------------------------------
  
  general    <- reactiveVal(general)
  short_diar <- reactiveVal(short_diar)
  long_diar  <- reactiveVal(long_diar)
  simple_diar_table   <- reactiveVal(NULL)
  detailed_diar_table <- reactiveVal(NULL)
  
  short_pna <- reactiveVal(short_pna)
  long_pna  <- reactiveVal(long_pna)
  simple_pna_table   <- reactiveVal(NULL)
  detailed_pna_table <- reactiveVal(NULL)
  
  # Apply the fetched sheet values ONCE at session start (seed the state)
  observeEvent(general_live(), {
    general(general_live())
  }, once = TRUE)
  
  observeEvent(short_diar_ref(), {
    short_diar(short_diar_ref())
  }, once = TRUE)
  
  observeEvent(long_diar_ref(), {
    long_diar(long_diar_ref())
  }, once = TRUE)
  
  observeEvent(short_pna_ref(), {
    short_pna(short_pna_ref())
  }, once = TRUE)
  
  observeEvent(long_pna_ref(), {
    long_pna(long_pna_ref())
  }, once = TRUE)
  
  observe({
    current_general <- general()
    current_general["num_calvings"] <- input$num_calvings
    current_general["perc_hefers"]  <- input$perc_hefers
    general(current_general)
    
    # --- Diarrhea: total -> split -------------------------------------------
    short_diar_share <- 0.4357                        #36.89 / (36.89 + 47.75)
    diar_total_prev  <- input$perc_diar_total
    
    current_short_diar <- short_diar()
    current_short_diar["perc_prev"] <- diar_total_prev * short_diar_share
    short_diar(current_short_diar)
    
    current_long_diar <- long_diar()
    current_long_diar["perc_prev"]  <- diar_total_prev * (1 - short_diar_share)
    long_diar(current_long_diar)
    
    # --- Pneumonia: total -> split ------------------------------------------
    short_pna_share <- 0.488                          #22 / (22 + 23)   
    pna_total_prev  <- input$perc_pna_total
    
    current_short_pna <- short_pna()
    current_short_pna["perc_prev"] <- pna_total_prev * short_pna_share
    short_pna(current_short_pna)
    
    current_long_pna <- long_pna()
    current_long_pna["perc_prev"]  <- pna_total_prev * (1 - short_pna_share)
    long_pna(current_long_pna)
  })
  
  observeEvent(input$apply_diar_changes, {
    current_general <- general()
    current_general["calf_cost"] <- input$calf_cost
    current_general["labour_cost"] <- input$labour_cost
    current_general["dry_feed_cost"] <- input$dry_feed_cost
    current_general["milk_price"] <- input$milk_price
    current_general["mr_cost"] <- input$mr_cost
    current_general["total_mr"] <- input$total_mr
    current_general["starter_cost"] <- input$starter_cost
    current_general["total_starter"] <- input$total_starter
    general(current_general)
    
    current_short_diar <- short_diar()
    current_short_diar["mort_risk"] <- input$short_diar_mort_risk
    current_short_diar["treatment_time"] <- input$short_diar_treatment_time
    current_short_diar["add_days_to_calving"] <- input$short_diar_add_days_to_calving
    current_short_diar["reduced_milk"] <- input$short_diar_reduced_milk
    current_short_diar["prewean_weight_gain"] <- input$short_diar_prewean_weight_gain
    current_short_diar["prewean_add_feed"] <- input$short_diar_prewean_add_feed
    current_short_diar["postwean_weight_gain"] <- input$short_diar_postwean_weight_gain
    current_short_diar["post_wean_add_feed"] <- input$short_diar_post_wean_add_feed
    short_diar(current_short_diar)
    
    current_long_diar <- long_diar()
    current_long_diar["mort_risk"] <- input$long_diar_mort_risk
    current_long_diar["treatment_time"] <- input$long_diar_treatment_time
    current_long_diar["add_days_to_calving"] <- input$long_diar_add_days_to_calving
    current_long_diar["reduced_milk"] <- input$long_diar_reduced_milk
    current_long_diar["prewean_weight_gain"] <- input$long_diar_prewean_weight_gain
    current_long_diar["prewean_add_feed"] <- input$long_diar_prewean_add_feed
    current_long_diar["postwean_weight_gain"] <- input$long_diar_postwean_weight_gain
    current_long_diar["post_wean_add_feed"] <- input$long_diar_post_wean_add_feed
    long_diar(current_long_diar)
    
    removeModal()
  })
  
  observeEvent(input$apply_pna_changes, {
    current_general <- general()
    current_general["calf_cost"] <- input$calf_cost
    current_general["labour_cost"] <- input$labour_cost
    current_general["dry_feed_cost"] <- input$dry_feed_cost
    current_general["milk_price"] <- input$milk_price
    current_general["mr_cost"] <- input$mr_cost
    current_general["total_mr"] <- input$total_mr
    current_general["starter_cost"] <- input$starter_cost
    current_general["total_starter"] <- input$total_starter
    general(current_general)
    
    current_short_pna <- short_pna()
    current_short_pna["mort_risk"] <- input$short_pna_mort_risk
    current_short_pna["treatment_time"] <- input$short_pna_treatment_time
    current_short_pna["reduced_milk"] <- input$short_pna_reduced_milk
    current_short_pna["prewean_weight_gain"] <- input$short_pna_prewean_weight_gain
    current_short_pna["prewean_add_feed"] <- input$short_pna_prewean_add_feed
    current_short_pna["postwean_weight_gain"] <- input$short_pna_postwean_weight_gain
    current_short_pna["post_wean_add_feed"] <- input$short_pna_post_wean_add_feed
    short_pna(current_short_pna)
    
    current_long_pna <- long_pna()
    current_long_pna["mort_risk"] <- input$long_pna_mort_risk
    current_long_pna["treatment_time"] <- input$long_pna_treatment_time
    current_long_pna["reduced_milk"] <- input$long_pna_reduced_milk
    current_long_pna["prewean_weight_gain"] <- input$long_pna_prewean_weight_gain
    current_long_pna["prewean_add_feed"] <- input$long_pna_prewean_add_feed
    current_long_pna["postwean_weight_gain"] <- input$long_pna_postwean_weight_gain
    current_long_pna["post_wean_add_feed"] <- input$long_pna_post_wean_add_feed
    long_pna(current_long_pna)
    
    removeModal()
  })
  
  # Render Additional Controls ----------------------------------------------
  observeEvent(input$additional_controls_diar, {
    showModal(
      modalDialog(
        title = "Additional Controls",
        
        fluidPage(
          h4("General Values"),
          fluidRow(
            column(6,
                   numericInput("calf_cost", "Cost of Calf ($)", value = general()[["calf_cost"]], min = 0, max = 10000),
                   numericInput("labour_cost", "Hourly Labour Rate ($)", value = general()[["labour_cost"]], min = 0, max = 10000),
                   numericInput("dry_feed_cost", "Dry Feed Cost ($/kg)", value = general()[["dry_feed_cost"]], min = 0, max = 10000),
                   numericInput("milk_price", "Milk Price ($/L)", value = general()[["milk_price"]], min = 0, max = 10000)
            ),
            column(6,
                   numericInput("mr_cost", "Milk Replacer Cost ($/kg)", value = general()[["mr_cost"]], min = 0, max = 10000),
                   numericInput("total_mr", "Total Meal Replacer (kg)", value = general()[["total_mr"]], min = 0, max = 10000),
                   numericInput("starter_cost", "Starter Cost Cost ($/kg)", value = general()[["starter_cost"]], min = 0, max = 10000),
                   numericInput("total_starter", "Total Starter (kg)", value = general()[["total_starter"]], min = 0, max = 10000)
            )
          ),
          hr(),
          
          h4("Short Diarrhea Values"),
          fluidRow(
            column(6,
                   numericInput("short_diar_mort_risk", "Risk of Mortality", value = short_diar()[["mort_risk"]], min = 0, max = 1),
                   numericInput("short_diar_treatment_time", "Time to Treat (hrs)", value = short_diar()[["treatment_time"]], min = 0, max = 10000),
                   numericInput("short_diar_add_days_to_calving", "Additional Days to Calving", value = short_diar()[["add_days_to_calving"]], min = 0, max = 10000),
                   numericInput("short_diar_reduced_milk", "Reduced Milk (L)", value = short_diar()[["reduced_milk"]], min = 0, max = 10000)
            ),
            column(6,
                   numericInput("short_diar_prewean_weight_gain", "Preweaning Weight Gained (kg)", value = short_diar()[["prewean_weight_gain"]], min = 0, max = 10000),
                   numericInput("short_diar_prewean_add_feed", "Preweaning Additional Feed (/kg gained)", value = short_diar()[["prewean_add_feed"]], min = 0, max = 10000),
                   numericInput("short_diar_postwean_weight_gain", "Postweaning Weight Gained (kg)", value = short_diar()[["postwean_weight_gain"]], min = 0, max = 10000),
                   numericInput("short_diar_post_wean_add_feed", "Postweaning Additional Feed (/kg gained)", value = short_diar()[["post_wean_add_feed"]], min = 0, max = 10000)
            )
          ),
          hr(),
          
          h4("Long Diarrhea Values"),
          fluidRow(
            column(6,
                   numericInput("long_diar_mort_risk", "Risk of Mortality", value = long_diar()[["mort_risk"]], min = 0, max = 1),
                   numericInput("long_diar_treatment_time", "Time to Treat (hrs)", value = long_diar()[["treatment_time"]], min = 0, max = 10000),
                   numericInput("long_diar_add_days_to_calving", "Additional Days to Calving", value = long_diar()[["add_days_to_calving"]], min = 0, max = 10000),
                   numericInput("long_diar_reduced_milk", "Reduced Milk (L)", value = long_diar()[["reduced_milk"]], min = 0, max = 10000)
            ),
            column(6,
                   numericInput("long_diar_prewean_weight_gain", "Preweaning Weight Gained (kg)", value = long_diar()[["prewean_weight_gain"]], min = 0, max = 10000),
                   numericInput("long_diar_prewean_add_feed", "Preweaning Additional Feed (/kg gained)", value = long_diar()[["prewean_add_feed"]], min = 0, max = 10000),
                   numericInput("long_diar_postwean_weight_gain", "Postweaning Weight Gained (kg)", value = long_diar()[["postwean_weight_gain"]], min = 0, max = 10000),
                   numericInput("long_diar_post_wean_add_feed", "Postweaning Additional Feed (/kg gained)", value = long_diar()[["post_wean_add_feed"]], min = 0, max = 10000)
            )
          )
        ),
        
        easyClose = FALSE,
        footer = tagList(
          modalButton("Close"),
          actionButton("apply_diar_changes", "Apply Changes")
        )
      )
    )
  })
  
  observeEvent(input$additional_controls_pna, {
    showModal(
      modalDialog(
        title = "Additional Controls",
        
        fluidPage(
          h4("General Values"),
          fluidRow(
            column(6,
                   numericInput("calf_cost", "Cost of Calf ($)", value = general()[["calf_cost"]], min = 0, max = 10000),
                   numericInput("labour_cost", "Hourly Labour Rate ($)", value = general()[["labour_cost"]], min = 0, max = 10000),
                   numericInput("dry_feed_cost", "Dry Feed Cost ($/kg)", value = general()[["dry_feed_cost"]], min = 0, max = 10000),
                   numericInput("milk_price", "Milk Price ($/L)", value = general()[["milk_price"]], min = 0, max = 10000)
            ),
            column(6,
                   numericInput("mr_cost", "Milk Replacer Cost ($/kg)", value = general()[["mr_cost"]], min = 0, max = 10000),
                   numericInput("total_mr", "Total Meal Replacer (kg)", value = general()[["total_mr"]], min = 0, max = 10000),
                   numericInput("starter_cost", "Starter Cost Cost ($/kg)", value = general()[["starter_cost"]], min = 0, max = 10000),
                   numericInput("total_starter", "Total Starter (kg)", value = general()[["total_starter"]], min = 0, max = 10000)
            )
          ),
          hr(),
          
          h4("Short Pneumonia Values"),
          fluidRow(
            column(6,
                   numericInput("short_pna_mort_risk", "Risk of Mortality", value = short_pna()[["mort_risk"]], min = 0, max = 1),
                   numericInput("short_pna_treatment_time", "Time to Treat (hrs)", value = short_pna()[["treatment_time"]], min = 0, max = 10000),
                   numericInput("short_pna_reduced_milk", "Reduced Milk (L)", value = short_pna()[["reduced_milk"]], min = 0, max = 10000)
            ),
            column(6,
                   numericInput("short_pna_prewean_weight_gain", "Preweaning Weight Gained (kg)", value = short_pna()[["prewean_weight_gain"]], min = 0, max = 10000),
                   numericInput("short_pna_prewean_add_feed", "Preweaning Additional Feed (/kg gained)", value = short_pna()[["prewean_add_feed"]], min = 0, max = 10000),
                   numericInput("short_pna_postwean_weight_gain", "Postweaning Weight Gained (kg)", value = short_pna()[["postwean_weight_gain"]], min = 0, max = 10000),
                   numericInput("short_pna_post_wean_add_feed", "Postweaning Additional Feed (/kg gained)", value = short_pna()[["post_wean_add_feed"]], min = 0, max = 10000)
            )
          ),
          hr(),
          
          h4("Long Pneumonia Values"),
          fluidRow(
            column(6,
                   numericInput("long_pna_mort_risk", "Risk of Mortality", value = long_pna()[["mort_risk"]], min = 0, max = 1),
                   numericInput("long_pna_treatment_time", "Time to Treat (hrs)", value = long_pna()[["treatment_time"]], min = 0, max = 10000),
                   numericInput("long_pna_reduced_milk", "Reduced Milk (L)", value = long_pna()[["reduced_milk"]], min = 0, max = 10000)
            ),
            column(6,
                   numericInput("long_pna_prewean_weight_gain", "Preweaning Weight Gained (kg)", value = long_pna()[["prewean_weight_gain"]], min = 0, max = 10000),
                   numericInput("long_pna_prewean_add_feed", "Preweaning Additional Feed (/kg gained)", value = long_pna()[["prewean_add_feed"]], min = 0, max = 10000),
                   numericInput("long_pna_postwean_weight_gain", "Postweaning Weight Gained (kg)", value = long_pna()[["postwean_weight_gain"]], min = 0, max = 10000),
                   numericInput("long_pna_post_wean_add_feed", "Postweaning Additional Feed (/kg gained)", value = long_pna()[["post_wean_add_feed"]], min = 0, max = 10000)
            )
          )
        ),
        
        easyClose = FALSE,
        footer = tagList(
          modalButton("Close"),
          actionButton("apply_pna_changes", "Apply Changes")
        )
      )
    )
  })
  
  # Calculations ------------------------------------------------------------
  general_calculations <- list(
    calf_feed_cost = function(general) {
      mr_perc <- general[["total_mr"]] / (general[["total_mr"]] + general[["total_starter"]])
      starter_perc <- general[["total_starter"]] / (general[["total_mr"]] + general[["total_starter"]])
      starter_perc * general[["starter_cost"]] + mr_perc * general[["mr_cost"]]
    }
  )
  
  diar_calculations <- list(
    
    mort_cost = function(data, general) {
      general[["calf_cost"]] * data[["mort_risk"]]
    },
    
    prewean_feed_cost = function(data, general, diar_calculations) {
      general[["calf_feed_cost"]] * data[["prewean_weight_gain"]] * data[["prewean_add_feed"]]
    },
    
    postwean_feed_cost = function(data, general) {
      general[["dry_feed_cost"]] * data[["postwean_weight_gain"]] * data[["post_wean_add_feed"]]
    },
    
    labour_cost = function(data, general) {
      general[["labour_cost"]] * data[["treatment_time"]]
    },
    
    dtc_cost = function(data, general) {
      general[["dry_feed_cost"]] * data[["add_days_to_calving"]]
    },
    
    milk_cost = function(data, general) {
      general[["milk_price"]] * data[["reduced_milk"]]
    }
    
  )
  
  pna_calculations <- list(
    
    mort_cost = function(data, general) {
      general[["calf_cost"]] * data[["mort_risk"]]
    },
    
    prewean_feed_cost = function(data, general, diar_calculations) {
      general[["calf_feed_cost"]] * data[["prewean_weight_gain"]] * data[["prewean_add_feed"]]
    },
    
    postwean_feed_cost = function(data, general) {
      general[["dry_feed_cost"]] * data[["postwean_weight_gain"]] * data[["post_wean_add_feed"]]
    },
    
    labour_cost = function(data, general) {
      general[["labour_cost"]] * data[["treatment_time"]]
    },
    
    milk_cost = function(data, general) {
      general[["milk_price"]] * data[["reduced_milk"]]
    }
    
  )
  
  observe({
    new_general <- apply_general_calculations(general(), general_calculations)
    
    new_short_diar <- apply_calculations(short_diar(), new_general, diar_calculations)
    new_long_diar  <- apply_calculations(long_diar(), new_general, diar_calculations)
    
    new_short_diar <- add_totals(new_short_diar, new_general)
    new_long_diar  <- add_totals(new_long_diar, new_general)
    
    new_short_pna <- apply_calculations(short_pna(), new_general, pna_calculations)
    new_long_pna  <- apply_calculations(long_pna(), new_general, pna_calculations)
    
    new_short_pna <- add_totals(new_short_pna, new_general)
    new_long_pna  <- add_totals(new_long_pna, new_general)
    
    general(new_general)
    short_diar(new_short_diar)
    long_diar(new_long_diar) 
    short_pna(new_short_pna)
    long_pna(new_long_pna)
    
    # Simple Diarrhea Table ---------------------------------------------------
    simple_diar_table <- data.frame(
      Short = c(short_diar()[["mort_cost"]],
                short_diar()[["prewean_feed_cost"]],
                short_diar()[["postwean_feed_cost"]],
                short_diar()[["labour_cost"]],
                short_diar()[["dtc_cost"]],
                short_diar()[["milk_cost"]],
                short_diar()[["cost_per_calf"]],
                short_diar()[["total_prev"]],
                short_diar()[["total_cost"]]),
      
      Value = c("<b>Calf Mortality</b>",
                "<b>Preweaning Feed</b>",
                "<b>Postweaning Feed</b>",
                "<b>Labour</b>",
                "<b>Additional Days to Calving</b>",
                "<b>Reduced Milk Production</b>",
                "<b>Total per Calf</b>", 
                "<b>Herd Prevalence</b>",
                "<b>Total Cost</b>"),
      
      Long = c(long_diar()[["mort_cost"]],
               long_diar()[["prewean_feed_cost"]],
               long_diar()[["postwean_feed_cost"]],
               long_diar()[["labour_cost"]],
               long_diar()[["dtc_cost"]],
               long_diar()[["milk_cost"]],
               long_diar()[["cost_per_calf"]],
               long_diar()[["total_prev"]],
               long_diar()[["total_cost"]])
    )
    
    simple_diar_table$Short <- round(simple_diar_table$Short, 2)
    simple_diar_table$Long <- round(simple_diar_table$Long, 2)
    
    simple_diar_table[, 1] <- ifelse(
      seq_len(nrow(simple_diar_table)) == 8,
      simple_diar_table[, 1],
      scales::dollar(as.numeric(simple_diar_table[, 1]))
    )
    
    simple_diar_table[, 3] <- ifelse(
      seq_len(nrow(simple_diar_table)) == 8,
      simple_diar_table[, 3],
      scales::dollar(as.numeric(simple_diar_table[, 3]))
    )
    
    simple_diar_table(simple_diar_table)
    
    # Detailed Diarrhea Table -------------------------------------------------
    detailed_diar_table <- data.frame(
      Short = c("",
                general()[["calf_cost"]],
                short_diar()[["mort_risk"]],
                short_diar()[["mort_cost"]],
                
                "",
                general()[["calf_feed_cost"]],
                short_diar()[["prewean_weight_gain"]],
                short_diar()[["prewean_add_feed"]],
                short_diar()[["prewean_feed_cost"]],
                
                "",
                general()[["dry_feed_cost"]],
                short_diar()[["postwean_weight_gain"]],
                short_diar()[["post_wean_add_feed"]],
                short_diar()[["postwean_feed_cost"]],
                
                "",
                general()[["labour_cost"]],
                short_diar()[["treatment_time"]],
                short_diar()[["labour_cost"]],
                
                "",
                short_diar()[["add_days_to_calving"]],
                general()[["dry_feed_cost"]],
                short_diar()[["dtc_cost"]],
                
                "",
                general()[["milk_price"]],
                short_diar()[["reduced_milk"]],
                short_diar()[["milk_cost"]],
                
                short_diar()[["cost_per_calf"]],
                short_diar()[["total_prev"]],
                short_diar()[["total_cost"]]
      ),
      
      Value = c("<b>Calf Mortality</b>",
                "Cost of a Calf",
                "Increased Risk of Mortality",
                "Total",
                
                "<b>Preweaning Feed</b>",
                "Cost of Feed (/kg)",
                "Weight Gained (kg)",
                "Additional Feed (/kg gained)",
                "Total",
                
                "<b>Postweaning Feed</b>",
                "Cost of Feed (/kg)",
                "Weight Gained (kg)",
                "Additional Feed (/kg gained)",
                "Total",
                
                "<b>Labour</b>",
                "Hourly Rate",
                "Time to Treat (hours)", 
                "Total",
                
                "<b>Additional Days to Calving</b>",
                "Additional Time (days)",
                "Feed Cost (/kg)",
                "Total",
                
                "<b>Reduced Milk Production</b>",
                "Milk Price (/L)",
                "Reduced Milk (L)",
                "Total",
                
                "<b>Total per Calf</b>", 
                "<b>Herd Prevalence</b>",
                "<b>Total Cost</b>"
      ),
      
      Long = c("",
               general()[["calf_cost"]],
               long_diar()[["mort_risk"]],
               long_diar()[["mort_cost"]],
               
               "",
               general()[["calf_feed_cost"]],
               long_diar()[["prewean_weight_gain"]],
               long_diar()[["prewean_add_feed"]],
               long_diar()[["prewean_feed_cost"]],
               
               "",
               general()[["dry_feed_cost"]],
               long_diar()[["postwean_weight_gain"]],
               long_diar()[["post_wean_add_feed"]],
               long_diar()[["postwean_feed_cost"]],
               
               "",
               general()[["labour_cost"]],
               long_diar()[["treatment_time"]],
               long_diar()[["labour_cost"]],
               
               "",
               long_diar()[["add_days_to_calving"]],
               general()[["dry_feed_cost"]],
               long_diar()[["dtc_cost"]],
               
               "",
               general()[["milk_price"]],
               long_diar()[["reduced_milk"]],
               long_diar()[["milk_cost"]],
               
               long_diar()[["cost_per_calf"]],
               long_diar()[["total_prev"]],
               long_diar()[["total_cost"]]
      )
    )
    
    detailed_diar_table[, 1] <- ifelse(
      seq_len(nrow(detailed_diar_table)) %in% c(3,7,8,12,13,17,20,25,28),
      detailed_diar_table[, 1],
      scales::dollar(as.numeric(detailed_diar_table[, 1]), accuracy = 0.01)
    )
    
    detailed_diar_table[, 3] <- ifelse(
      seq_len(nrow(detailed_diar_table)) %in% c(3,7,8,12,13,17,20,25,28),
      detailed_diar_table[, 3],
      scales::dollar(as.numeric(detailed_diar_table[, 3]), accuracy = 0.01)
    )
    
    detailed_diar_table(detailed_diar_table)
    
    # Simple Pneumonia Table ---------------------------------------------------
    simple_pna_table <- data.frame(
      Short = c(short_pna()[["mort_cost"]],
                short_pna()[["prewean_feed_cost"]],
                short_pna()[["postwean_feed_cost"]],
                short_pna()[["labour_cost"]],
                short_pna()[["milk_cost"]],
                short_pna()[["cost_per_calf"]],
                short_pna()[["total_prev"]],
                short_pna()[["total_cost"]]),
      
      Value = c("<b>Calf Mortality</b>",
                "<b>Preweaning Feed</b>",
                "<b>Postweaning Feed</b>",
                "<b>Labour</b>",
                "<b>Reduced Milk Production</b>",
                "<b>Total per Calf</b>", 
                "<b>Herd Prevalence</b>",
                "<b>Total Cost</b>"),
      
      Long = c(long_pna()[["mort_cost"]],
               long_pna()[["prewean_feed_cost"]],
               long_pna()[["postwean_feed_cost"]],
               long_pna()[["labour_cost"]],
               long_pna()[["milk_cost"]],
               long_pna()[["cost_per_calf"]],
               long_pna()[["total_prev"]],
               long_pna()[["total_cost"]])
    )
    
    simple_pna_table$Short <- round(simple_pna_table$Short, 2)
    simple_pna_table$Long <- round(simple_pna_table$Long, 2)
    
    simple_pna_table[, 1] <- ifelse(
      seq_len(nrow(simple_pna_table)) == 7,
      simple_pna_table[, 1],
      scales::dollar(as.numeric(simple_pna_table[, 1]))
    )
    
    simple_pna_table[, 3] <- ifelse(
      seq_len(nrow(simple_pna_table)) == 7,
      simple_pna_table[, 3],
      scales::dollar(as.numeric(simple_pna_table[, 3]))
    )
    
    simple_pna_table(simple_pna_table)
    
    # Detailed Pneumonia Table -------------------------------------------------
    detailed_pna_table <- data.frame(
      Short = c("",
                general()[["calf_cost"]],
                short_pna()[["mort_risk"]],
                short_pna()[["mort_cost"]],
                
                "",
                general()[["calf_feed_cost"]],
                short_pna()[["prewean_weight_gain"]],
                short_pna()[["prewean_add_feed"]],
                short_pna()[["prewean_feed_cost"]],
                
                "",
                general()[["dry_feed_cost"]],
                short_pna()[["postwean_weight_gain"]],
                short_pna()[["post_wean_add_feed"]],
                short_pna()[["postwean_feed_cost"]],
                
                "",
                general()[["labour_cost"]],
                short_pna()[["treatment_time"]],
                short_pna()[["labour_cost"]],
                
                "",
                general()[["milk_price"]],
                short_pna()[["reduced_milk"]],
                short_pna()[["milk_cost"]],
                
                short_pna()[["cost_per_calf"]],
                short_pna()[["total_prev"]],
                short_pna()[["total_cost"]]
      ),
      
      Value = c("<b>Calf Mortality</b>",
                "Cost of a Calf",
                "Increased Risk of Mortality",
                "Total",
                
                "<b>Preweaning Feed</b>",
                "Cost of Feed (/kg)",
                "Weight Gained (kg)",
                "Additional Feed (/kg gained)",
                "Total",
                
                "<b>Postweaning Feed</b>",
                "Cost of Feed (/kg)",
                "Weight Gained (kg)",
                "Additional Feed (/kg gained)",
                "Total",
                
                "<b>Labour</b>",
                "Hourly Rate",
                "Time to Treat (hours)", 
                "Total",
                
                "<b>Reduced Milk Production</b>",
                "Milk Price (/L)",
                "Reduced Milk (L)",
                "Total",
                
                "<b>Total per Calf</b>", 
                "<b>Herd Prevalence</b>",
                "<b>Total Cost</b>"
      ),
      
      Long = c("",
               general()[["calf_cost"]],
               long_pna()[["mort_risk"]],
               long_pna()[["mort_cost"]],
               
               "",
               general()[["calf_feed_cost"]],
               long_pna()[["prewean_weight_gain"]],
               long_pna()[["prewean_add_feed"]],
               long_pna()[["prewean_feed_cost"]],
               
               "",
               general()[["dry_feed_cost"]],
               long_pna()[["postwean_weight_gain"]],
               long_pna()[["post_wean_add_feed"]],
               long_pna()[["postwean_feed_cost"]],
               
               "",
               general()[["labour_cost"]],
               long_pna()[["treatment_time"]],
               long_pna()[["labour_cost"]],
               
               "",
               general()[["milk_price"]],
               long_pna()[["reduced_milk"]],
               long_pna()[["milk_cost"]],
               
               long_pna()[["cost_per_calf"]],
               long_pna()[["total_prev"]],
               long_pna()[["total_cost"]]
      )
    )
    
    detailed_pna_table[, 1] <- ifelse(
      seq_len(nrow(detailed_pna_table)) %in% c(3,7,8,12,13,17,21,24),
      detailed_pna_table[, 1],
      scales::dollar(as.numeric(detailed_pna_table[, 1]), accuracy = 0.01)
    )
    
    detailed_pna_table[, 3] <- ifelse(
      seq_len(nrow(detailed_pna_table)) %in% c(3,7,8,12,13,17,21,24),
      detailed_pna_table[, 3],
      scales::dollar(as.numeric(detailed_pna_table[, 3]), accuracy = 0.01)
    )
    
    detailed_pna_table(detailed_pna_table)
    
  })
  
  # Outputs -----------------------------------------------------------------
  output$diar_grand_total <- renderText({
    total <- short_diar()[["total_cost"]] + long_diar()[["total_cost"]]
    total <- round(total, 2)
    total <- prettyNum(total ,big.mark=",")
    total <- paste0("$", total)
    paste(total)
  })
  
  output$short_diar_total <- renderUI({
    total <- short_diar()[["total_cost"]]
    total <- round(total, 2)
    total <- prettyNum(total ,big.mark=",")
    total <- paste0("$", total)
    
    h4(total)
  })
  
  output$long_diar_total <- renderUI({
    total <- long_diar()[["total_cost"]]
    total <- round(total, 2)
    total <- prettyNum(total ,big.mark=",")
    total <- paste0("$", total)
    
    h4(total)
  })
  
  output$pna_grand_total <- renderText({
    total <- short_pna()[["total_cost"]] + long_pna()[["total_cost"]]
    total <- round(total, 2)
    total <- prettyNum(total ,big.mark=",")
    total <- paste0("$", total)
    paste(total)
  })
  
  output$short_pna_total <- renderUI({
    total <- short_pna()[["total_cost"]]
    total <- round(total, 2)
    total <- prettyNum(total ,big.mark=",")
    total <- paste0("$", total)
    
    h4(total)
  })
  
  output$long_pna_total <- renderUI({
    total <- long_pna()[["total_cost"]]
    total <- round(total, 2)
    total <- prettyNum(total ,big.mark=",")
    total <- paste0("$", total)
    
    h4(total)
  })
  
  output$diarTable <- renderDT({
    table_data <- if (input$diar_toggle) detailed_diar_table() else simple_diar_table()
    row_callback <- if (input$diar_toggle) {
      JS(
        'function(row, data, index) {',
        '  if (index === 4 || index === 9 || index === 14 || index === 18 || index === 22 || index === 26 || index === 28) {',
        '    $(row).css("border-top", "3px solid #ccc");',
        '  }',
        '  $(row).css("border-bottom", "1px solid #eee");',
        '}'
      )
    } else {
      JS(
        'function(row, data, index) {',
        '  if (index === 6 || index === 8) {',
        '    $(row).css("border-top", "3px solid #ccc");',
        '  }',
        '  $(row).css("border-bottom", "1px solid #eee");',
        '}'
      )
    }
    datatable(
      table_data,
      escape = FALSE,
      rownames = FALSE,
      selection = "none",
      colnames = c("Short Diarrhea", " ", "Long Diarrhea"),
      options = list(
        info = FALSE,
        paging = FALSE,
        ordering = FALSE,
        searching = FALSE,
        stripeClasses = FALSE,
        columnDefs = list(
          list(className = 'dt-right',  targets = 0, width = "30%"),
          list(className = 'dt-center', targets = 1, width = "40%"),
          list(className = 'dt-left',   targets = 2, width = "30%")
        ),
        rowCallback = row_callback
      )
    )
  })
  
  output$pnaTable <- renderDT({
    table_data <- if (input$pna_toggle) detailed_pna_table() else simple_pna_table()
    row_callback <- if (input$pna_toggle) {
      JS(
        'function(row, data, index) {',
        '  if (index === 4 || index === 9 || index === 14 || index === 18 || index === 22 || index === 24) {',
        '    $(row).css("border-top", "3px solid #ccc");',
        '  }',
        '  $(row).css("border-bottom", "1px solid #eee");',
        '}'
      )
    } else {
      JS(
        'function(row, data, index) {',
        '  if (index === 5 || index === 7) {',
        '    $(row).css("border-top", "3px solid #ccc");',
        '  }',
        '  $(row).css("border-bottom", "1px solid #eee");',
        '}'
      )
    }
    datatable(
      table_data,
      escape = FALSE,
      rownames = FALSE,
      selection = "none",
      colnames = c("Short Pneumonia", " ", "Long Pneumonia"),
      options = list(
        info = FALSE,
        paging = FALSE,
        ordering = FALSE,
        searching = FALSE,
        stripeClasses = FALSE,
        columnDefs = list(
          list(className = 'dt-right',  targets = 0, width = "30%"),
          list(className = 'dt-center', targets = 1, width = "40%"),
          list(className = 'dt-left',   targets = 2, width = "30%")
        ),
        rowCallback = row_callback
      )
    )
  })
  
  
  # -------------------------------------------------------------------------
  # IgG Tab  (Frequentist version — using pre-fitted lmer model)
  # -------------------------------------------------------------------------
  
  ## --- Single Calculation -------------------------------------------------
  single_igg_result <- reactiveVal(NULL)
  
  observeEvent(input$single_igg_submit_button, {
    if (is.null(input$single_igg_value) || is.null(input$single_igg_day) ||
        is.na(input$single_igg_value) || is.na(input$single_igg_day) ||
        !is.numeric(input$single_igg_value) || !is.numeric(input$single_igg_day) ||
        input$single_igg_day < 1 || input$single_igg_day > 7 || input$single_igg_value <= 0) {
      
      single_igg_result(NULL)
      showNotification("Please enter a valid IgG value (>0) and Day between 1 and 7.",
                       type = "error", duration = 5)
      
    } else {
      x <- predict_day1_igg(igg_model, input$single_igg_value, input$single_igg_day)
      single_igg_result(x)
    }
  })
  
  output$single_igg_result_ui <- renderUI({
    output_value <- single_igg_result()
    req(!is.null(output_value))
    
    tagList(
      h4(
        "Est. IgG on Day 1: ",  
        span(paste(round(output_value$median, 1), "g/L"),
             style = "display: inline; font-weight: 500; color: #4facfe"),
        style = "font-weight: 400; text-align: center;"
      ),
      plotOutput("single_igg_plot", height = "120px")
    )
  })
  
  output$single_igg_plot <- renderPlot({
    preds <- single_igg_result()
    if (is.null(preds) || any(!is.finite(unlist(preds)))) {
      return(
        ggplot() +
          annotate("text", x = 0, y = 0,
                   label = "Invalid input or calculation failed",
                   size = 5, color = "#fe4f4f") +
          theme_void()
      )
    }
    
    intervals_df <- tibble::tibble(
      Interval = factor(c("95%", "80%", "50%"),
                        levels = c("95%", "80%", "50%")),
      lower = c(preds$lower_95, preds$lower_80, preds$lower_50),
      upper = c(preds$upper_95, preds$upper_80, preds$upper_50),
      y = 1
    )
    
    x_vals <- c(preds$median, preds$lower_95, preds$upper_95,
                preds$lower_80, preds$upper_80,
                preds$lower_50, preds$upper_50)
    rng <- range(x_vals, na.rm = TRUE)
    pad <- diff(rng) * 0.08
    if (!is.finite(pad) || pad == 0) pad <- 0.2
    xlim <- c(rng[1] - pad, rng[2] + pad)
    brks <- scales::pretty_breaks(n = 6)(xlim)
    
    ggplot(intervals_df) +
      geom_errorbarh(aes(xmin = lower, xmax = upper, y = y, color = Interval),
                     height = 0, size = 3, lineend = "round") +
      geom_point(data = tibble::tibble(x = preds$median, y = 1),
                 aes(x = x, y = y), size = 5, shape = 21,
                 fill = "#fe4f4f", color = "#000000") +
      scale_x_continuous(breaks = brks, limits = xlim, expand = expansion(mult = 0.02)) +
      scale_color_manual(values = c("95%" = "#a9d1fe",
                                    "80%" = "#4facfe",
                                    "50%" = "#2a7bd1")) +
      theme_minimal() +
      theme(
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.box.just = "center",
        legend.text = element_text(size = 12),
        legend.title = element_blank(),
        legend.key.size = unit(1.2, "lines"),
        plot.caption = element_text(hjust = 0.5, size = 12, face = "bold",
                                    color = "#444444", margin = margin(t = 5))
      ) +
      labs(x = "Estimated Day-1 IgG (g/L)", y = NULL, caption = "Prediction Interval")
  })
  
  ## --- Batch Calculations -------------------------------------------------
  observeEvent(input$show_example_igg_data, {
    showModal(
      modalDialog(
        title = "Example Data Layout & Guidelines",
        size = "m",
        tags$div(
          h3("Accepted File Formats"),
          p("We accept Excel (.xls, .xlsx) and CSV files."),
          h3("Data Structure Requirements"),
          p("Each IgG observation should occupy its own row."),
          p("Each IgG value must have a corresponding column for sampling day."),
          br(),
          h4("Example Table"),
          tableOutput("batch_example_igg_table")
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })
  
  output$batch_example_igg_table <- renderTable({
    data.frame(
      "Calf ID" = c("Calf001", "Calf002", "Calf003"),
      "IgG value (g/L)" = c(23.4, 19.8, 15.2),
      "Days after calving" = as.integer(c(2, 3, 1)),
      check.names = FALSE
    )
  }, striped = TRUE, bordered = TRUE, width = "100%")
  
  batch_igg_data <- reactive({
    req(input$batch_igg_file_in)
    file <- input$batch_igg_file_in$datapath
    ext <- tools::file_ext(file)
    if (ext == "csv") read.csv(file)
    else if (ext %in% c("xls","xlsx")) readxl::read_excel(file)
    else {
      showNotification("Invalid file type", type = "error")
      return(NULL)
    }
  })
  
  output$batch_igg_submit_button_ui <- renderUI({
    if (is.null(batch_igg_data())) return(NULL)
    div(
      actionButton("batch_igg_submit_button", "Estimate", class = "submit_button"),
      style = "text-align: right;"
    )
  })
  
  output$batch_igg_column_select <- renderUI({
    if (is.null(batch_igg_data())) return(NULL)
    col_names <- colnames(batch_igg_data())
    igg_matches <- c("igg","igg value","igg_value","igg (g/l)")
    day_matches <- c("day","days","day sample taken","sample day")
    selected_igg <- col_names[which(tolower(col_names) %in% tolower(igg_matches))[1]]
    selected_day <- col_names[which(tolower(col_names) %in% tolower(day_matches))[1]]
    fluidRow(
      column(6,
             selectizeInput("batch_igg_value_column","IgG Column:",
                            selected = selected_igg %||% NULL,
                            choices = col_names,
                            options = list(placeholder="Select column..."))),
      column(6,
             selectizeInput("batch_igg_day_column","Day Column:",
                            selected = selected_day %||% NULL,
                            choices = col_names,
                            options = list(placeholder="Select column...")))
    )
  })
  
  batch_igg_data_predicted <- reactiveVal(NULL)
  
  observeEvent(input$batch_igg_submit_button, {
    req(batch_igg_data())
    withProgress(message = 'Calculating IgG Values...', value = 0, {
      df <- batch_igg_data()
      df_pred <- df %>%
        rowwise() %>%
        mutate(pred = list(predict_day1_igg(igg_model,
                                            IgG_obs = cur_data()[[input$batch_igg_value_column]],
                                            Day_obs = cur_data()[[input$batch_igg_day_column]]))) %>%
        unnest_wider(pred) %>%
        ungroup() %>%
        mutate(across(
          c(median, lower_95, upper_95, lower_80, upper_80, lower_50, upper_50),
          ~ round(.x, 1)
        )) %>%
        rename(
          "Day 1 IgG (g/L)" = median,
          "Lower 95 CI" = lower_95, "Upper 95 CI" = upper_95,
          "Lower 80 CI" = lower_80, "Upper 80 CI" = upper_80,
          "Lower 50 CI" = lower_50, "Upper 50 CI" = upper_50
        )
      batch_igg_data_predicted(df_pred)
    })
  })
  
  output$batch_igg_result_ui <- renderUI({
    req(batch_igg_data_predicted())
    tagList(
      wellPanel(
        DTOutput("batch_igg_result_table"),
        br(),
        div(
          downloadButton("batch_igg_result_download","Download",class="submit_button"),
          style = "text-align: right;"
        )
      )
    )
  })
  
  output$batch_igg_result_table <- renderDT({
    req(batch_igg_data_predicted())
    datatable(
      batch_igg_data_predicted(),
      rownames = FALSE,
      options = list(dom='tip', ordering=TRUE, scrollX=TRUE),
      selection = "none", escape = FALSE
    ) %>%
      formatStyle("Day 1 IgG (g/L)", color="#4facfe", fontWeight="bold")
  })
  
  output$batch_igg_result_download <- downloadHandler(
    filename = function() paste0("batch_igg_results_", Sys.Date(), ".csv"),
    content = function(file) {
      req(batch_igg_data_predicted())
      write.csv(batch_igg_data_predicted(), file, row.names = FALSE)
    }
  )
  
  
  # STP Tab ---------------------------------------------------------------------
  
  .valid_group <- function(x) {
    out <- suppressWarnings(as.integer(x))
    ok  <- out %in% c(1L,2L,3L)
    ifelse(ok, as.character(out), NA_character_)
  }
  
  # ---- Single Calculation ------------------------------------------------------
  single_stp_result <- reactiveVal(NULL)
  
  observeEvent(input$single_stp_submit_button, {
    if (is.null(input$single_stp_value) || is.null(input$single_stp_day) || is.null(input$single_stp_group) ||
        is.na(input$single_stp_value)   || is.na(input$single_stp_day)   || is.na(input$single_stp_group)   ||
        !is.numeric(input$single_stp_value) || !is.numeric(input$single_stp_day) ||
        input$single_stp_day < 1 || input$single_stp_day > 7 ||
        input$single_stp_value <= 0 ||
        is.na(.valid_group(input$single_stp_group))) {
      
      single_stp_result(NULL)
      showNotification("Please enter a valid STP value (>0), Day between 1 and 7, and a Colostrum group (1, 2, or 3).",
                       type = "error", duration = 5)
      
    } else {
      preds <- back_predict_day1(
        model       = stp_gam,
        day         = input$single_stp_day,
        colostrum   = .valid_group(input$single_stp_group),
        stp_measure = input$single_stp_value
      )
      single_stp_result(preds)
    }
  })
  
  output$single_stp_result_ui <- renderUI({
    out <- single_stp_result()
    req(!is.null(out), nrow(out) == 1)
    
    center_val <- out$STP_day1_pred[1]
    center_lab <- sprintf("%.2f", center_val)
    
    tagList(
      h4(
        "Est. Day 1 STP: ",
        span(center_lab, style = "display: inline; font-weight: 500; color: #4facfe"),
        style = "font-weight: 400; text-align: center;"
      ),
      plotOutput("single_stp_plot", height = "120px")
    )
  })
  
  output$single_stp_plot <- renderPlot({
    preds <- single_stp_result()
    req(!is.null(preds))
    
    has50 <- all(c("PI50_low","PI50_high") %in% names(preds))
    has95 <- all(c("PI95_low","PI95_high") %in% names(preds))

    if (!has95 && all(c("PI_low_95","PI_high_95") %in% names(preds))) {
      preds$PI95_low  <- preds$PI_low_95
      preds$PI95_high <- preds$PI_high_95
      has95 <- TRUE
    }
    
    center <- preds$STP_day1_pred[1]
    
    intervals_df <- dplyr::bind_rows(
      if (has95) tibble::tibble(Interval = factor("95%", levels = c("95%","50%")),
                                lower = preds$PI95_low[1], upper = preds$PI95_high[1], y = 1) else NULL,
      if (has50) tibble::tibble(Interval = factor("50%", levels = c("95%","50%")),
                                lower = preds$PI50_low[1], upper = preds$PI50_high[1], y = 1) else NULL
    )
    
    x_vals <- c(center,
                if (has95) c(preds$PI95_low[1], preds$PI95_high[1]),
                if (has50) c(preds$PI50_low[1], preds$PI50_high[1]))
    rng <- range(x_vals, na.rm = TRUE)
    pad <- diff(rng) * 0.08
    if (!is.finite(pad) || pad == 0) pad <- 0.2
    xlim <- c(rng[1] - pad, rng[2] + pad)
    
    brks <- scales::pretty_breaks(n = 6)(xlim)
    
    interval_colors <- c("95%" = "#a9d1fe", "50%" = "#2a7bd1")
    
    ggplot(intervals_df) +
      geom_errorbarh(aes(xmin = lower, xmax = upper, y = y, color = Interval),
                     height = 0, size = 3, lineend = "round") +
      geom_point(aes(x = center, y = 1),
                 size = 5, shape = 21, fill = "#fe4f4f", color = "#000000") +
      scale_x_continuous(breaks = brks, limits = xlim, expand = expansion(mult = 0.02)) +
      scale_color_manual(values = interval_colors, drop = FALSE) +
      labs(x = "Estimated Day-1 STP", y = NULL, caption = "Prediction Interval") +
      theme_minimal() +
      theme(
        axis.title.y = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.title = element_blank(),
        legend.text  = element_text(size = 12),
        plot.caption = element_text(hjust = 0.5, size = 12, face = "bold", color = "#444444")
      )
  })
  
  # ---- Batch Calculations ------------------------------------------------------
  observeEvent(input$show_example_stp_data, {
    showModal(
      modalDialog(
        title = "Example Data Layout & Guidelines",
        size = "m",
        tags$div(
          h3("Accepted File Formats"),
          p("We accept Excel files (.xls, .xlsx) and comma-separated values (.csv). Please ensure your file is saved in one of these formats."),
          
          h3("Data Structure Requirements"),
          p("Your data table should be organized so that each STP observation occupies its own row."),
          p("Each STP value must have a corresponding column indicating the number of days after calving when the sample was taken."),
          p("Column headers do not have to follow a specific naming convention, as you will be prompted to identify which columns correspond to the STP values and sample days during import."),
          p("After processing, your data will be returned with an additional column displaying the estimated STP value one day after calving."),
          p("Colostrum groups are coded as follows: 1) Maternal, 2) Replacer, 3) Mixed."),
          
          br(),
          h4("Example Table"),
          tableOutput("batch_example_stp_table")
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })
  
  # Sample Data
  output$batch_example_stp_table <- renderTable({
    df <- data.frame(
      "Calf ID" = c("Calf001", "Calf002", "Calf003"),
      "STP value (g/L)" = c(5.4, 4.2, 4.8),
      "Days after calving" = as.integer(c(3, 4, 2)),
      "Colostrum" = as.integer(c(1, 3, 1)),
      check.names = FALSE
    )
    df
  }, striped = TRUE, bordered = TRUE, width = "100%")
  
  batch_stp_data <- reactive({
    req(input$batch_stp_file_in)
    
    file <- input$batch_stp_file_in$datapath
    ext <- tools::file_ext(file)
    
    if (ext == "csv") {
      read.csv(file)
    } else if (ext %in% c("xls", "xlsx")) {
      readxl::read_excel(file)
    } else {
      showNotification("Invalid file type", type = "error")
      return(NULL)
    }
  })
  
  # Column select UI
  output$batch_stp_column_select <- renderUI({
    req(batch_stp_data())
    col_names <- colnames(batch_stp_data())
    
    stp_matches  <- c("stp", "stp value", "stp_value")
    day_matches  <- c("day", "days", "day sample taken", "sample day")
    grp_matches  <- c("colostrum", "group", "colostrum group")
    
    sel_stp <- col_names[which(tolower(col_names) %in% tolower(stp_matches))[1]]
    sel_day <- col_names[which(tolower(col_names) %in% tolower(day_matches))[1]]
    sel_grp <- col_names[which(tolower(col_names) %in% tolower(grp_matches))[1]]
    
    fluidRow(
      column(4,
             selectizeInput("batch_stp_value_column", "STP Column:",
                            selected = sel_stp %||% NULL, choices = col_names,
                            options = list(placeholder = "Select column...")
             )
      ),
      column(4,
             selectizeInput("batch_stp_day_column", "Day Column:",
                            selected = sel_day %||% NULL, choices = col_names,
                            options = list(placeholder = "Select column...")
             )
      ),
      column(4,
             selectizeInput("batch_stp_group_column", "Colostrum Column:",
                            selected = sel_grp %||% NULL, choices = col_names,
                            options = list(placeholder = "Select column...")
             )
      )
    )
  })
  
  output$batch_stp_submit_button_ui <- renderUI({
    if (is.null(batch_stp_data())) return(NULL)
    div(
      actionButton("batch_stp_submit_button", "Estimate", class = "submit_button"),
      style = "text-align: right;"
    )
  })
  
  batch_stp_data_predicted <- reactiveVal(NULL)
  
  observeEvent(input$batch_stp_submit_button, {
    req(batch_stp_data(), input$batch_stp_value_column, input$batch_stp_day_column, input$batch_stp_group_column)
    
    df <- batch_stp_data()
    if (any(!is.finite(suppressWarnings(as.numeric(df[[input$batch_stp_value_column]]))))) {
      showNotification("STP column has non-numeric values.", type = "error"); return(NULL)
    }
    if (any(!is.finite(suppressWarnings(as.numeric(df[[input$batch_stp_day_column]]))))) {
      showNotification("Day column has non-numeric values.", type = "error"); return(NULL)
    }
    
    withProgress(message = 'Calculating STP Day-1...', value = 0, {
      day_vec   <- as.numeric(df[[input$batch_stp_day_column]])
      stp_vec   <- as.numeric(df[[input$batch_stp_value_column]])
      group_vec <- .valid_group(df[[input$batch_stp_group_column]])
      
      ok <- is.finite(day_vec) & day_vec >= 1 & day_vec <= 7 &
        is.finite(stp_vec) & stp_vec > 0 &
        !is.na(group_vec)
      
      if (!all(ok)) {
        bad_n <- sum(!ok)
        showNotification(paste0("Skipping ", bad_n, " invalid rows (check STP>0, Day 1–7, Colostrum 1/2/3)."),
                         type = "warning", duration = 6)
      }
      
      preds <- back_predict_day_1_safe <- tryCatch({
        back_predict_day1(
          model       = stp_gam,
          day         = day_vec[ok],
          colostrum   = group_vec[ok],
          stp_measure = stp_vec[ok]
        )
      }, error = function(e) {
        showNotification(paste("Prediction failed:", e$message), type = "error", duration = 6)
        return(NULL)
      })
      req(!is.null(preds))
      
      out <- df
      if (!all(c("PI95_low","PI95_high") %in% names(preds)) &&
          all(c("PI_low_95","PI_high_95") %in% names(preds))) {
        preds <- preds %>%
          dplyr::rename(PI95_low = PI_low_95, PI95_high = PI_high_95)
      }
      has50 <- all(c("PI50_low","PI50_high") %in% names(preds))
      
      out$`Day 1 STP`    <- NA_real_
      out$`Lower 95 PI`  <- NA_real_
      out$`Upper 95 PI`  <- NA_real_
      if (has50) {
        out$`Lower 50 PI` <- NA_real_
        out$`Upper 50 PI` <- NA_real_
      }
      
      out$`Day 1 STP`[ok]   <- round(preds$STP_day1_pred, 2)
      out$`Lower 95 PI`[ok] <- round(preds$PI95_low, 2)
      out$`Upper 95 PI`[ok] <- round(preds$PI95_high, 2)
      if (has50) {
        out$`Lower 50 PI`[ok] <- round(preds$PI50_low, 2)
        out$`Upper 50 PI`[ok] <- round(preds$PI50_high, 2)
      }
      
      batch_stp_data_predicted(out)
    })
  })
  
  # Output predicted values as table
  output$batch_stp_result_ui <- renderUI({
    req(batch_stp_data_predicted())
    tagList(
      wellPanel(
        DTOutput("batch_stp_result_table"),
        br(),
        div(
          downloadButton("batch_stp_result_download", "Download", class = "submit_button"),
          style = "text-align: right;"
        )
      )
    )
  })
  
  output$batch_stp_result_table <- renderDT({
    req(batch_stp_data_predicted())
    df <- batch_stp_data_predicted()
    
    preferred <- c("Day 1 STP", "Lower 95 PI", "Upper 95 PI", "Lower 50 PI", "Upper 50 PI")
    
    show_cols <- c(setdiff(names(df), preferred), intersect(preferred, names(df)))
    df <- df[, show_cols, drop = FALSE]
    
    datatable(
      df,
      rownames = FALSE,
      options = list(dom = 'tip', ordering = TRUE, scrollX = TRUE),
      selection = "none",
      escape = FALSE
    ) %>%
      formatStyle("Day 1 STP", color = "#4facfe", fontWeight = "bold")
  })
  
  
  # Download predicted values
  output$batch_stp_result_download <- downloadHandler(
    filename = function() paste0("batch_stp_results_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(batch_stp_data_predicted(), file, row.names = FALSE)
  )
  
}
