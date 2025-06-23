function(input, output, session) {
  
  # Initiate Reactive Values ------------------------------------------------
  general <- reactiveVal(general)
  
  short_diar <- reactiveVal(short_diar)
  long_diar <- reactiveVal(long_diar)
  simple_diar_table <- reactiveVal(NULL)
  detailed_diar_table <- reactiveVal(NULL)
  
  short_pna <- reactiveVal(short_pna)
  long_pna <- reactiveVal(long_pna)
  simple_pna_table <- reactiveVal(NULL)
  detailed_pna_table <- reactiveVal(NULL)
  
  observe({
    current_general <- general()
    current_general["num_calvings"] <- input$num_calvings
    current_general["perc_hefers"] <- input$perc_hefers
    general(current_general)
    
    current_short_diar <- short_diar()
    current_short_diar["perc_prev"] <- input$perc_short_diar
    short_diar(current_short_diar)
    
    current_long_diar <- long_diar()
    current_long_diar["perc_prev"] <- input$perc_long_diar
    long_diar(current_long_diar)
    
    current_short_pna <- short_pna()
    current_short_pna["perc_prev"] <- input$perc_short_pna
    short_pna(current_short_pna)
    
    current_long_pna <- long_pna()
    current_long_pna["perc_prev"] <- input$perc_long_pna
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
      seq_len(nrow(simple_pna_table)) == 8,
      simple_pna_table[, 1],
      scales::dollar(as.numeric(simple_pna_table[, 1]))
    )
    
    simple_pna_table[, 3] <- ifelse(
      seq_len(nrow(simple_pna_table)) == 8,
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
      seq_len(nrow(detailed_pna_table)) %in% c(3,7,8,12,13,17,21,242),
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
  
  
  # IgG Tab -----------------------------------------------------------------
  
  
  ## Formula for Predicting Day 1 IgG ---------------------------------------
  
  calculate_day_1_igg <- function(IgG_obs, Day_obs) {
    #posterior_draws <- posterior::as_draws_df(fit)
    
    beta_intercept <- posterior_draws$b_Intercept
    beta_day       <- posterior_draws$b_Day
    sigma          <- posterior_draws$sigma
    sd_intercept   <- posterior_draws$sd_CalfID__Intercept
    sd_slope       <- posterior_draws$sd_CalfID__Day
    
    n_draws <- length(beta_intercept)
    
    re_intercept <- rnorm(n_draws, 0, sd_intercept)
    re_slope     <- rnorm(n_draws, 0, sd_slope)
    
    log_IgG_pred <- beta_intercept + re_intercept +
      (beta_day + re_slope) * 1 + 
      rnorm(n_draws, 0, sigma) + 
      (log(IgG_obs) - (beta_intercept + beta_day * Day_obs))
    
    IgG_pred <- exp(log_IgG_pred)
    
    tibble(
      median = median(IgG_pred),
      lower_95 = quantile(IgG_pred, 0.025),
      upper_95 = quantile(IgG_pred, 0.975),
      lower_80 = quantile(IgG_pred, 0.10),
      upper_80 = quantile(IgG_pred, 0.90),
      lower_50 = quantile(IgG_pred, 0.25),
      upper_50 = quantile(IgG_pred, 0.75)
    )
  }
  
  ## Single Calculation -----------------------------------------------------
  single_igg_result <- reactiveVal(NULL)
  
  observeEvent(input$single_igg_submit_button, {

    if (is.null(input$single_igg_value) || is.null(input$single_igg_day) ||
        is.na(input$single_igg_value) || is.na(input$single_igg_day) ||
        !is.numeric(input$single_igg_value) || !is.numeric(input$single_igg_day) ||
        input$single_igg_day < 1 || input$single_igg_day > 7 || input$single_igg_value <= 0) {
      
      single_igg_result(NULL)
      showNotification("Please enter a valid IgG value (>0) and Day between 1 and 7.", type = "error", duration = 5)
      
    } else {
      
      x <- calculate_day_1_igg(
        IgG_obs = input$single_igg_value,
        Day_obs = input$single_igg_day
      )
      single_igg_result(x)
    }
    
  })
  
  output$single_igg_result_ui <- renderUI({
    output_value <- single_igg_result()
    
    req(!is.null(output_value))
    req(!is.na(output_value))
    
    tagList(
      h4(
        "Est. IgG on Day 1: ",  
        span(paste(round(output_value$median, 1), "g/L"), style = "display: inline; font-weight: 500; color: #4facfe"),
        style = "font-weight: 400; text-align: center;"
      ),
      plotOutput("single_igg_plot", height = "120px")
    )
  })
  
  output$single_igg_plot <- renderPlot({
    preds <- single_igg_result()
    
    if (is.null(preds) || any(!is.finite(unlist(preds)))) {
      return(ggplot() + 
               annotate("text", x = 0, y = 0, label = "Invalid input or calculation failed", size = 5, color = "red") + 
               theme_void())
    }
    
    intervals_df <- tibble(
      Interval = factor(c("95%", "80%", "50%"), 
                        levels = c("95%", "80%", "50%")),
      lower = c(preds$lower_95, preds$lower_80, preds$lower_50),
      upper = c(preds$upper_95, preds$upper_80, preds$upper_50),
      y = c(1, 1, 1)
    )
    
    interval_colors <- c("95%" = "#a9d1fe", "80%" = "#4facfe", "50%" = "#2a7bd1")
    
    ggplot(intervals_df) +
      geom_errorbarh(aes(xmin = lower, xmax = upper, y = y, color = Interval), height = 0, size = 2) +
      geom_point(data = tibble(x = preds$median, y = 1), aes(x = x, y = y), size = 5, shape = 21, fill = "#fe4f4f", color = "black") +
      scale_color_manual(values = interval_colors) +
      theme_minimal() +
      theme(
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.x = element_text(size = 10),
        panel.grid.major.y = element_blank(),
        panel.grid.minor.y = element_blank(),
        legend.position = "bottom",
        legend.box = "horizontal",
        legend.box.just = "center",
        legend.text = element_text(size = 12),
        legend.key.size = unit(1.2, "lines"),
        plot.caption = element_text(hjust = 0.5, size = 12, face = "bold", color = "#444444", margin = margin(t = 5))
      ) +
      guides(color = guide_legend(nrow = 1, byrow = TRUE, title = NULL)) +   # no title
      labs(caption = "Prediction Interval") +
      coord_cartesian(ylim = c(0.94, 1.05))
    

  })
  
  ## Batch Calculations -----------------------------------------------------
  # Help text
  observeEvent(input$show_example_data, {
    showModal(
      modalDialog(
        title = "Example Data Layout & Guidelines",
        size = "m",
        tags$div(
          h3("Accepted File Formats"),
          p("We accept Excel files (.xls, .xlsx) and comma-separated values (.csv). Please ensure your file is saved in one of these formats."),
          
          h3("Data Structure Requirements"),
          p("Your data table should be organized so that each IgG observation occupies its own row. "),
          p("Each IgG value must have a corresponding column indicating the number of days after calving when the sample was taken."),
          p("Column headers do not have to follow a specific naming convention, as you will be prompted to identify which columns correspond to the IgG values and sample days during import."),
          p("After processing, your data will be returned with an additional column displaying the estimated IgG value one day after calving."),
          
          br(),
          h4("Example Table"),
          tableOutput("batch_example_table")
        ),
        easyClose = TRUE,
        footer = modalButton("Close")
      )
    )
  })
  
  # Sample Data
  output$batch_example_table <- renderTable({
    df <- data.frame(
      "Calf ID" = c("Calf001", "Calf002", "Calf003"),
      "IgG value (g/L)" = c(23.4, 19.8, 15.2),
      "Days after calving" = as.integer(c(2, 3, 1)),
      check.names = FALSE
    )
    df
  }, striped = TRUE, bordered = TRUE, width = "100%")
  
  batch_igg_data <- reactive({
    req(input$batch_igg_file_in)
    
    file <- input$batch_igg_file_in$datapath
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
  
  # Conditional submit button
  output$batch_igg_submit_button_ui <- renderUI({
    if (is.null(batch_igg_data())) return(NULL)
    
    div(
      actionButton("batch_igg_submit_button", "Estimate", class = "submit_button"),
      style = "text-align: right;"
    )
  })
  
  # Column name select UI
  output$batch_igg_column_select <- renderUI({
    if (is.null(batch_igg_data())) return(NULL)
    
    col_names <- colnames(batch_igg_data())
    
    igg_matches <- c("igg", "igg value", "igg_value", "igg (g/l)")
    day_matches <- c("day", "days", "day sample taken", "sample day")
    
    selected_igg <- col_names[which(tolower(col_names) %in% tolower(igg_matches))[1]]
    selected_day <- col_names[which(tolower(col_names) %in% tolower(day_matches))[1]]
    
    fluidRow(
      column(6,
             selectizeInput(
               "batch_igg_value_column", 
               "IgG Column:", 
               selected = selected_igg %||% NULL, 
               choices = col_names,
               options = list(placeholder = "Select column...")
             )
      ),
      column(6,
             selectizeInput(
               "batch_igg_day_column", 
               "Day Column:", 
               selected = selected_day %||% NULL, 
               choices = col_names,
               options = list(placeholder = "Select column...")
             )
      )
    )
  })
  
  # Calculate day 1 values
  batch_igg_data_predicted <- reactiveVal(NULL)
  
  observeEvent(input$batch_igg_submit_button, {
    req(batch_igg_data())
    
    withProgress(message = 'Calculating IgG Values...', value = 0, {
    
    df <- batch_igg_data()
    
    df_pred <- df %>%
      rowwise() %>%
      mutate(pred = list(calculate_day_1_igg(
        IgG_obs = cur_data()[[input$batch_igg_value_column]],
        Day_obs = cur_data()[[input$batch_igg_day_column]]
      ))) %>%
      unnest_wider(pred) %>%
      ungroup() %>%
      mutate(across(
        c(median, lower_95, upper_95, lower_80, upper_80, lower_50, upper_50),
        ~ round(.x, 1)
      )) %>%
      rename(
        "Day 1 IgG (g/L)" = median,
        "Lower 95 CI" = lower_95,
        "Upper 95 CI" = upper_95,
        "Lower 80 CI" = lower_80,
        "Upper 80 CI" = upper_80,
        "Lower 50 CI" = lower_50,
        "Upper 50 CI" = upper_50
      )
    
    batch_igg_data_predicted(df_pred)
    })
  })
  
  # Output predcited values as table
  output$batch_igg_result_ui <- renderUI({
    req(batch_igg_data_predicted())
    
    tagList(
      wellPanel(
        DTOutput("batch_igg_result_table"),
        br(),
        div(
          downloadButton("batch_igg_result_download", "Download", class = "submit_button"),
          style = "text-align: right;"
        )
      )
    )
  })
  
  output$batch_igg_result_table <- renderDT({
    req(batch_igg_data_predicted())
    
    df <- batch_igg_data_predicted()
    
    datatable(
      df,
      rownames = FALSE,
      options = list(
        dom = 'tip',
        ordering = TRUE,
        scrollX = TRUE
      ),
      selection = "none",
      escape = FALSE
    ) %>%
      formatStyle(
        "Day 1 IgG (g/L)",
        color = "#4facfe",
        fontWeight = "bold"
      )
    
  })
  
  # Download predicted values
  output$batch_igg_result_download <- downloadHandler(
    filename = function() {
      paste0("batch_igg_results_", Sys.Date(), ".csv")
    },
    content = function(file) {
      req(batch_igg_data_predicted())
      write.csv(batch_igg_data_predicted(), file, row.names = FALSE)
    }
  )
  
}
