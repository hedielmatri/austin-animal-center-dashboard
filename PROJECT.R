# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(lubridate)
library(GSODR)
library(tibble)


# Read data
Intakes = read.csv("Austin_Animal_Center_Intakes_(10_01_2013_to_05_05_2025)_20251112.csv")
Outcomes = read.csv("Austin_Animal_Center_Outcomes_(10_01_2013_to_05_05_2025)_20251112.csv")

# Get weather data
Weather = get_GSOD(station = "722540-13904", years = 2013:2025)
Weather = Weather %>% mutate(YEARMODA = as.Date(YEARMODA))

# Clean and join
All_Pairs = inner_join(Intakes, Outcomes, by = "Animal.ID")
All_Pairs$Intake.Date = mdy_hms(All_Pairs$DateTime.x)
All_Pairs$Outcome.Date = ymd_hms(All_Pairs$DateTime.y)
Valid_Pairs = All_Pairs %>% filter(Outcome.Date > Intake.Date)
Clean_Data = Valid_Pairs %>% group_by(Animal.ID, Intake.Date) %>% slice_min(order_by = Outcome.Date, n = 1) %>% ungroup()
Clean_Data$YEARMODA = as.Date(Clean_Data$Intake.Date)
Dataset = inner_join(Clean_Data, Weather, by = "YEARMODA")

# Feature engineering
Dataset$Intake.Month = month(Dataset$Intake.Date)

Season = function(months){
  d = character(length(months))
  for (i in 1:length(months)){
    spe <- months[i]
    if (is.na(spe)){ d[i] <- NA }
    else if (spe %in% c(12, 1, 2)){ 
      d[i] <- "Winter" 
    }
    else if (spe %in% c(3, 4, 5)){ 
      d[i] <- "Spring" 
    }
    else if (spe %in% c(6, 7, 8)){ 
      d[i] <- "Summer" 
    }
    else{ 
      d[i] <- "Fall" 
    }
  }
  return(d)
}

Dataset = Dataset %>% mutate(Season.of.Intake = Season(Intake.Month))
Dataset$Birth.Date = as.Date(Dataset$Date.of.Birth)
Dataset$Age = round(pmax(0, as.numeric(difftime(as.Date(Dataset$Intake.Date), Dataset$Birth.Date, units = "days")) / 30), 2)
Dataset$Intake.Condition = na_if(Dataset$Intake.Condition, "")
Dataset$Days.in.Shelter = as.numeric(difftime(Dataset$Outcome.Date, Dataset$Intake.Date, units = "days"))

# Final selection
ACP = Dataset %>% select(Animal_Type = Intake.Type, Health_Condition = Intake.Condition, Outcome = Outcome.Type, Intake_Season = Season.of.Intake, Age_in_Months = Age, Days_in_Shelter = Days.in.Shelter, Avg_Temp_on_Intake = TEMP)
ACP = ACP %>% mutate(Animal_Type = as.factor(Animal_Type), Health_Condition = as.factor(Health_Condition), Outcome = as.factor(Outcome), Intake_Season = as.factor(Intake_Season)) 


# UI
ui = page_sidebar(
  theme = bs_theme(version = 5),
  title = "Austin Animal Center Analytics",
  
  sidebar = sidebar(
    title = "Controls",
    
    # Global Variable Selector
    selectizeInput("vars", "Select Variables to Plot:", 
                   choices = names(ACP), 
                   multiple = TRUE, 
                   selected = c("Animal_Type", "Outcome"),
                   options = list(maxItems = 3)),
    hr(),
    
    # FILTERS
    sliderInput("age_filter", "Filter by Age (Months):", 
                min = 0, max = max(ACP$Age_in_Months, na.rm=TRUE), 
                value = c(0, max(ACP$Age_in_Months, na.rm=TRUE))),
    
    selectInput("intake_filter", "Filter by Intake Category:", 
                choices = c("All", sort(as.character(unique(ACP$Animal_Type))))),
    
    hr(),
    selectInput("plot_color", "Graph Color:",
                choices = c("Blue" = "#027BC2", "Green" = "#18bc9c", "Red" = "#e74c3c")),
    checkboxInput("log_scale", "Log Transform Y-Axis", value = FALSE)
  ),
  
  layout_columns(
    fill = FALSE,
    value_box(title = "Records", value = textOutput("kpi_n"), showcase = bsicons::bs_icon("database")),
    value_box(title = "Avg Days Shelter", value = textOutput("kpi_days"), showcase = bsicons::bs_icon("calendar3")),
    value_box(title = "Adoption Rate", value = textOutput("kpi_adopt"), showcase = bsicons::bs_icon("heart-fill"), theme = "success")
  ),
  
  navset_card_underline(
    nav_panel("Visualization", 
              card_body(
                textOutput("plot_desc"),
                plotlyOutput("main_plot", height = "550px")
              )
    ),
    
    nav_panel("Prediction",
              layout_columns(
                col_widths = c(4, 8),
                
                # SIMULATOR INPUTS
                card(
                  card_header("Adoption Simulator"),
                  helpText("Predict outcome based on your specific columns."),
                  
                  selectInput("sim_animal", "Animal Type:", 
                              choices = sort(unique(as.character(ACP$Animal_Type)))),
                  selectInput("sim_cond", "Health Condition:", 
                              choices = sort(unique(as.character(ACP$Health_Condition)))),
                  selectInput("sim_season", "Intake Season:", 
                              choices = sort(unique(as.character(ACP$Intake_Season)))),
                  numericInput("sim_age", "Age (Months):", value = 12, min = 0),
                  
                  hr(),
                  actionButton("calc_pred", "Calculate Probability", class = "btn-primary w-100")
                ),
                
                # RESULTS
                card(
                  card_header("Prediction Results"),
                  layout_columns(
                    value_box(title = "Outcome Prediction:", value = uiOutput("pred_badge")),
                    value_box(title = "Adoption Probability:", value = textOutput("pred_prob"), theme = "primary")
                  ),
                  hr(),
                  h5("Most Influential Factors"),
                  plotOutput("importance_plot", height = "300px")
                )
              )
    ),
    
    nav_panel("Statistics",
              card(
                card_header("Variable Summaries"),
                helpText("Variables selected in the sidebar are highlighted in blue."),
                uiOutput("stats_summary")
              )
    ),
    
    nav_panel("Time Trends", plotlyOutput("trend_plot", height = "500px")),
    nav_panel("Raw Data", DTOutput("raw_table"))
  )
)


# SERVER

server = function(input, output, session) {
  
  # Filtering Logic
  filtered_data = reactive({
    req(input$age_filter)
    df = ACP
    df = df %>% filter(Age_in_Months >= input$age_filter[1], Age_in_Months <= input$age_filter[2])
    if (input$intake_filter != "All") {
      df = df %>% filter(Animal_Type == input$intake_filter)
    }
    df
  })
  
  # KPI
  output$kpi_n = renderText({ format(nrow(filtered_data()), big.mark=",") })
  output$kpi_days = renderText({ round(mean(filtered_data()$Days_in_Shelter, na.rm=TRUE), 1) })
  output$kpi_adopt = renderText({
    if("Adoption" %in% filtered_data()$Outcome) {
      rate = mean(filtered_data()$Outcome == "Adoption", na.rm=TRUE) * 100
      paste0(round(rate, 1), "%")
    } else {
      "0% (No Adoptions)"
    }
  })
  
  # Visualization
  output$plot_desc = renderText({
    req(input$vars)
    paste("Analysis of:", paste(input$vars, collapse = " vs "))
  })
  
  output$main_plot = renderPlotly({
    req(input$vars)
    data = filtered_data()
    validate(need(nrow(data) > 0, "No data matches filters."))
    if(nrow(data) > 5000) data = sample_n(data, 5000)
    
    p = ggplot(data) + theme_minimal()
    col = input$plot_color
    vars = input$vars
    
    # 1 variable
    if (length(vars) == 1) {
      x = vars[1]
      if(is.numeric(data[[x]])) p = p + geom_histogram(aes(x=.data[[x]]), fill=col, color="white", bins=30)
      else p = p + geom_bar(aes(x=.data[[x]]), fill=col)
    } 
    # 2 variables
    else if (length(vars) == 2) {
      x=vars[1]
      y=vars[2]
      if(is.numeric(data[[x]]) & is.numeric(data[[y]])) 
        p = p + geom_point(aes(x=.data[[x]], y=.data[[y]]), alpha=0.5, color=col)
      else if(is.factor(data[[x]]) & is.numeric(data[[y]])) 
        p = p + geom_boxplot(aes(x=.data[[x]], y=.data[[y]]), fill=col)
      else if(is.numeric(data[[x]]) & is.factor(data[[y]])) 
        p = p + geom_boxplot(aes(x=.data[[y]], y=.data[[x]]), fill=col) + coord_flip()
      else 
        p = p + geom_bar(aes(x=.data[[x]], fill=.data[[y]]), position="fill") + scale_fill_viridis_d()
    }
    # 3 variables
    else if (length(vars) == 3) {
      x=vars[1]
      y=vars[2]
      z=vars[3]
      if(is.numeric(data[[x]]) & is.numeric(data[[y]]))
        p = p + geom_point(aes(x=.data[[x]], y=.data[[y]], color=.data[[z]]), alpha=0.5)
      else
        p = p + geom_jitter(aes(x=.data[[x]], y=.data[[y]], color=.data[[z]]), alpha=0.5)
    }
    
    if(input$log_scale) p = p + scale_y_log10()
    ggplotly(p)
  })
  
  # Prediction
  model = reactive({
    df = filtered_data()
    needed_cols = c("Outcome", "Animal_Type", "Age_in_Months", "Health_Condition", "Intake_Season")
    valid_cols = intersect(names(df), needed_cols)
    if(!"Outcome" %in% valid_cols) return(NULL)
    
    df = df %>% select(all_of(valid_cols)) %>% na.omit()
    if(nrow(df) < 20) return(NULL)
    
    df$Target = factor(ifelse(df$Outcome == "Adoption", "Adopted", "Other"))
    if(length(unique(df$Target)) < 2) return(NULL)
    
    rpart(Target ~ ., data = df %>% select(-Outcome), method = "class")
  })
  
  prediction_vals = reactive({
    input$calc_pred
    isolate({
      fit = model()
      if(is.null(fit)) return(list(prob = 0, label = "Insufficient Data"))
      
      new_case = data.frame(
        Animal_Type = input$sim_animal,
        Health_Condition = input$sim_cond,
        Intake_Season = input$sim_season,
        Age_in_Months = input$sim_age,
        stringsAsFactors = FALSE
      )
      
      prob = tryCatch({
        predict(fit, new_case, type = "prob")[,"Adopted"]
      }, error = function(e) return(0))
      
      label = ifelse(prob > 0.5, "Likely Adoption", "Unlikely Adoption")
      return(list(prob = prob, label = label))
    })
  })
  
  output$pred_prob = renderText({ paste0(round(prediction_vals()$prob * 100, 1), "%") })
  output$pred_badge = renderUI({
    val = prediction_vals()
    color = if(val$prob > 0.5) "success" else "danger"
    span(class = paste0("badge bg-", color), val$label, style="font-size: 1.2rem;")
  })
  
  output$importance_plot = renderPlot({
    fit = model()
    if(is.null(fit)) {
      par(mar=c(0,0,0,0)); plot(c(0,1),c(0,1),ann=F,bty='n',type='n',xaxt='n',yaxt='n')
      text(0.5,0.5,"Not enough data", cex=1.5)
      return()
    }
    imp = fit$variable.importance
    df_imp = data.frame(Variable = names(imp), Importance = imp)
    ggplot(df_imp, aes(x=reorder(Variable, Importance), y=Importance)) + geom_col(fill = input$plot_color) + coord_flip() + theme_minimal() + labs(x="", y="Importance")
  })
  

  # Statistics 
  output$stats_summary = renderUI({
    df = filtered_data()
    selected_vars = input$vars
    
    ui_elements = lapply(names(df), function(col_name) {
      
      sum_text = paste(capture.output(summary(df[[col_name]])), collapse = "\n")
      
      is_selected = col_name %in% selected_vars
      
      card_class = if(is_selected) "border-primary mb-3 shadow-sm" else "mb-3"
      header_class = if(is_selected) "bg-primary text-white" else "bg-light"
      
      div(
        class = paste("card", card_class),
        div(class = paste("card-header fw-bold", header_class), col_name),
        div(class = "card-body",
            pre(sum_text, style = "background-color: transparent; border: none;")
        )
      )
    })
    
    do.call(tagList, ui_elements)
  })
  
  # Trends
  output$trend_plot = renderPlotly({
    df = filtered_data()
    trend = df %>% count(Intake_Year, Animal_Type)
    p = ggplot(trend, aes(x=Intake_Year, y=n, color=Animal_Type)) + geom_line(linewidth=1) + geom_point() + theme_minimal()
    ggplotly(p)
  })
  
  # Data 
  output$raw_table = renderDT({
    datatable(filtered_data(), options = list(pageLength = 10))
  })
}

shinyApp(ui, server)