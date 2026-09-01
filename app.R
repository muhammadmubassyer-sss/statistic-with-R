library(shiny)
library(ggplot2)
library(readxl)
library(DT)
library(dplyr)
library(tidyr)
install.packages("shinylive")
install.packages("httpuv")
library(shinylive)
library(httpuv)

# 1. USER INTERFACE (UI)
ui <- navbarPage(
  title = "Data Analysis Dashboard",
  
  # Module 1: Data Input & Management
  tabPanel("1. Data Input",
           sidebarLayout(
             sidebarPanel(
               fileInput("file_upload", "Choose CSV or Excel File",
                         accept = c(".csv", ".xlsx", ".xls")),
               hr(),
               h4("Dataset Summary"),
               verbatimTextOutput("data_summary_text")
             ),
             mainPanel(
               h4("Interactive Data Preview"),
               DTOutput("data_preview")
             )
           )
  ),
  
  # Module 2: Descriptive Statistics Engine
  tabPanel("2. Descriptive Stats",
           sidebarLayout(
             sidebarPanel(
               h4("Variable Selection"),
               checkboxGroupInput("num_var", "Select Quantitative Variables (Select multiple to compare):", choices = NULL),
               hr(),
               selectInput("cat_var", "Select Qualitative Variable:", choices = NULL)
             ),
             mainPanel(
               h4("Quantitative Data Summary"),
               tableOutput("num_summary_table"),
               hr(),
               h4("Qualitative Data Summary"),
               DTOutput("cat_summary_table")
             )
           )
  ),
  
# Module 3: Dynamic Data Visualization (Updated with Multi-Variable Boxplot)
tabPanel("3. Visualizations",
         sidebarLayout(
           sidebarPanel(
             h4("Plot Options"),
             tabsetPanel(
               id = "viz_tab",
               # Existing Single-Variable Tabs (keep these)
               tabPanel("Quantitative",
                        br(),
                        selectInput("viz_num_var", "Select Variable:", choices = NULL),
                        selectInput("viz_num_type", "Plot Type:", choices = c("Histogram", "Density Curve")),
                        conditionalPanel(
                          condition = "input.viz_num_type == 'Histogram'",
                          sliderInput("bin_width", "Bin Width:", min = 0.1, max = 50, value = 5, step = 0.5)
                        ),
                        selectInput("num_color", "Bar / Fill Color:", 
                                    choices = c("Steel Blue" = "steelblue", 
                                                "Coral" = "coral", 
                                                "Forest Green" = "forestgreen", 
                                                "Purple" = "purple"))
               ),
               tabPanel("Qualitative",
                        br(),
                        selectInput("viz_cat_var", "Select Variable:", choices = NULL),
                        selectInput("viz_cat_type", "Plot Type:", choices = c("Bar Chart", "Pie Chart")),
                        selectInput("cat_palette", "Color Palette:", 
                                    choices = c("Set1", "Set2", "Set3", "Pastel1", "Dark2"))
               ),
               # NEW: Multi-Variable Boxplot Tab with Checkboxes
               tabPanel("Boxplot (Comparison)",
                        br(),
                        checkboxGroupInput("viz_box_vars", "Select Variables to Compare:", choices = NULL),
                        helpText("Check multiple variables to see them compared side-by-side.")
               )
             )
           ),
           mainPanel(
             # Dynamically show either the distribution plots OR the comparative boxplot
             conditionalPanel(
               condition = "input.viz_tab == 'Quantitative' || input.viz_tab == 'Qualitative'",
               plotOutput("dist_plot", height = "500px")
             ),
             conditionalPanel(
               condition = "input.viz_tab == 'Boxplot (Comparison)'",
               plotOutput("boxplot_plot", height = "500px")
             )
           )
         )
),
  
  # Module 4: Automated Hypothesis Testing
  tabPanel("4. Hypothesis Testing",
           sidebarLayout(
             sidebarPanel(
               h4("Hypothesis Test Setup"),
               radioButtons("test_type", "Sample Structure:",
                            choices = c("One-Sample", "Two-Sample")),
               uiOutput("mod4_ui_vars"),
               
               hr(),
               h4("Test Logic (Z vs T)"),
               radioButtons("zt_logic", "Is Population Std. Deviation known?",
                            choices = c("No (Use T-Test)", "Yes (Use Z-Test)")),
               
               hr(),
               h4("Parameters"),
               conditionalPanel(
                 condition = "input.test_type == 'One-Sample'",
                 numericInput("null_mu", "Null Hypothesis Mean (mu):", value = 0, step = 0.1)
               ),
               conditionalPanel(
                 condition = "input.zt_logic == 'Yes (Use Z-Test)' && input.test_type == 'One-Sample'",
                 numericInput("sigma1", "Known Population SD (sigma):", value = 1, min = 0.0001)
               ),
               conditionalPanel(
                 condition = "input.zt_logic == 'Yes (Use Z-Test)' && input.test_type == 'Two-Sample'",
                 numericInput("sigma1_2", "Known Population SD (Group 1):", value = 1, min = 0.0001),
                 numericInput("sigma2_2", "Known Population SD (Group 2):", value = 1, min = 0.0001)
               )
             ),
             mainPanel(
               h4("Diagnostic Output"),
               verbatimTextOutput("htest_output")
             )
           )
  ),
  
  # Module 5: Relationship Modeling & Regression
  tabPanel("5. Regression Modeling",
           tabsetPanel(
             tabPanel("Correlation Analysis",
                      br(),
                      sidebarLayout(
                        sidebarPanel(
                          selectInput("corr_x", "Select X Variable:", choices = NULL),
                          selectInput("corr_y", "Select Y Variable:", choices = NULL),
                          radioButtons("corr_method", "Correlation Method:",
                                       choices = c("Pearson" = "pearson", "Spearman" = "spearman"))
                        ),
                        mainPanel(
                          h4("Correlation Result"),
                          verbatimTextOutput("corr_text"),
                          hr(),
                          plotOutput("corr_plot", height = "400px")
                        )
                      )
             ),
             tabPanel("Linear Regression",
                      br(),
                      sidebarLayout(
                        sidebarPanel(
                          # New multi-selection checkbox UI
                          selectInput("reg_y", "Select Dependent Variable (Y):", choices = NULL),
                          checkboxGroupInput("reg_x", "Select Independent Variables (X):", choices = NULL)
                        ),
                        mainPanel(
                          h4("Formatted Linear Regression Equation"),
                          verbatimTextOutput("reg_equation"),
                          hr(),
                          h4("Model Performance & Coefficients"),
                          verbatimTextOutput("reg_summary"),
                          hr(),
                          h4("Diagnostic Residual Plots"),
                          plotOutput("reg_plot", height = "500px") # <--- Make sure height is set and ID is "reg_plot"
                        )
                      )
             )
           )
  ),
  
  # Built-In User Guide (Rubric Criteria 4.1 & 5.1 - 10 Marks)
  tabPanel("User Guide",
           fluidRow(
             column(12,
                    h2("User Onboarding Manual & Module Instructions"),
                    p("Welcome to the Data Analysis Dashboard! This application enables non-technical users to perform full data analysis without writing code."),
                    hr(),
                    h3("Step 1: Data Input & Management (Tab 1)"),
                    p("• Click 'Browse...' to upload any custom dataset in .csv or .xlsx format."),
                    p("• View the automatically calculated metadata (Total Rows, Columns, Missing NA counts)."),
                    p("• Interact with the data preview table to sort or filter rows."),
                    
                    h3("Step 2: Descriptive Statistics Engine (Tab 2)"),
                    p("• Select quantitative variables to view mean, median, standard deviation, quartiles, and min/max stats."),
                    p("• Select qualitative variables to view frequency counts and category percentages."),
                    
                    h3("Step 3: Dynamic Data Visualization (Tab 3)"),
                    p("• Choose 'Quantitative' to plot Histograms or Density Curves with dynamic bin width adjustment."),
                    p("• Choose 'Qualitative' to view color-customized Bar Charts or Pie Charts."),
                    
                    h3("Step 4: Automated Hypothesis Testing (Tab 4)"),
                    p("• Toggle between One-Sample and Two-Sample test paradigms."),
                    p("• Choose whether Population SD is known to run either a Z-Test or T-Test automatically."),
                    
                    h3("Step 5: Relationship Modeling & Regression (Tab 5)"),
                    p("• Compute Pearson or Spearman correlation coefficients alongside fitted scatterplots."),
                    p("• Run linear regression models to view coefficients, R² values, formatted equations (Y = β₀ + β₁X), and diagnostic residual plots.")
             )
           )
  )
)

# 2. SERVER LOGIC (The Brain)
server <- function(input, output, session) {
  
  # Reactive dataset loader
  user_data <- reactive({
    req(input$file_upload)
    
    ext <- tools::file_ext(input$file_upload$name)
    
    data <- switch(ext,
                   csv = read.csv(input$file_upload$datapath, stringsAsFactors = FALSE),
                   xlsx = readxl::read_excel(input$file_upload$datapath),
                   xls = readxl::read_excel(input$file_upload$datapath),
                   validate("Invalid file format. Please upload a .csv or .xlsx file.")
    )
    return(data)
  })
  
  # Module 1 Outputs
  output$data_summary_text <- renderText({
    req(user_data())
    df <- user_data()
    paste0("Total Rows: ", nrow(df), "\n",
           "Total Columns: ", ncol(df), "\n",
           "Missing Values (NAs): ", sum(is.na(df)))
  })
  
  output$data_preview <- renderDT({
    req(user_data())
    datatable(user_data(), options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Dropdown Updater across all modules
  observeEvent(user_data(), {
    df <- user_data()
    
    num_cols <- names(df)[sapply(df, is.numeric)]
    cat_cols <- names(df)[sapply(df, function(col) is.character(col) || is.factor(col) || is.logical(col))]
    
    first_num <- if (length(num_cols) > 0) num_cols[1] else NULL
    second_num <- if (length(num_cols) > 1) num_cols[2] else first_num
    first_cat <- if (length(cat_cols) > 0) cat_cols[1] else NULL
    
    updateCheckboxGroupInput(session, "num_var", choices = num_cols, selected = num_cols[1:min(3, length(num_cols))])
    updateSelectInput(session, "cat_var", choices = cat_cols, selected = first_cat)
    
    updateSelectInput(session, "viz_num_var", choices = num_cols, selected = first_num)
    updateSelectInput(session, "viz_cat_var", choices = cat_cols, selected = first_cat)
    updateCheckboxGroupInput(session, "viz_box_vars", choices = num_cols, selected = num_cols[1:min(3, length(num_cols))])
    
    updateSelectInput(session, "corr_x", choices = num_cols, selected = first_num)
    updateSelectInput(session, "corr_y", choices = num_cols, selected = second_num)
    
    updateSelectInput(session, "reg_y", choices = num_cols, selected = first_num)
    updateCheckboxGroupInput(session, "reg_x", choices = num_cols, selected = num_cols[2:min(4, length(num_cols))])
  })
  
  # Dynamic Bin Width (Module 3)
  observeEvent(input$viz_num_var, {
    req(user_data(), input$viz_num_var)
    if (input$viz_num_var == "") return()
    
    vec <- user_data()[[input$viz_num_var]]
    vec <- vec[!is.na(vec)]
    
    if (length(vec) > 0) {
      data_range <- max(vec) - min(vec)
      if (data_range > 0) {
        default_bw <- max(round(data_range / 30, 2), 0.1)
        max_bw <- max(round(data_range / 5, 2), 1)
        min_bw <- max(round(data_range / 100, 2), 0.01)
        updateSliderInput(session, "bin_width", min = min_bw, max = max_bw, value = default_bw, step = min_bw)
      }
    }
  })
  
  # Module 2 Outputs
  output$num_summary_table <- renderTable({
    validate(need(input$file_upload, "Please upload a dataset in Tab 1 first."))
    req(input$num_var)
    
    df <- user_data()
    selected_cols <- input$num_var
    
    # Build a summary dataframe for all selected columns side-by-side
    summary_list <- lapply(selected_cols, function(col_name) {
      vec <- df[[col_name]]
      c(
        Mean = round(mean(vec, na.rm = TRUE), 4),
        Median = round(median(vec, na.rm = TRUE), 4),
        `Std. Deviation` = round(sd(vec, na.rm = TRUE), 4),
        Minimum = round(min(vec, na.rm = TRUE), 4),
        Maximum = round(max(vec, na.rm = TRUE), 4),
        `Q1 (25%)` = round(quantile(vec, 0.25, na.rm = TRUE), 4),
        `Q3 (75%)` = round(quantile(vec, 0.75, na.rm = TRUE), 4),
        `Missing Count` = sum(is.na(vec))
      )
    })
    
    # Combine into a clean matrix/table
    res_df <- as.data.frame(do.call(cbind, summary_list))
    colnames(res_df) <- selected_cols
    res_df <- cbind(Statistic = rownames(res_df), res_df)
    rownames(res_df) <- NULL
    
    return(res_df)
  }, striped = TRUE, hover = TRUE, bordered = TRUE)
  
  output$cat_summary_table <- renderDT({
    validate(need(input$file_upload, "Please upload a dataset in Tab 1 first."))
    req(input$cat_var)
    vec <- user_data()[[input$cat_var]]
    
    freq_table <- table(vec, useNA = "ifany")
    prop_table <- prop.table(freq_table) * 100
    
    summary_df <- data.frame(Category = names(freq_table), Frequency = as.vector(freq_table),
                             Percentage = paste0(round(as.vector(prop_table), 2), "%"))
    datatable(summary_df, options = list(pageLength = 10, searching = FALSE, lengthChange = FALSE))
  })
  
  # Module 3 Plot Rendering
  output$dist_plot <- renderPlot({
    validate(need(input$file_upload, "Please upload a dataset in Tab 1 first."))
    df <- user_data()
    
    if (input$viz_tab == "Quantitative") {
      validate(need(input$viz_num_var != "", "Please select a numeric variable."))
      if (input$viz_num_type == "Histogram") {
        ggplot(df, aes(x = .data[[input$viz_num_var]])) +
          geom_histogram(binwidth = input$bin_width, fill = input$num_color, color = "black", alpha = 0.7) +
          labs(title = paste("Distribution of", input$viz_num_var), x = input$viz_num_var, y = "Frequency") +
          theme_minimal(base_size = 14)
      } else {
        ggplot(df, aes(x = .data[[input$viz_num_var]])) +
          geom_density(fill = input$num_color, alpha = 0.5, color = "black", linewidth = 1) +
          labs(title = paste("Density Curve of", input$viz_num_var), x = input$viz_num_var, y = "Density") +
          theme_minimal(base_size = 14)
      }
    } else {
      validate(need(input$viz_cat_var != "", "Please select a categorical variable."))
      if (input$viz_cat_type == "Bar Chart") {
        ggplot(df, aes(x = .data[[input$viz_cat_var]], fill = .data[[input$viz_cat_var]])) +
          geom_bar(color = "black") +
          scale_fill_brewer(palette = input$cat_palette) +
          labs(title = paste("Counts for", input$viz_cat_var), x = input$viz_cat_var, y = "Frequency") +
          theme_minimal(base_size = 14)
      } else {
        pie_df <- df %>% group_by(.data[[input$viz_cat_var]]) %>% summarise(Count = n())
        ggplot(pie_df, aes(x = "", y = Count, fill = .data[[input$viz_cat_var]])) +
          geom_bar(stat = "identity", width = 1, color = "white") +
          coord_polar("y", start = 0) +
          scale_fill_brewer(palette = input$cat_palette) +
          labs(title = paste("Pie Chart of", input$viz_cat_var)) +
          theme_void(base_size = 14)
      }
    }
  })
  # NEW: Module 3 Plot Rendering for Comparative Boxplot
  output$boxplot_plot <- renderPlot({
    validate(need(input$file_upload, "Please upload a dataset in Tab 1 first."))
    req(input$viz_box_vars)
    
    df <- user_data()
    selected_vars <- input$viz_box_vars
    
    # ggplot requires data in "long" format for multi-variable boxplots.
    # We use tidyr::pivot_longer to achieve this.
    plot_df <- df %>%
      select(all_of(selected_vars)) %>%
      pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
      filter(!is.na(Value)) # Remove NAs for cleaner plotting
    
    # Create the comparative boxplot
    ggplot(plot_df, aes(x = Variable, y = Value, fill = Variable)) +
      geom_boxplot(color = "black", alpha = 0.8) +
      stat_summary(fun = mean, geom = "point", shape = 23, size = 3, fill = "white", color = "black") + # Add mean points
      labs(title = "Comparative Distribution of Numeric Variables",
           x = "Numeric Variables",
           y = "Values",
           fill = "Variable") +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5),
            axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotate x-labels if many variables
      scale_fill_brewer(palette = "Set3") # Nice color palette
  })
  
  # Module 4 UI & Output
  output$mod4_ui_vars <- renderUI({
    df <- user_data()
    num_cols <- names(df)[sapply(df, is.numeric)]
    cat_cols <- names(df)[sapply(df, function(col) is.character(col) || is.factor(col) || is.logical(col))]
    
    if(input$test_type == "One-Sample") {
      selectInput("h_num_var", "Select Target Variable (Numeric):", choices = num_cols)
    } else {
      tagList(
        selectInput("h_num_var", "Select Target Variable (Numeric):", choices = num_cols),
        selectInput("h_cat_var", "Select Grouping Variable (2 Groups):", choices = cat_cols)
      )
    }
  })
  
  output$htest_output <- renderPrint({
    validate(need(input$file_upload, "Please upload a dataset in Tab 1 first."))
    req(input$h_num_var)
    df <- user_data()
    x <- df[[input$h_num_var]]
    
    if(input$test_type == "One-Sample") {
      x <- na.omit(x)
      mu <- input$null_mu
      if(input$zt_logic == "No (Use T-Test)") {
        res <- t.test(x, mu = mu)
        print(res)
      } else {
        req(input$sigma1)
        n <- length(x)
        xbar <- mean(x)
        z_stat <- (xbar - mu) / (input$sigma1 / sqrt(n))
        p_val <- 2 * pnorm(-abs(z_stat))
        cat("\n\tOne-Sample Z-Test\n\nZ-Statistic =", round(z_stat, 4), "\np-value =", format.pval(p_val, digits = 4), "\nSample Mean =", xbar, "\n")
      }
    } else {
      req(input$h_cat_var)
      group_col <- df[[input$h_cat_var]]
      valid_idx <- !is.na(x) & !is.na(group_col)
      x_valid <- x[valid_idx]
      g_valid <- as.factor(group_col[valid_idx])
      levels_g <- levels(g_valid)
      
      if(length(levels_g) != 2) {
        cat("ERROR: Two-Sample test requires a grouping variable with EXACTLY 2 categories.\n")
        return()
      }
      
      g1 <- x_valid[g_valid == levels_g[1]]
      g2 <- x_valid[g_valid == levels_g[2]]
      
      if(input$zt_logic == "No (Use T-Test)") {
        res <- t.test(g1, g2)
        print(res)
      } else {
        req(input$sigma1_2, input$sigma2_2)
        mean1 <- mean(g1); mean2 <- mean(g2)
        se <- sqrt((input$sigma1_2^2 / length(g1)) + (input$sigma2_2^2 / length(g2)))
        z_stat <- (mean1 - mean2) / se
        p_val <- 2 * pnorm(-abs(z_stat))
        cat("\n\tTwo-Sample Z-Test\n\nZ-Statistic =", round(z_stat, 4), "\np-value =", format.pval(p_val, digits = 4), "\nGroup 1 Mean =", mean1, "\nGroup 2 Mean =", mean2, "\n")
      }
    }
  })
  
  # Module 5 Logic: Correlation
  output$corr_text <- renderText({
    validate(need(input$file_upload, "Please upload a dataset in Tab 1 first."))
    req(input$corr_x, input$corr_y)
    df <- user_data()
    val_x <- df[[input$corr_x]]
    val_y <- df[[input$corr_y]]
    valid <- complete.cases(val_x, val_y)
    
    res <- cor.test(val_x[valid], val_y[valid], method = input$corr_method)
    paste0("Selected Method: ", toupper(input$corr_method), "\n",
           "Correlation Coefficient (r): ", round(res$estimate, 4), "\n",
           "p-value: ", format.pval(res$p.value, digits = 4))
  })
  
  output$corr_plot <- renderPlot({
    validate(need(input$file_upload, "Please upload a dataset in Tab 1 first."))
    req(input$corr_x, input$corr_y)
    df <- user_data()
    
    ggplot(df, aes(x = .data[[input$corr_x]], y = .data[[input$corr_y]])) +
      geom_point(color = "steelblue", alpha = 0.7, size = 3) +
      geom_smooth(method = "lm", color = "darkred", se = TRUE) +
      labs(title = paste("Scatterplot:", input$corr_x, "vs", input$corr_y),
           x = input$corr_x, y = input$corr_y) +
      theme_minimal(base_size = 14) +
      theme(plot.title = element_text(face = "bold", hjust = 0.5))
  })
  
  # Module 5 Logic: Linear Regression
  fit_model <- reactive({
    req(input$file_upload, input$reg_y, input$reg_x)
    df <- user_data()
    formula <- as.formula(paste(paste0("`", input$reg_y, "`"), "~", paste0("`", input$reg_x, "`")))
    lm(formula, data = df)
  })
  
  # 1. Formatted Linear Regression Equation
  output$reg_equation <- renderText({
    req(input$file_upload, input$reg_y, input$reg_x)
    validate(
      need(!input$reg_y %in% input$reg_x, "Error: Dependent variable (Y) cannot also be checked under Independent variables (X).")
    )
    
    df <- user_data()
    formula_str <- paste(input$reg_y, "~", paste(input$reg_x, collapse = " + "))
    fit <- lm(as.formula(formula_str), data = df)
    
    coefs <- round(coef(fit), 4)
    eq_terms <- paste(coefs[-1], names(coefs[-1]), sep = " * ")
    paste0(input$reg_y, " = ", coefs[1], " + ", paste(eq_terms, collapse = " + "))
  })
  
  # 2. Model Performance & Coefficients Output
  output$reg_summary <- renderPrint({
    req(input$file_upload, input$reg_y, input$reg_x)
    validate(
      need(!input$reg_y %in% input$reg_x, "Error: Dependent variable (Y) cannot also be checked under Independent variables (X).")
    )
    
    df <- user_data()
    formula_str <- paste(input$reg_y, "~", paste(input$reg_x, collapse = " + "))
    fit <- lm(as.formula(formula_str), data = df)
    
    summary(fit)
  })
  
  # 3.  Diagnostic Residual Plots
  output$reg_plot <- renderPlot({
    req(input$file_upload, input$reg_y, input$reg_x)
    validate(
      need(!input$reg_y %in% input$reg_x, "Error: Dependent variable (Y) cannot also be checked under Independent variables (X).")
    )
    
    df <- user_data()
    formula_str <- paste(input$reg_y, "~", paste(input$reg_x, collapse = " + "))
    fit <- lm(as.formula(formula_str), data = df)
    
    # Grid layout for residual diagnostics
    par(mfrow = c(2, 2))
    plot(fit)
  })  
  
} # <--- ADD THIS MISSING BRACE TO CLOSE THE SERVER FUNCTION!

# 4. LAUNCH APP
shinyApp(ui = ui, server = server)

# 1. Move back into the docs folder where your file is
setwd("C:/Users/DELL/Desktop/docs")

# 2. Rename the file to app.R automatically
file.rename("completedassignment.R.R", "app.R")

# 3. Export to a new output folder named site
shinylive::export(".", "site")