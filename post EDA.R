library(shiny)
library(shinydashboard)
library(DT)
library(readr)
library(readxl)
library(jsonlite)
library(ggplot2)
library(ggExtra)

# ============================================================
# UI
# ============================================================
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "DataExplorer Pro"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Home",          tabName = "home",   icon = icon("house")),
      menuItem("Upload Data",   tabName = "upload", icon = icon("upload")),
      menuItem("Data Cleaning", tabName = "clean",  icon = icon("broom")),
      menuItem("Feature Engineering", tabName = "feat", icon = icon("wrench")),   # ← 新增菜单项
      menuItem("EDA", tabName = "eda", icon = icon("chart-bar")) #eda
    )
  ),
  
  dashboardBody(
    
    tags$head(tags$style(HTML("
      .feature-card {
        background: #fff;
        border-radius: 10px;
        padding: 20px 24px;
        margin-bottom: 18px;
        box-shadow: 0 2px 8px rgba(0,0,0,.08);
        border-left: 5px solid #3c8dbc;
      }
      .feature-card h4 { margin-top: 0; color: #3c8dbc; }
      .feature-card p  { color: #555; margin-bottom: 0; }

      .hero-banner {
        background: linear-gradient(135deg, #1a6fa8 0%, #3c8dbc 100%);
        color: #fff;
        border-radius: 10px;
        padding: 36px 40px;
        margin-bottom: 28px;
      }
      .hero-banner h2 { margin-top: 0; font-size: 26px; }
      .hero-banner p  { font-size: 15px; opacity: .9; margin-bottom: 0; }

      .step-badge {
        display: inline-block;
        background: #3c8dbc;
        color: #fff;
        border-radius: 50%;
        width: 28px; height: 28px;
        line-height: 28px;
        text-align: center;
        font-weight: bold;
        margin-right: 8px;
      }

      .upload-zone {
        border: 2px dashed #3c8dbc;
        border-radius: 10px;
        padding: 24px;
        text-align: center;
        background: #f7fbff;
        margin-bottom: 12px;
      }

      .info-row td { padding: 5px 8px; border-bottom: 1px solid #f0f0f0; }
      .info-row td:first-child { color: #888; font-size: 13px; }
      .info-row td:last-child  { font-weight: bold; }
    "))),
    
    tabItems(
      
      # ==================== HOME ====================
      tabItem(tabName = "home",
              
              div(class = "hero-banner",
                  h2(icon("chart-bar"), " DataExplorer Pro"),
                  p("An interactive web application for data upload, cleaning, preprocessing,
            feature engineering, and exploratory data analysis.
            Upload your own dataset or try one of our built-in examples.")
              ),
              
              fluidRow(
                column(6,
                       div(class = "feature-card",
                           h4(icon("upload"), " Upload Data"),
                           p("Supports CSV, Excel (.xlsx / .xls), JSON, and RDS formats.
                Two built-in demo datasets (iris, mtcars) let you explore immediately.")
                       ),
                       div(class = "feature-card",
                           h4(icon("broom"), " Data Cleaning"),
                           p("Handle missing values (removal / imputation), detect and remove duplicates,
                fix column types — all with live row-count feedback.")
                       )
                ),
                column(6,
                       div(class = "feature-card",
                           h4(icon("wrench"), " Feature Engineering"),
                           p("Create derived columns via expressions, apply log / sqrt transforms,
                and bin numeric variables with instant preview.")
                       ),
                       div(class = "feature-card",
                           h4(icon("magnifying-glass-chart"), " Exploratory Data Analysis"),
                           p("Interactive charts (histogram, scatter, box, bar), correlation heatmap,
                summary statistics, and dynamic column-level filtering.")
                       )
                )
              ),
              
              box(title = "How to Use This App", width = 12, solidHeader = TRUE, status = "primary",
                  tags$ol(style = "line-height: 2;",
                          tags$li(tags$span(class="step-badge","1"),
                                  "Go to ", tags$b("Upload Data"),
                                  " — upload a file or select a built-in dataset, then click ",
                                  tags$b("Load / Refresh"), "."),
                          tags$li(tags$span(class="step-badge","2"),
                                  "Open ", tags$b("Data Cleaning"),
                                  " — fix missing values, remove duplicates, and correct column types."),
                          tags$li(tags$span(class="step-badge","3"),
                                  "Visit ", tags$b("Feature Engineering"),
                                  " — derive new variables from existing columns (coming soon)."),
                          tags$li(tags$span(class="step-badge","4"),
                                  "Explore ", tags$b("EDA"),
                                  " — generate interactive charts and statistics (coming soon).")
                  )
              )
      ),
      
      # ==================== UPLOAD ====================
      tabItem(tabName = "upload",
              
              fluidRow(
                column(4,
                       box(width = 12, title = "Load Dataset",
                           status = "primary", solidHeader = TRUE,
                           
                           radioButtons("data_source", "Data source:",
                                        choices = c("Upload my own file" = "upload",
                                                    "Built-in: iris"      = "iris",
                                                    "Built-in: mtcars"    = "mtcars"),
                                        selected = "iris"
                           ),
                           
                           hr(),
                           
                           conditionalPanel(
                             condition = "input.data_source == 'upload'",
                             div(class = "upload-zone",
                                 fileInput("user_file", label = NULL,
                                           accept      = c(".csv", ".xlsx", ".xls", ".json", ".rds"),
                                           buttonLabel = "Browse...",
                                           placeholder = "No file selected"
                                 ),
                                 p(style = "color:#999;font-size:12px;margin:0;",
                                   icon("circle-info"),
                                   " CSV, Excel, JSON, RDS supported")
                             ),
                             conditionalPanel(
                               condition = "input.user_file !== null &&
                    (input.user_file.name.endsWith('.csv') ||
                     input.user_file.name.endsWith('.txt'))",
                               checkboxInput("csv_header", "First row is header", TRUE),
                               selectInput("csv_sep", "Delimiter",
                                           choices = c("Comma (,)" = ",", "Semicolon (;)" = ";",
                                                       "Tab"       = "\t","Pipe (|)"      = "|"))
                             )
                           ),
                           
                           hr(),
                           
                           numericInput("preview_rows", "Rows to preview:",
                                        value = 100, min = 5, max = 5000, step = 5),
                           
                           actionButton("btn_load", "Load / Refresh",
                                        icon = icon("rotate"),
                                        class = "btn-primary",
                                        style = "width:100%;margin-top:6px;")
                       ),
                       
                       uiOutput("info_box")
                ),
                
                column(8,
                       box(width = 12, title = "Data Preview",
                           status = "info", solidHeader = TRUE,
                           tabsetPanel(
                             tabPanel("Table",          br(), DTOutput("preview_table")),
                             tabPanel("Column Types",   br(), DTOutput("col_types_table")),
                             tabPanel("Missing Values", br(), DTOutput("missing_table"))
                           )
                       )
                )
              )
      ),
      
      # ==================== DATA CLEANING ====================
      tabItem(tabName = "clean",
              
              uiOutput("clean_no_data_warning"),
              fluidRow(uiOutput("clean_stats_bar")),
              
              fluidRow(
                column(4,
                       
                       box(width = 12, title = "1. Duplicate Rows",
                           status = "warning", solidHeader = TRUE, collapsible = TRUE,
                           p(style="color:#888;font-size:13px;",
                             icon("circle-info"), " Rows that are identical across all columns."),
                           verbatimTextOutput("dup_count"),
                           actionButton("btn_remove_dup", "Remove Duplicates",
                                        icon = icon("trash"), class = "btn-warning",
                                        style = "width:100%;")
                       ),
                       
                       box(width = 12, title = "2. Missing Values",
                           status = "danger", solidHeader = TRUE, collapsible = TRUE,
                           p(style="color:#888;font-size:13px;",
                             icon("circle-info"), " Choose columns and a strategy for handling NAs."),
                           uiOutput("missing_col_select"),
                           selectInput("missing_strategy", "Strategy:",
                                       choices = c(
                                         "Remove rows with NA"           = "remove_rows",
                                         "Impute with Mean (numeric)"    = "mean",
                                         "Impute with Median (numeric)"  = "median",
                                         "Impute with Mode (any type)"   = "mode",
                                         "Impute with a fixed value"     = "fixed",
                                         "Remove selected columns"       = "remove_cols"
                                       )
                           ),
                           conditionalPanel(
                             condition = "input.missing_strategy == 'fixed'",
                             textInput("missing_fixed_val", "Fixed value:", placeholder = "e.g. 0 or Unknown")
                           ),
                           actionButton("btn_handle_missing", "Apply Missing Value Treatment",
                                        icon = icon("wand-magic-sparkles"), class = "btn-danger",
                                        style = "width:100%;")
                       ),
                       
                       box(width = 12, title = "3. Column Type Conversion",
                           status = "info", solidHeader = TRUE, collapsible = TRUE,
                           p(style="color:#888;font-size:13px;",
                             icon("circle-info"), " Change a column's data type."),
                           uiOutput("type_col_select"),
                           selectInput("target_type", "Convert to:",
                                       choices = c("Numeric"           = "numeric",
                                                   "Character"         = "character",
                                                   "Factor"            = "factor",
                                                   "Logical"           = "logical",
                                                   "Date (YYYY-MM-DD)" = "date")),
                           actionButton("btn_convert_type", "Convert Type",
                                        icon = icon("arrows-rotate"), class = "btn-info",
                                        style = "width:100%;")
                       ),
                       
                       box(width = 12, title = "4. Outlier Handling (IQR)",
                           status = "primary", solidHeader = TRUE, collapsible = TRUE,
                           p(style="color:#888;font-size:13px;",
                             icon("circle-info"), " Detects outliers via 1.5 × IQR rule on numeric columns."),
                           uiOutput("outlier_col_select"),
                           selectInput("outlier_strategy", "Strategy:",
                                       choices = c("Remove rows with outliers"     = "remove",
                                                   "Cap to IQR bounds (Winsorise)" = "cap")),
                           verbatimTextOutput("outlier_count"),
                           actionButton("btn_handle_outlier", "Apply Outlier Treatment",
                                        icon = icon("filter"), class = "btn-primary",
                                        style = "width:100%;")
                       ),
                       
                       box(width = 12, title = "5. Scaling & Encoding",
                           status = "success", solidHeader = TRUE, collapsible = TRUE,
                           p(style="color:#888;font-size:13px;",
                             icon("circle-info"), " Scale numeric columns or encode categorical columns."),
                           uiOutput("scale_col_select"),
                           selectInput("scale_method", "Method:",
                                       choices = c(
                                         "Min-Max Normalisation [0,1]"  = "minmax",
                                         "Z-score Standardisation"      = "zscore",
                                         "One-Hot Encode (factor/char)" = "onehot"
                                       )),
                           actionButton("btn_scale", "Apply",
                                        icon = icon("sliders"), class = "btn-success",
                                        style = "width:100%;")
                       ),
                       
                       box(width = 12, status = "warning", solidHeader = FALSE,
                           actionButton("btn_reset_clean", "Reset to Original Data",
                                        icon = icon("rotate-left"), class = "btn-warning",
                                        style = "width:100%;"),
                           br(), br(),
                           downloadButton("btn_download", "Download Cleaned Data (.csv)",
                                          style = "width:100%;")
                       )
                ),
                
                column(8,
                       box(width = 12, title = "Cleaned Data Preview",
                           status = "success", solidHeader = TRUE,
                           tabsetPanel(
                             tabPanel("Table",                  br(), DTOutput("clean_preview_table")),
                             tabPanel("Missing Values Summary", br(), DTOutput("clean_missing_summary")),
                             tabPanel("Column Types",           br(), DTOutput("clean_col_types")),
                             tabPanel("Operation Log",          br(), verbatimTextOutput("clean_log"))
                           )
                       )
                )
              )
      ),
      
      # ==================== FEATURE ENGINEERING (新增) ====================
      tabItem(tabName = "feat",
              
              uiOutput("feat_no_data_warning"),
              
              fluidRow(
                column(4,
                       box(width = 12, title = "Create New Feature",
                           status = "primary", solidHeader = TRUE,
                           
                           selectInput("feat_type", "Feature type:",
                                       choices = c(
                                         "Arithmetic (+, -, *, /)" = "arithmetic",
                                         "Mathematical transform"   = "math",
                                         "Binning"                  = "bin"
                                       )),
                           
                           # Arithmetic panel
                           conditionalPanel(
                             condition = "input.feat_type == 'arithmetic'",
                             selectInput("feat_arith_col1", "First column:", choices = NULL),
                             selectInput("feat_arith_op", "Operator:",
                                         choices = c("+" = "+", "-" = "-", "*" = "*", "/" = "/")),
                             selectInput("feat_arith_col2", "Second column:", choices = NULL)
                           ),
                           
                           # Math transform panel
                           conditionalPanel(
                             condition = "input.feat_type == 'math'",
                             selectInput("feat_math_col", "Column:", choices = NULL),
                             selectInput("feat_math_func", "Function:",
                                         choices = c(
                                           "Log (natural)" = "log",
                                           "Log10"         = "log10",
                                           "Square root"   = "sqrt",
                                           "Square"        = "^2",
                                           "Exponential"   = "exp"
                                         ))
                           ),
                           
                           # Binning panel
                           conditionalPanel(
                             condition = "input.feat_type == 'bin'",
                             selectInput("feat_bin_col", "Column:", choices = NULL),
                             numericInput("feat_bin_n", "Number of bins:", value = 5, min = 2, max = 100),
                             selectInput("feat_bin_method", "Method:",
                                         choices = c(
                                           "Equal width" = "equal_width",
                                           "Equal frequency (quantile)" = "equal_freq"
                                         ))
                           ),
                           
                           textInput("feat_new_name", "New column name:", placeholder = "e.g., new_col"),
                           
                           actionButton("btn_create_feat", "Create Feature",
                                        icon = icon("plus"), class = "btn-success",
                                        style = "width:100%;")
                       ),
                       
                       box(width = 12, title = "Existing Features",
                           status = "info", solidHeader = TRUE, collapsible = TRUE,
                           DTOutput("feat_list_table"),
                           br(),
                           actionButton("btn_remove_feat", "Remove Selected Feature",
                                        icon = icon("trash"), class = "btn-warning",
                                        style = "width:100%;")
                       )
                ),
                
                column(8,
                       box(width = 12, title = "Data Preview with New Features",
                           status = "success", solidHeader = TRUE,
                           tabsetPanel(
                             tabPanel("Table", br(), DTOutput("feat_preview_table")),
                             tabPanel("New Feature Summary", br(), verbatimTextOutput("feat_summary")),
                             tabPanel("Operation Log", br(), verbatimTextOutput("feat_log"))
                           )
                       )
                )
              )
      ),

      # EDA (jana)
      tabItem(tabName = "eda",
              
              uiOutput("eda_no_data_warning"), 
              
              fluidRow(
                column(4, # for the "left"
                       box(width = 12, title = "EDA Controls",
                           status = "primary", solidHeader = TRUE,
                           selectInput("eda_plot_type", "Plot type:",
                                       choices = c( "Histogram"= "hist", "Boxplot"= "box",
                                                    "Scatterplot"= "scatter","Bar Plot"= "bar")),
                           uiOutput("eda_x_ui"),
                           uiOutput("eda_y_ui"),
                           uiOutput("eda_color_ui"),
                           
                           conditionalPanel(
                             condition = "input.eda_plot_type == 'hist'",
                             sliderInput("eda_bins", "Number of bins:", min = 5, max = 50, value = 15)),
                           
                           checkboxInput("eda_add_lm", "Add Linear Regression Line", value = FALSE),
                           
                           hr(),
                           h4("Filter Data"),
                           uiOutput("eda_filter_var_ui"),
                           uiOutput("eda_filter_ui")
                           ),
                       #adding summary stats
                       box(width = 12, title = "Summary Statistics",
                           status = "info", solidHeader = TRUE,
                           verbatimTextOutput("eda_summary"))
                       ),
                
                column(8, #for the right vwhere visualization come in 
                       box(width = 12, title = "Visualization",
                           status = "success", solidHeader = TRUE,
                           plotOutput("eda_plot", height = 450)
                           ),
                       
                       box(width = 12, title = "Correlation Matrix",
                           status = "warning", solidHeader = TRUE,
                           tableOutput("eda_corr"))
                       )
                ),
              # whole preveiw
              fluidRow(
                box(width = 12, title = "Filtered Data Preview",
                    status = "primary", solidHeader = TRUE,
                    DTOutput("eda_table")))
      )
    )
  )
)

# ============================================================
# SERVER
# ============================================================
server <- function(input, output, session) {
  
  # ----------------------------------------------------------
  # 1. Shared reactive state — declared FIRST so all outputs
  #    below can reference clean_df safely
  # ----------------------------------------------------------
  clean_df <- reactiveVal(NULL)
  op_log   <- reactiveVal(character(0))
  created_features <- reactiveVal(character(0))   
  
  log_op <- function(msg) {
    ts <- format(Sys.time(), "[%H:%M:%S]")
    op_log(c(op_log(), paste(ts, msg)))
  }
  
  # ----------------------------------------------------------
  # 2. Load data when button is clicked
  #    ignoreNULL = FALSE  →  also fires once on app start
  #    so the default "iris" dataset appears immediately
  # ----------------------------------------------------------
  loaded_data <- eventReactive(input$btn_load, ignoreNULL = FALSE, {
    if (input$data_source == "iris")   return(iris)
    if (input$data_source == "mtcars") return(mtcars)
    
    req(input$user_file)
    f   <- input$user_file
    ext <- tolower(tools::file_ext(f$name))
    
    df <- tryCatch({
      switch(ext,
             "csv"  = ,
             "txt"  = read_delim(f$datapath,
                                 delim          = input$csv_sep,
                                 col_names      = input$csv_header,
                                 show_col_types = FALSE),
             "xlsx" = ,
             "xls"  = read_excel(f$datapath),
             "json" = as.data.frame(fromJSON(f$datapath)),
             "rds"  = readRDS(f$datapath),
             stop("Unsupported format: .", ext)
      )
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 8)
      NULL
    })
    
    df
  })
  
  # ----------------------------------------------------------
  # 3. Whenever loaded_data changes → push into clean_df
  #    (this is the bridge between Upload and Cleaning tabs)
  # ----------------------------------------------------------
  observeEvent(loaded_data(), {
    df <- loaded_data()
    req(df)
    clean_df(as.data.frame(df))
    op_log(character(0))
    log_op("Dataset loaded into cleaning workspace.")
    
    # 如果是内置数据集，自动生成一些预定义特征
    if (input$data_source %in% c("iris", "mtcars")) {
      df <- clean_df()  # 获取当前数据框
      new_features <- character(0)  # 记录新列名
      
      if (input$data_source == "iris") {
        # 为 iris 生成 10 个特征
        df$Sepal_Plus_Sepal.Width <- df$Sepal.Length + df$Sepal.Width
        new_features <- c(new_features, "Sepal_Plus_Sepal.Width")
        
        df$Sepal_Minus_Petal.Length <- df$Sepal.Length - df$Petal.Length
        new_features <- c(new_features, "Sepal_Minus_Petal.Length")
        
        df$Sepal_Times_Petal.Width <- df$Sepal.Length * df$Petal.Width
        new_features <- c(new_features, "Sepal_Times_Petal.Width")
        
        df$Petal_Ratio <- df$Petal.Length / df$Petal.Width
        new_features <- c(new_features, "Petal_Ratio")
        
        df$Log_Sepal.Length <- log(df$Sepal.Length)
        new_features <- c(new_features, "Log_Sepal.Length")
        
        df$Sqrt_Petal.Width <- sqrt(df$Petal.Width)
        new_features <- c(new_features, "Sqrt_Petal.Width")
        
        df$Sepal.Length_Squared <- df$Sepal.Length^2
        new_features <- c(new_features, "Sepal.Length_Squared")
        
        df$Exp_Petal.Length <- exp(df$Petal.Length)
        new_features <- c(new_features, "Exp_Petal.Length")
        
        df$Sepal.Width_Log <- log(df$Sepal.Width)
        new_features <- c(new_features, "Sepal.Width_Log")
        
        df$Petal.Length_Plus_Petal.Width <- df$Petal.Length + df$Petal.Width
        new_features <- c(new_features, "Petal.Length_Plus_Petal.Width")
      } else if (input$data_source == "mtcars") {
        # 为 mtcars 生成 10 个特征
        df$mpg_plus_disp <- df$mpg + df$disp
        new_features <- c(new_features, "mpg_plus_disp")
        
        df$hp_minus_wt <- df$hp - df$wt
        new_features <- c(new_features, "hp_minus_wt")
        
        df$drat_times_gear <- df$drat * df$gear
        new_features <- c(new_features, "drat_times_gear")
        
        df$wt_div_hp <- df$wt / df$hp
        new_features <- c(new_features, "wt_div_hp")
        
        df$log_mpg <- log(df$mpg)
        new_features <- c(new_features, "log_mpg")
        
        df$sqrt_disp <- sqrt(df$disp)
        new_features <- c(new_features, "sqrt_disp")
        
        df$hp_squared <- df$hp^2
        new_features <- c(new_features, "hp_squared")
        
        df$exp_wt <- exp(df$wt)
        new_features <- c(new_features, "exp_wt")
        
        df$qsec_log <- log(df$qsec)
        new_features <- c(new_features, "qsec_log")
        
        df$carb_plus_gear <- df$carb + df$gear
        new_features <- c(new_features, "carb_plus_gear")
      }
      
      clean_df(df)
      # 将新特征添加到 created_features
      created_features(new_features)
      log_op(paste("Auto-generated", length(new_features), "predefined features for", input$data_source))
    }
    
    showNotification(
      sprintf("Loaded: %d rows × %d columns", nrow(df), ncol(df)),
      type = "message", duration = 3
    )
  })
  
  # ----------------------------------------------------------
  # UPLOAD TAB outputs  (all read from clean_df)
  # ----------------------------------------------------------
  
  output$info_box <- renderUI({
    df <- clean_df(); req(df)
    box(width = 12, title = "Dataset Info", status = "success", solidHeader = TRUE,
        tags$table(class = "info-row", style = "width:100%;",
                   tags$tr(tags$td(icon("table"),                " Rows"),
                           tags$td(format(nrow(df), big.mark = ","))),
                   tags$tr(tags$td(icon("columns"),              " Columns"),
                           tags$td(ncol(df))),
                   tags$tr(tags$td(icon("triangle-exclamation"), " Missing cells"),
                           tags$td(format(sum(is.na(df)), big.mark = ","))),
                   tags$tr(tags$td(icon("copy"),                 " Duplicate rows"),
                           tags$td(format(sum(duplicated(df)),   big.mark = ",")))
        )
    )
  })
  
  output$preview_table <- renderDT({
    df <- clean_df(); req(df)
    head(df, input$preview_rows)
  }, options = list(scrollX = TRUE, pageLength = 15,
                    lengthMenu = c(10, 15, 25, 50, 100)),
  rownames = FALSE, filter = "top")
  
  output$col_types_table <- renderDT({
    df <- clean_df(); req(df)
    data.frame(
      Column    = names(df),
      Type      = sapply(df, function(x) class(x)[1]),
      N_Unique  = sapply(df, function(x) length(unique(x))),
      N_Missing = sapply(df, function(x) sum(is.na(x))),
      stringsAsFactors = FALSE
    )
  }, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  
  output$missing_table <- renderDT({
    df <- clean_df(); req(df)
    miss <- data.frame(
      Column        = names(df),
      Missing_Count = sapply(df, function(x) sum(is.na(x))),
      Missing_Pct   = paste0(round(sapply(df, function(x) mean(is.na(x))) * 100, 1), "%"),
      stringsAsFactors = FALSE
    )
    miss[order(-miss$Missing_Count), ]
  }, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  
  # ----------------------------------------------------------
  # CLEANING TAB outputs
  # ----------------------------------------------------------
  
  output$clean_no_data_warning <- renderUI({
    if (is.null(clean_df())) {
      div(style = "background:#fff3cd;border:1px solid #ffc107;border-radius:8px;
                   padding:14px 20px;margin-bottom:16px;",
          icon("triangle-exclamation", style="color:#856404;"),
          tags$b(style="color:#856404;", " No dataset loaded."),
          span(style="color:#856404;",
               " Please go to ", tags$b("Upload Data"), " and click Load / Refresh first.")
      )
    }
  })
  
  output$clean_stats_bar <- renderUI({
    orig <- loaded_data()
    curr <- clean_df()
    if (is.null(orig) || is.null(curr)) return(NULL)
    
    make_stat <- function(label, val, color) {
      column(3,
             div(style = sprintf(
               "background:%s;color:#fff;border-radius:8px;padding:14px;
           text-align:center;margin-bottom:10px;", color),
               div(style="font-size:22px;font-weight:bold;", val),
               div(style="font-size:12px;opacity:.9;", label)
             )
      )
    }
    
    fluidRow(
      make_stat("Original rows",  format(nrow(orig), big.mark=","),            "#3c8dbc"),
      make_stat("Current rows",   format(nrow(curr), big.mark=","),            "#00a65a"),
      make_stat("Missing cells",  format(sum(is.na(curr)), big.mark=","),      "#dd4b39"),
      make_stat("Duplicate rows", format(sum(duplicated(curr)), big.mark=","), "#f39c12")
    )
  })
  
  # Dynamic selectors
  output$missing_col_select <- renderUI({
    df <- clean_df(); req(df)
    cols_na <- names(df)[sapply(df, function(x) any(is.na(x)))]
    if (length(cols_na) == 0) cols_na <- names(df)
    checkboxGroupInput("missing_cols", "Select columns:",
                       choices = cols_na, selected = cols_na)
  })
  
  output$type_col_select <- renderUI({
    df <- clean_df(); req(df)
    selectInput("type_col", "Select column:", choices = names(df))
  })
  
  output$outlier_col_select <- renderUI({
    df <- clean_df(); req(df)
    num_cols <- names(df)[sapply(df, is.numeric)]
    selectInput("outlier_col", "Select numeric column:", choices = num_cols)
  })
  
  output$scale_col_select <- renderUI({
    df <- clean_df(); req(df)
    selectInput("scale_col", "Select column:", choices = names(df))
  })
  
  output$dup_count <- renderText({
    df <- clean_df()
    if (is.null(df)) return("No data loaded.")
    paste("Duplicate rows found:", sum(duplicated(df)))
  })
  
  output$outlier_count <- renderText({
    df  <- clean_df(); req(df)
    col <- input$outlier_col; req(col)
    x   <- df[[col]]
    if (!is.numeric(x)) return("Selected column is not numeric.")
    q   <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
    iqr <- q[2] - q[1]
    n   <- sum(x < (q[1] - 1.5*iqr) | x > (q[2] + 1.5*iqr), na.rm = TRUE)
    paste("Outliers detected in", col, ":", n, "rows")
  })
  
  # Actions
  observeEvent(input$btn_remove_dup, {
    df <- clean_df(); req(df)
    before <- nrow(df)
    df <- df[!duplicated(df), ]
    clean_df(df)
    removed <- before - nrow(df)
    log_op(sprintf("Remove duplicates: %d row(s) removed. (%d → %d)", removed, before, nrow(df)))
    showNotification(paste("Removed", removed, "duplicate row(s)."),
                     type = if (removed > 0) "message" else "warning")
  })
  
  observeEvent(input$btn_handle_missing, {
    df    <- clean_df(); req(df)
    cols  <- input$missing_cols; req(cols)
    strat <- input$missing_strategy
    before_na <- sum(is.na(df))
    
    mode_val <- function(x) {
      ux <- unique(x[!is.na(x)])
      if (length(ux) == 0) return(NA)
      ux[which.max(tabulate(match(x, ux)))]
    }
    
    tryCatch({
      if (strat == "remove_rows") {
        df <- df[complete.cases(df[, cols, drop = FALSE]), ]
      } else if (strat == "remove_cols") {
        df <- df[, !(names(df) %in% cols), drop = FALSE]
      } else if (strat == "mean") {
        for (col in cols) if (is.numeric(df[[col]]))
          df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
      } else if (strat == "median") {
        for (col in cols) if (is.numeric(df[[col]]))
          df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
      } else if (strat == "mode") {
        for (col in cols)
          df[[col]][is.na(df[[col]])] <- mode_val(df[[col]])
      } else if (strat == "fixed") {
        val <- input$missing_fixed_val
        for (col in cols) df[[col]][is.na(df[[col]])] <- val
      }
      after_na <- sum(is.na(df))
      clean_df(df)
      log_op(sprintf("Missing [%s] on [%s]: NAs %d → %d",
                     strat, paste(cols, collapse=", "), before_na, after_na))
      showNotification("Missing value treatment applied.", type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 8)
    })
  })
  
  observeEvent(input$btn_convert_type, {
    df  <- clean_df(); req(df)
    col <- input$type_col; req(col)
    tgt <- input$target_type
    old_type <- class(df[[col]])[1]
    tryCatch({
      df[[col]] <- switch(tgt,
                          "numeric"   = suppressWarnings(as.numeric(df[[col]])),
                          "character" = as.character(df[[col]]),
                          "factor"    = as.factor(df[[col]]),
                          "logical"   = as.logical(df[[col]]),
                          "date"      = as.Date(df[[col]])
      )
      clean_df(df)
      log_op(sprintf("Type conversion: '%s' %s → %s", col, old_type, tgt))
      showNotification(sprintf("'%s' converted to %s.", col, tgt), type = "message")
    }, error = function(e) {
      showNotification(paste("Conversion error:", e$message), type = "error", duration = 8)
    })
  })
  
  observeEvent(input$btn_handle_outlier, {
    df  <- clean_df(); req(df)
    col <- input$outlier_col; req(col)
    x   <- df[[col]]; req(is.numeric(x))
    q   <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
    iqr <- q[2] - q[1]
    lo  <- q[1] - 1.5 * iqr
    hi  <- q[2] + 1.5 * iqr
    before <- nrow(df)
    if (input$outlier_strategy == "remove") {
      df <- df[!is.na(x) & x >= lo & x <= hi, ]
      log_op(sprintf("Outlier removal on '%s': %d row(s) removed.", col, before - nrow(df)))
    } else {
      df[[col]] <- pmin(pmax(df[[col]], lo), hi)
      log_op(sprintf("Outlier capping on '%s': winsorised to [%.3f, %.3f].", col, lo, hi))
    }
    clean_df(df)
    showNotification("Outlier treatment applied.", type = "message")
  })
  
  observeEvent(input$btn_scale, {
    df     <- clean_df(); req(df)
    col    <- input$scale_col; req(col)
    method <- input$scale_method
    tryCatch({
      if (method == "minmax") {
        x <- df[[col]]; req(is.numeric(x))
        rng <- range(x, na.rm = TRUE)
        df[[col]] <- (x - rng[1]) / (rng[2] - rng[1])
        log_op(sprintf("Min-Max normalisation applied to '%s'.", col))
      } else if (method == "zscore") {
        x <- df[[col]]; req(is.numeric(x))
        df[[col]] <- as.numeric(scale(x))
        log_op(sprintf("Z-score standardisation applied to '%s'.", col))
      } else if (method == "onehot") {
        x    <- as.factor(df[[col]])
        lvls <- levels(x)
        if (length(lvls) > 20) {
          showNotification("Too many levels (>20) for one-hot encoding.", type = "warning")
          return()
        }
        for (lv in lvls) df[[paste0(col, "_", lv)]] <- as.integer(x == lv)
        df[[col]] <- NULL
        log_op(sprintf("One-hot encoding on '%s': %d new columns.", col, length(lvls)))
      }
      clean_df(df)
      showNotification("Scaling / encoding applied.", type = "message")
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 8)
    })
  })
  
  observeEvent(input$btn_reset_clean, {
    df <- loaded_data(); req(df)
    clean_df(as.data.frame(df))
    created_features(character(0))   
    op_log(character(0))
    log_op("Reset to original loaded data.")
    showNotification("Data reset to original.", type = "warning")
  })
  
  output$clean_preview_table <- renderDT({
    df <- clean_df(); req(df)
    df
  }, options = list(scrollX = TRUE, pageLength = 15,
                    lengthMenu = c(10, 15, 25, 50)),
  rownames = FALSE, filter = "top")
  
  output$clean_missing_summary <- renderDT({
    df <- clean_df(); req(df)
    miss <- data.frame(
      Column        = names(df),
      Missing_Count = sapply(df, function(x) sum(is.na(x))),
      Missing_Pct   = paste0(round(sapply(df, function(x) mean(is.na(x))) * 100, 1), "%"),
      stringsAsFactors = FALSE
    )
    miss[order(-miss$Missing_Count), ]
  }, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  
  output$clean_col_types <- renderDT({
    df <- clean_df(); req(df)
    data.frame(
      Column   = names(df),
      Type     = sapply(df, function(x) class(x)[1]),
      N_Unique = sapply(df, function(x) length(unique(x))),
      stringsAsFactors = FALSE
    )
  }, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  
  output$clean_log <- renderText({
    log <- op_log()
    if (length(log) == 0) return("No operations performed yet.")
    paste(rev(log), collapse = "\n")
  })
  
  output$btn_download <- downloadHandler(
    filename = function() paste0("cleaned_data_", Sys.Date(), ".csv"),
    content  = function(file) write.csv(clean_df(), file, row.names = FALSE)
  )
  
  # ----------------------------------------------------------
  # FEATURE ENGINEERING (新增 server 代码)
  # ----------------------------------------------------------
  
  observe({
    df <- clean_df()
    req(df)
    cols <- names(df)
    updateSelectInput(session, "feat_arith_col1", choices = cols)
    updateSelectInput(session, "feat_arith_col2", choices = cols)
    updateSelectInput(session, "feat_math_col",   choices = cols)
    updateSelectInput(session, "feat_bin_col",    choices = cols)
  })
  
  # 创建新特征
  observeEvent(input$btn_create_feat, {
    df <- clean_df()
    req(df)
    
    new_name <- input$feat_new_name
    if (new_name == "") {
      showNotification("Please enter a new column name.", type = "warning")
      return()
    }
    if (new_name %in% names(df)) {
      showNotification("Column name already exists. Choose a different name.", type = "warning")
      return()
    }
    
    tryCatch({
      if (input$feat_type == "arithmetic") {
        col1 <- input$feat_arith_col1
        col2 <- input$feat_arith_col2
        op   <- input$feat_arith_op
        if (!col1 %in% names(df) || !col2 %in% names(df)) {
          showNotification("Selected columns not found.", type = "error")
          return()
        }
        if (!is.numeric(df[[col1]]) || !is.numeric(df[[col2]])) {
          showNotification("Arithmetic operations require numeric columns.", type = "warning")
          return()
        }
        new_col <- switch(op,
                          "+" = df[[col1]] + df[[col2]],
                          "-" = df[[col1]] - df[[col2]],
                          "*" = df[[col1]] * df[[col2]],
                          "/" = df[[col1]] / df[[col2]])
        log_detail <- paste(col1, op, col2)
        
      } else if (input$feat_type == "math") {
        col  <- input$feat_math_col
        func <- input$feat_math_func
        if (!col %in% names(df)) {
          showNotification("Selected column not found.", type = "error")
          return()
        }
        if (!is.numeric(df[[col]])) {
          showNotification("Mathematical transforms require numeric columns.", type = "warning")
          return()
        }
        new_col <- switch(func,
                          "log"   = log(df[[col]]),
                          "log10" = log10(df[[col]]),
                          "sqrt"  = sqrt(df[[col]]),
                          "^2"    = df[[col]]^2,
                          "exp"   = exp(df[[col]]))
        log_detail <- paste(func, "of", col)
        
      } else if (input$feat_type == "bin") {
        col    <- input$feat_bin_col
        n      <- input$feat_bin_n
        method <- input$feat_bin_method
        if (!col %in% names(df)) {
          showNotification("Selected column not found.", type = "error")
          return()
        }
        if (!is.numeric(df[[col]])) {
          showNotification("Binning requires numeric columns.", type = "warning")
          return()
        }
        x <- df[[col]]
        if (method == "equal_width") {
          breaks <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n + 1)
          new_col <- cut(x, breaks = breaks, include.lowest = TRUE, labels = FALSE)
        } else { # equal frequency
          probs <- seq(0, 1, length.out = n + 1)
          breaks <- quantile(x, probs = probs, na.rm = TRUE)
          if (any(duplicated(breaks))) {
            showNotification("Equal frequency bins resulted in duplicate breaks. Try fewer bins.", type = "warning")
            return()
          }
          new_col <- cut(x, breaks = breaks, include.lowest = TRUE, labels = FALSE)
        }
        log_detail <- paste("binning", col, "into", n, "bins")
      }
      
      # 添加新列
      df[[new_name]] <- new_col
      clean_df(df)
      created_features(c(created_features(), new_name))
      log_op(sprintf("Feature created: '%s' from %s", new_name, log_detail))
      showNotification(sprintf("Feature '%s' created.", new_name), type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error creating feature:", e$message), type = "error", duration = 8)
    })
  })
  
  # 显示已创建的特征列表
  output$feat_list_table <- renderDT({
    feat_names <- created_features()
    if (length(feat_names) == 0) {
      data.frame(Message = "No features created yet.")
    } else {
      data.frame(Feature = feat_names, stringsAsFactors = FALSE)
    }
  }, options = list(pageLength = 10, dom = 't'), rownames = FALSE, selection = 'single')
  
  # 删除选中的特征
  observeEvent(input$btn_remove_feat, {
    df <- clean_df()
    req(df)
    selected <- input$feat_list_table_rows_selected
    if (length(selected) == 0) {
      showNotification("Please select a feature to remove.", type = "warning")
      return()
    }
    feat_name <- created_features()[selected]
    
    if (feat_name %in% names(df)) {
      df[[feat_name]] <- NULL
      clean_df(df)
      created_features(setdiff(created_features(), feat_name))
      log_op(sprintf("Feature removed: '%s'", feat_name))
      showNotification(sprintf("Feature '%s' removed.", feat_name), type = "message")
    } else {
      created_features(setdiff(created_features(), feat_name))
      showNotification(sprintf("Feature '%s' was not found in data, removed from list.", feat_name), type = "warning")
    }
  })
  
  # 数据预览表格 
  output$feat_preview_table <- renderDT({
    df <- clean_df()
    req(df)
    df
  }, options = list(scrollX = TRUE, pageLength = 15), rownames = FALSE, filter = "top")
  
  # 新特征摘要
  output$feat_summary <- renderPrint({
    df <- clean_df()
    req(df)
    feat_names <- created_features()
    if (length(feat_names) == 0) {
      cat("No features created yet.")
    } else {
      for (fn in feat_names) {
        if (fn %in% names(df)) {
          if (is.numeric(df[[fn]])) {
            cat(paste0("Summary of '", fn, "':\n"))
            print(summary(df[[fn]]))
            cat("\n")
          } else {
            cat(paste0("'", fn, "' is not numeric. First 10 values:\n"))
            print(head(df[[fn]], 10))
            cat("\n")
          }
        }
      }
    }
  })
  
  output$feat_log <- renderText({
    log <- op_log()
    if (length(log) == 0) return("No operations performed yet.")
    paste(rev(log), collapse = "\n")
  })
  
  
  output$feat_no_data_warning <- renderUI({
    if (is.null(clean_df())) {
      div(style = "background:#fff3cd;border:1px solid #ffc107;border-radius:8px;padding:14px 20px;margin-bottom:16px;",
          icon("triangle-exclamation", style="color:#856404;"),
          tags$b(style="color:#856404;", " No dataset loaded."),
          span(style="color:#856404;", " Please go to ", tags$b("Upload Data"), " and click Load / Refresh first.")
      )
    }
  })
  
  #EDA 
  
  output$eda_no_data_warning <- renderUI({
    if (is.null(clean_df())) {
      div(style = "background:#fff3cd;border:1px solid #ffc107;border-radius:8px;padding:14px 20px;margin-bottom:16px;",
          "No data")
    }
  })
  # creating dropdowans
  # https://shiny.posit.co/r/reference/shiny/latest/renderui.html
  # https://stackoverflow.com/questions/53027209/how-to-pass-data-frame-object-from-renderui-to-eventreactive
  output$eda_x_ui <- renderUI({req(clean_df())
    df <- clean_df()
    
    # only these three needs nuemeric
    if (input$eda_plot_type %in% c("hist", "box", "scatter")) {
      num_cols <- names(df)[sapply(df, is.numeric)]
      selectInput("eda_x", "X variable:", choices = num_cols)
    } else {
      all_cols <- names(df)
      selectInput("eda_x", "Variable:", choices = all_cols)
    }
  })
  
  output$eda_y_ui <- renderUI({
    req(clean_df())
    df <- clean_df()
    
    if (input$eda_plot_type == "scatter") {
      num_cols <- names(df)[sapply(df, is.numeric)]
      selectInput("eda_y", "Y variable:", choices = num_cols)
    }
  })
  
  output$eda_color_ui <- renderUI({
    req(clean_df())
    df <- clean_df()
    group_cols <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x) || is.logical(x))]
    
    selectInput("eda_color", "Color / group by:",
                choices = c("None", group_cols),
                selected = "None")
  })
  
  output$eda_filter_var_ui <- renderUI({
    req(clean_df())
    df <- clean_df()
    
    selectInput("eda_filter_var", "Filter variable:",
                choices = c("None", names(df)))
  })
  
  output$eda_filter_ui <- renderUI({
    req(clean_df(), input$eda_filter_var)
    df <- clean_df()
    
    if (input$eda_filter_var == "None") return(NULL)
    
    x <- df[[input$eda_filter_var]]
    
    if (is.numeric(x)) {
      sliderInput("eda_filter_range", "Select range:",
                  min = min(x, na.rm = TRUE),
                  max = max(x, na.rm = TRUE),
                  value = c(min(x, na.rm = TRUE), max(x, na.rm = TRUE)))
    } else {
      vals <- unique(as.character(x))
      vals <- vals[!is.na(vals)]
      
      selectInput("eda_filter_levels", "Select values:",
                  choices = vals,
                  selected = vals,
                  multiple = TRUE)
    }
  })
  
  eda_df <- reactive({
    req(clean_df())
    df <- clean_df()
    
    if (!is.null(input$eda_filter_var) && input$eda_filter_var != "None") {
      var <- input$eda_filter_var
      
      if (is.numeric(df[[var]]) && !is.null(input$eda_filter_range)) {
        df <- df[!is.na(df[[var]]) &
                   df[[var]] >= input$eda_filter_range[1] &
                   df[[var]] <= input$eda_filter_range[2], , drop = FALSE]
      }
      
      if (!is.numeric(df[[var]]) && !is.null(input$eda_filter_levels)) {
        df <- df[as.character(df[[var]]) %in% input$eda_filter_levels, , drop = FALSE]
      }
    }
    
    df
  })
  
  output$eda_plot <- renderPlot({
    req(eda_df(), input$eda_x)
    df <- eda_df()
    
    color_var <- if (!is.null(input$eda_color) && input$eda_color != "None") input$eda_color else NULL
    
    if (input$eda_plot_type == "hist") {
      p <- ggplot(df, aes_string(x = input$eda_x)) +
        geom_histogram(bins = input$eda_bins, fill = "skyblue", color = "black", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Histogram of", input$eda_x),
             x = input$eda_x, y = "Frequency")
      print(p)
    }
    
    if (input$eda_plot_type == "box") {
      if (!is.null(color_var)) {
        p <- ggplot(df, aes_string(x = color_var, y = input$eda_x, fill = color_var)) +
          geom_boxplot(alpha = 0.7) +
          theme_minimal() +
          labs(title = paste("Boxplot of", input$eda_x, "by", color_var),
               x = color_var, y = input$eda_x)
      } else {
        p <- ggplot(df, aes_string(y = input$eda_x)) +
          geom_boxplot(fill = "lightblue", color = "black", alpha = 0.7) +
          theme_minimal() +
          labs(title = paste("Boxplot of", input$eda_x),
               y = input$eda_x, x = "")
      }
      print(p)
    }
    
    if (input$eda_plot_type == "scatter") {
      req(input$eda_y)
      
      if (!is.null(color_var)) {
        p <- ggplot(df, aes_string(x = input$eda_x, y = input$eda_y, color = color_var)) +
          geom_point(alpha = 0.7) +
          theme_minimal() +
          labs(title = paste("Scatterplot of", input$eda_y, "vs", input$eda_x),
               x = input$eda_x, y = input$eda_y)
      } else {
        p <- ggplot(df, aes_string(x = input$eda_x, y = input$eda_y)) +
          geom_point(color = "steelblue", alpha = 0.7) +
          theme_minimal() +
          labs(title = paste("Scatterplot of", input$eda_y, "vs", input$eda_x),
               x = input$eda_x, y = input$eda_y)
      }
      
      if (isTRUE(input$eda_add_lm)) {
        p <- p + geom_smooth(method = "lm", se = FALSE, color = "red")
      }
      
      print(p)
    }
    
    if (input$eda_plot_type == "bar") {
      p <- ggplot(df, aes_string(x = input$eda_x)) +
        geom_bar(fill = "darkgreen", alpha = 0.7) +
        theme_minimal() +
        labs(title = paste("Bar Plot of", input$eda_x),
             x = input$eda_x, y = "Count") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      print(p)
      }
  })
  
  output$eda_summary <- renderPrint({
    req(eda_df(), input$eda_x)
    df <- eda_df()
    
    cat("dataset dimensions:\n")
    print(dim(df))
    
    cat("\nMissing values in filtered data:\n")
    print(sum(is.na(df)))
    
    cat("\nSummary of selected variable:\n")
    print(summary(df[[input$eda_x]]))
    
    if (!is.null(input$eda_y) && input$eda_plot_type == "scatter") {
      cat("\nSummary of Y:\n")
      print(summary(df[[input$eda_y]]))
      
      if (is.numeric(df[[input$eda_x]]) && is.numeric(df[[input$eda_y]])) {
        cat("\nCorrelation between selected x and y:\n")
        print(cor(df[[input$eda_x]], df[[input$eda_y]], use = "complete.obs"))
      }
      }
  })
  
  output$eda_corr <- renderTable({
    req(eda_df())
    df <- eda_df()
    
    num_df <- df[, sapply(df, is.numeric), drop = FALSE]
    
    if (ncol(num_df) < 2) return(NULL)
    
    round(cor(num_df, use = "complete.obs"), 3)
  }, rownames = TRUE)
  
  output$eda_table <- renderDT({
    req(eda_df())
    datatable(head(eda_df(), 20),
              options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
}


shinyApp(ui = ui, server = server)