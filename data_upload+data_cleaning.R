library(shiny)
library(shinydashboard)
library(DT)
library(readr)
library(readxl)
library(jsonlite)

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
      menuItem("Data Cleaning", tabName = "clean",  icon = icon("broom"))
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
}

shinyApp(ui = ui, server = server)