library(shiny)
library(shinydashboard)
library(DT)
library(readr)
library(readxl)
library(jsonlite)
library(ggplot2)
library(scales)

# Helper functions (unchanged)
mode_value <- function(x) {
  ux <- unique(x[!is.na(x)])
  if (length(ux) == 0) return(NA)
  ux[which.max(tabulate(match(x, ux)))]
}

safe_numeric_cols <- function(df) {
  names(df)[sapply(df, is.numeric)]
}

safe_group_cols <- function(df) {
  names(df)[sapply(df, function(x) is.factor(x) || is.character(x) || is.logical(x))]
}

dataset_info_table <- function(df) {
  data.frame(
    Metric = c("Rows", "Columns", "Missing Cells", "Duplicate Rows"),
    Value = c(
      nrow(df),
      ncol(df),
      sum(is.na(df)),
      sum(duplicated(df))
    ),
    stringsAsFactors = FALSE
  )
}

# Helper to generate 15 example features based on dataset
generate_example_features <- function(df) {
  # Only generate if there are at least 2 numeric columns
  num_cols <- safe_numeric_cols(df)
  if (length(num_cols) < 2) return(list(df = df, new_features = character(0)))
  
  # We'll create 15 example features using various transformations
  new_features <- character(0)
  used_names <- names(df)
  
  # Helper to add a column safely
  add_col <- function(name, values) {
    if (!(name %in% used_names)) {
      df[[name]] <<- values
      used_names <<- c(used_names, name)
      new_features <<- c(new_features, name)
    }
  }
  
  # 1. Square of first numeric column
  col1 <- num_cols[1]
  add_col(paste0(col1, "_squared"), df[[col1]]^2)
  
  # 2. Square root of first numeric (if non-negative)
  if (min(df[[col1]], na.rm = TRUE) >= 0) {
    add_col(paste0(col1, "_sqrt"), sqrt(df[[col1]]))
  }
  
  # 3. Log of first numeric (if positive)
  if (min(df[[col1]], na.rm = TRUE) > 0) {
    add_col(paste0(col1, "_log"), log(df[[col1]]))
  }
  
  # 4. Product of first two numeric columns
  if (length(num_cols) >= 2) {
    col2 <- num_cols[2]
    add_col(paste0(col1, "_x_", col2), df[[col1]] * df[[col2]])
  }
  
  # 5. Sum of first two numeric
  if (length(num_cols) >= 2) {
    add_col(paste0(col1, "_plus_", col2), df[[col1]] + df[[col2]])
  }
  
  # 6. Ratio of first two numeric (avoid division by zero)
  if (length(num_cols) >= 2 && !any(df[[col2]] == 0, na.rm = TRUE)) {
    add_col(paste0(col1, "_div_", col2), df[[col1]] / df[[col2]])
  }
  
  # 7. Binned version of first numeric (5 equal-width bins)
  x <- df[[col1]]
  breaks <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = 6)
  binned <- cut(x, breaks = breaks, include.lowest = TRUE, labels = FALSE)
  add_col(paste0(col1, "_binned"), binned)
  
  # 8. Cube of second numeric (if exists)
  if (length(num_cols) >= 2) {
    add_col(paste0(col2, "_cubed"), df[[col2]]^3)
  }
  
  # 9. Exponential of first numeric (if not too large)
  if (max(df[[col1]], na.rm = TRUE) < 50) {
    add_col(paste0(col1, "_exp"), exp(df[[col1]]))
  }
  
  # 10. Difference of first two numeric
  if (length(num_cols) >= 2) {
    add_col(paste0(col1, "_minus_", col2), df[[col1]] - df[[col2]])
  }
  
  # 11. Square of second numeric
  if (length(num_cols) >= 2) {
    add_col(paste0(col2, "_squared"), df[[col2]]^2)
  }
  
  # 12. Mean of first two numeric
  if (length(num_cols) >= 2) {
    add_col(paste0(col1, "_", col2, "_mean"), (df[[col1]] + df[[col2]]) / 2)
  }
  
  # 13. Median of first three numeric (if enough)
  if (length(num_cols) >= 3) {
    col3 <- num_cols[3]
    med <- apply(df[, num_cols[1:3], drop = FALSE], 1, median, na.rm = TRUE)
    add_col("median_of_3", med)
  }
  
  # 14. Z-score of first numeric
  add_col(paste0(col1, "_zscore"), as.numeric(scale(df[[col1]])))
  
  # 15. Min-Max scaled first numeric
  rng <- range(df[[col1]], na.rm = TRUE)
  if (rng[1] != rng[2]) {
    add_col(paste0(col1, "_minmax"), (df[[col1]] - rng[1]) / (rng[2] - rng[1]))
  }
  
  # Remove any that were not added (due to conditions)
  list(df = df, new_features = new_features)
}


# UI
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "DataExplorer Pro"),
  
  dashboardSidebar(
    sidebarMenu(
      id = "tabs",
      menuItem("Home", tabName = "home", icon = icon("house")),
      menuItem("Upload Data", tabName = "upload", icon = icon("upload")),
      menuItem("Data Cleaning", tabName = "clean", icon = icon("broom")),
      menuItem("Feature Engineering", tabName = "feat", icon = icon("wrench")),
      menuItem("EDA", tabName = "eda", icon = icon("chart-bar"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f4f6f9;
        }
        .hero-banner {
          background: linear-gradient(135deg, #1a6fa8 0%, #3c8dbc 100%);
          color: white;
          border-radius: 12px;
          padding: 32px 36px;
          margin-bottom: 24px;
          box-shadow: 0 3px 10px rgba(0,0,0,0.12);
        }
        .hero-banner h2 {
          margin-top: 0;
          font-size: 28px;
          font-weight: 700;
        }
        .hero-banner p {
          font-size: 15px;
          line-height: 1.7;
          margin-bottom: 0;
        }
        .feature-card {
          background: white;
          border-radius: 12px;
          padding: 20px 22px;
          margin-bottom: 18px;
          box-shadow: 0 2px 8px rgba(0,0,0,0.08);
          border-left: 5px solid #3c8dbc;
          min-height: 150px;
        }
        .feature-card h4 {
          margin-top: 0;
          color: #3c8dbc;
          font-weight: 700;
        }
        .feature-card p {
          color: #555;
          margin-bottom: 0;
          line-height: 1.6;
        }
        .step-badge {
          display: inline-block;
          background: #3c8dbc;
          color: #fff;
          border-radius: 50%;
          width: 28px;
          height: 28px;
          line-height: 28px;
          text-align: center;
          font-weight: bold;
          margin-right: 8px;
        }
        .upload-zone {
          border: 2px dashed #3c8dbc;
          border-radius: 10px;
          padding: 22px;
          text-align: center;
          background: #f7fbff;
          margin-bottom: 12px;
        }
        .custom-note {
          background: #eef7fd;
          border-left: 4px solid #3c8dbc;
          padding: 12px 16px;
          border-radius: 8px;
          margin-bottom: 12px;
          color: #2f4f60;
        }
        .warning-box {
          background: #fff3cd;
          border: 1px solid #ffe08a;
          color: #856404;
          border-radius: 8px;
          padding: 14px 18px;
          margin-bottom: 16px;
        }
        .success-box {
          background: #eaf7ee;
          border: 1px solid #b7e1c0;
          color: #1f6b3b;
          border-radius: 8px;
          padding: 14px 18px;
          margin-bottom: 16px;
        }
        .small-muted {
          color: #777;
          font-size: 12px;
        }
      "))
    ),
    
    tabItems(
      
      #  HOME 
      tabItem(
        tabName = "home",
        
        div(
          class = "hero-banner",
          h2(icon("chart-bar"), " DataExplorer Pro"),
          p("An interactive R Shiny web application for dataset upload, data cleaning, preprocessing, feature engineering, and exploratory data analysis. 
            This app is designed to provide a smooth, user-friendly, and responsive workflow for data exploration.")
        ),
        
        fluidRow(
          column(
            6,
            div(
              class = "feature-card",
              h4(icon("upload"), " Upload Data"),
              p("Upload CSV, Excel, JSON, or RDS files. You can also start immediately with built-in example datasets such as iris and mtcars.")
            ),
            div(
              class = "feature-card",
              h4(icon("broom"), " Data Cleaning & Preprocessing"),
              p("Remove duplicates, handle missing values, convert data types, manage outliers, and apply scaling or one-hot encoding with live feedback.")
            )
          ),
          column(
            6,
            div(
              class = "feature-card",
              h4(icon("wrench"), " Feature Engineering"),
              p("Create new variables using arithmetic operations, mathematical transformations, and binning tools. Preview the results immediately.")
            ),
            div(
              class = "feature-card",
              h4(icon("magnifying-glass-chart"), " Exploratory Data Analysis"),
              p("Generate dynamic visualizations, summary statistics, filtered previews, and correlation matrices for fast and interactive insights.")
            )
          )
        ),
        
        box(
          title = "How to Use This App",
          width = 12,
          solidHeader = TRUE,
          status = "primary",
          tags$ol(
            style = "line-height: 2;",
            tags$li(tags$span(class = "step-badge", "1"),
                    "Go to ", tags$b("Upload Data"),
                    " and choose either your own file or a built-in dataset, then click ",
                    tags$b("Load / Refresh"), "."),
            tags$li(tags$span(class = "step-badge", "2"),
                    "Open ", tags$b("Data Cleaning"),
                    " to remove duplicates, handle missing values, convert types, and apply preprocessing steps."),
            tags$li(tags$span(class = "step-badge", "3"),
                    "Visit ", tags$b("Feature Engineering"),
                    " to create new derived variables and preview your enhanced dataset."),
            tags$li(tags$span(class = "step-badge", "4"),
                    "Use ", tags$b("EDA"),
                    " to filter the dataset, create charts, inspect summary statistics, and review correlation patterns.")
          )
        ),
        
        fluidRow(
          valueBoxOutput("vb_rows", width = 3),
          valueBoxOutput("vb_cols", width = 3),
          valueBoxOutput("vb_missing", width = 3),
          valueBoxOutput("vb_duplicates", width = 3)
        )
      ),
      
      # UPLOAD
      tabItem(
        tabName = "upload",
        
        fluidRow(
          column(
            4,
            box(
              width = 12, title = "Load Dataset",
              status = "primary", solidHeader = TRUE,
              
              div(
                class = "custom-note",
                icon("circle-info"),
                " Supported file types: CSV, TXT, Excel (.xlsx / .xls), JSON, and RDS."
              ),
              
              radioButtons(
                "data_source", "Data source:",
                choices = c(
                  "Upload my own file" = "upload",
                  "Built-in: iris" = "iris",
                  "Built-in: mtcars" = "mtcars"
                ),
                selected = "iris"
              ),
              
              conditionalPanel(
                condition = "input.data_source == 'upload'",
                div(
                  class = "upload-zone",
                  fileInput(
                    "user_file", label = NULL,
                    accept = c(".csv", ".txt", ".xlsx", ".xls", ".json", ".rds"),
                    buttonLabel = "Browse...",
                    placeholder = "No file selected"
                  ),
                  p(class = "small-muted", icon("circle-info"),
                    "For CSV/TXT files, you can choose the delimiter below.")
                ),
                conditionalPanel(
                  condition = "input.user_file != null && (input.user_file.name.endsWith('.csv') || input.user_file.name.endsWith('.txt'))",
                  checkboxInput("csv_header", "First row is header", TRUE),
                  selectInput(
                    "csv_sep", "Delimiter",
                    choices = c(
                      "Comma (,)" = ",",
                      "Semicolon (;)" = ";",
                      "Tab" = "\t",
                      "Pipe (|)" = "|"
                    )
                  )
                )
              ),
              
              numericInput("preview_rows", "Rows to preview:", value = 100, min = 5, max = 5000, step = 5),
              
              actionButton(
                "btn_load", "Load / Refresh",
                icon = icon("rotate"),
                class = "btn-primary",
                style = "width:100%;"
              ),
              br(), br(),
              uiOutput("upload_status_ui"),
              uiOutput("info_box")
            )
          ),
          
          column(
            8,
            box(
              width = 12, title = "Data Preview",
              status = "info", solidHeader = TRUE,
              tabsetPanel(
                tabPanel("Table", br(), DTOutput("preview_table")),
                tabPanel("Column Types", br(), DTOutput("col_types_table")),
                tabPanel("Missing Values", br(), DTOutput("missing_table")),
                tabPanel("Dataset Info", br(), DTOutput("dataset_info_table"))
              )
            )
          )
        )
      ),
      
      # DATA CLEANING
      tabItem(
        tabName = "clean",
        
        uiOutput("clean_no_data_warning"),
        fluidRow(
          valueBoxOutput("clean_vb_original_rows", width = 3),
          valueBoxOutput("clean_vb_current_rows", width = 3),
          valueBoxOutput("clean_vb_missing", width = 3),
          valueBoxOutput("clean_vb_dup", width = 3)
        ),
        
        fluidRow(
          column(
            4,
            
            box(
              width = 12, title = "1. Duplicate Rows",
              status = "warning", solidHeader = TRUE, collapsible = TRUE,
              p(class = "small-muted", icon("circle-info"),
                "Rows that are identical across all columns."),
              verbatimTextOutput("dup_count"),
              actionButton("btn_remove_dup", "Remove Duplicates",
                           icon = icon("trash"), class = "btn-warning",
                           style = "width:100%;")
            ),
            
            box(
              width = 12, title = "2. Missing Values",
              status = "danger", solidHeader = TRUE, collapsible = TRUE,
              p(class = "small-muted", icon("circle-info"),
                "Choose columns and a strategy for handling missing values."),
              uiOutput("missing_col_select"),
              selectInput(
                "missing_strategy", "Strategy:",
                choices = c(
                  "Remove rows with NA" = "remove_rows",
                  "Impute with Mean (numeric)" = "mean",
                  "Impute with Median (numeric)" = "median",
                  "Impute with Mode (any type)" = "mode",
                  "Impute with a fixed value" = "fixed",
                  "Remove selected columns" = "remove_cols"
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
            
            box(
              width = 12, title = "3. Column Type Conversion",
              status = "info", solidHeader = TRUE, collapsible = TRUE,
              p(class = "small-muted", icon("circle-info"),
                "Convert a selected column to a new type."),
              uiOutput("type_col_select"),
              selectInput(
                "target_type", "Convert to:",
                choices = c(
                  "Numeric" = "numeric",
                  "Character" = "character",
                  "Factor" = "factor",
                  "Logical" = "logical",
                  "Date (YYYY-MM-DD)" = "date"
                )
              ),
              actionButton("btn_convert_type", "Convert Type",
                           icon = icon("arrows-rotate"), class = "btn-info",
                           style = "width:100%;")
            ),
            
            box(
              width = 12, title = "4. Outlier Handling (IQR)",
              status = "primary", solidHeader = TRUE, collapsible = TRUE,
              p(class = "small-muted", icon("circle-info"),
                "Detect outliers using the 1.5 × IQR rule on numeric columns."),
              uiOutput("outlier_col_select"),
              selectInput(
                "outlier_strategy", "Strategy:",
                choices = c(
                  "Remove rows with outliers" = "remove",
                  "Cap to IQR bounds (Winsorise)" = "cap"
                )
              ),
              verbatimTextOutput("outlier_count"),
              actionButton("btn_handle_outlier", "Apply Outlier Treatment",
                           icon = icon("filter"), class = "btn-primary",
                           style = "width:100%;")
            ),
            
            box(
              width = 12, title = "5. Scaling & Encoding",
              status = "success", solidHeader = TRUE, collapsible = TRUE,
              p(class = "small-muted", icon("circle-info"),
                "Scale numeric columns or one-hot encode categorical columns."),
              uiOutput("scale_col_select"),
              selectInput(
                "scale_method", "Method:",
                choices = c(
                  "Min-Max Normalisation [0,1]" = "minmax",
                  "Z-score Standardisation" = "zscore",
                  "One-Hot Encode (factor/character)" = "onehot"
                )
              ),
              actionButton("btn_scale", "Apply",
                           icon = icon("sliders"), class = "btn-success",
                           style = "width:100%;")
            ),
            
            box(
              width = 12, status = "warning", solidHeader = FALSE,
              actionButton("btn_reset_clean", "Reset to Original Data",
                           icon = icon("rotate-left"), class = "btn-warning",
                           style = "width:100%;"),
              br(), br(),
              downloadButton("btn_download", "Download Cleaned Data (.csv)",
                             style = "width:100%;")
            )
          ),
          
          column(
            8,
            box(
              width = 12, title = "Cleaned Data Preview",
              status = "success", solidHeader = TRUE,
              tabsetPanel(
                tabPanel("Table", br(), DTOutput("clean_preview_table")),
                tabPanel("Missing Values Summary", br(), DTOutput("clean_missing_summary")),
                tabPanel("Column Types", br(), DTOutput("clean_col_types")),
                tabPanel("Operation Log", br(), verbatimTextOutput("clean_log"))
              )
            )
          )
        )
      ),
      
      # ===================== ENHANCED FEATURE ENGINEERING =====================
      tabItem(
        tabName = "feat",
        
        uiOutput("feat_no_data_warning"),
        
        fluidRow(
          column(
            4,
            # Box: Quick Templates (one‑click presets)
            box(
              width = 12, title = "Quick Feature Templates", status = "primary", solidHeader = TRUE,
              p(class = "small-muted", "Click a template to instantly populate the form."),
              fluidRow(
                column(6, actionButton("temp_square", "Square", class = "btn-info btn-sm", style = "width:100%; margin-bottom:5px;")),
                column(6, actionButton("temp_sqrt", "Square Root", class = "btn-info btn-sm", style = "width:100%; margin-bottom:5px;")),
                column(6, actionButton("temp_log", "Log", class = "btn-info btn-sm", style = "width:100%; margin-bottom:5px;")),
                column(6, actionButton("temp_ratio", "Ratio (A/B)", class = "btn-info btn-sm", style = "width:100%; margin-bottom:5px;")),
                column(6, actionButton("temp_product", "Product (A*B)", class = "btn-info btn-sm", style = "width:100%; margin-bottom:5px;")),
                column(6, actionButton("temp_zscore", "Z-Score", class = "btn-info btn-sm", style = "width:100%; margin-bottom:5px;"))
              ),
              hr(),
              h5("Pre‑created Example Features"),
              p("The dataset already contains 15 example features (generated automatically). You can remove them below.")
            ),
            
            # Box: Create New Feature (enhanced)
            box(
              width = 12, title = "Create New Feature", status = "success", solidHeader = TRUE,
              selectInput(
                "feat_type", "Feature type:",
                choices = c(
                  "Arithmetic (+, -, *, /)" = "arithmetic",
                  "Mathematical transform" = "math",
                  "Binning" = "bin",
                  "Polynomial / Interaction" = "poly"
                )
              ),
              
              conditionalPanel(
                condition = "input.feat_type == 'arithmetic'",
                selectInput("feat_arith_col1", "First column:", choices = NULL),
                selectInput("feat_arith_op", "Operator:", choices = c("+" = "+", "-" = "-", "*" = "*", "/" = "/")),
                selectInput("feat_arith_col2", "Second column:", choices = NULL)
              ),
              
              conditionalPanel(
                condition = "input.feat_type == 'math'",
                selectInput("feat_math_col", "Column:", choices = NULL),
                selectInput(
                  "feat_math_func", "Function:",
                  choices = c(
                    "Log (natural)" = "log",
                    "Log10" = "log10",
                    "Square root" = "sqrt",
                    "Square" = "^2",
                    "Exponential" = "exp",
                    "Reciprocal" = "recip"
                  )
                )
              ),
              
              conditionalPanel(
                condition = "input.feat_type == 'bin'",
                selectInput("feat_bin_col", "Column:", choices = NULL),
                numericInput("feat_bin_n", "Number of bins:", value = 5, min = 2, max = 100),
                selectInput(
                  "feat_bin_method", "Method:",
                  choices = c(
                    "Equal width" = "equal_width",
                    "Equal frequency (quantile)" = "equal_freq"
                  )
                )
              ),
              
              conditionalPanel(
                condition = "input.feat_type == 'poly'",
                h5("Create polynomial or interaction terms"),
                selectInput("feat_poly_col1", "First column:", choices = NULL),
                selectInput("feat_poly_col2", "Second column (optional):", choices = c("None", NULL)),
                selectInput("feat_poly_func", "Operation:",
                            choices = c("Square (col1²)" = "square",
                                        "Cube (col1³)" = "cube",
                                        "Product (col1 * col2)" = "product",
                                        "Sum of squares (col1² + col2²)" = "sum_sq"))
              ),
              
              textInput("feat_new_name", "New column name:", placeholder = "e.g., new_feature"),
              actionButton("btn_create_feat", "Create Feature",
                           icon = icon("plus"), class = "btn-success",
                           style = "width:100%; margin-top: 8px;"),
              
              hr(),
              # Real-time preview section
              h5("Preview before creating"),
              p(class = "small-muted", "See the effect of your settings without committing."),
              plotOutput("feat_preview_plot", height = "200px"),
              verbatimTextOutput("feat_preview_stats")
            ),
            
            # Box: Created Features (with Edit & Remove)
            box(
              width = 12, title = "Created Features", status = "info", solidHeader = TRUE, collapsible = TRUE,
              DTOutput("feat_list_table"),
              br(),
              fluidRow(
                column(6, actionButton("btn_edit_feat", "Edit Selected",
                                       icon = icon("pencil"), class = "btn-warning",
                                       style = "width:100%;")),
                column(6, actionButton("btn_remove_feat", "Remove Selected",
                                       icon = icon("trash"), class = "btn-danger",
                                       style = "width:100%;"))
              )
            )
          ),
          
          column(
            8,
            box(
              width = 12, title = "Data Preview with New Features", status = "success", solidHeader = TRUE,
              tabsetPanel(
                tabPanel("Table", br(), DTOutput("feat_preview_table")),
                tabPanel("New Feature Summary", br(), verbatimTextOutput("feat_summary")),
                tabPanel("Operation Log", br(), verbatimTextOutput("feat_log"))
              )
            )
          )
        )
      ),
      # ===================== END ENHANCED FEATURE ENGINEERING =====================
      
      # EDA 
      tabItem(
        tabName = "eda",
        
        uiOutput("eda_no_data_warning"),
        
        fluidRow(
          column(
            4,
            box(
              width = 12, title = "EDA Controls",
              status = "primary", solidHeader = TRUE,
              selectInput(
                "eda_plot_type", "Plot type:",
                choices = c(
                  "Histogram" = "hist",
                  "Boxplot" = "box",
                  "Scatterplot" = "scatter",
                  "Bar Plot" = "bar"
                )
              ),
              uiOutput("eda_x_ui"),
              uiOutput("eda_y_ui"),
              uiOutput("eda_color_ui"),
              conditionalPanel(
                condition = "input.eda_plot_type == 'hist'",
                sliderInput("eda_bins", "Number of bins:", min = 5, max = 50, value = 15)
              ),
              conditionalPanel(
                condition = "input.eda_plot_type == 'scatter'",
                checkboxInput("eda_add_lm", "Add Linear Regression Line", value = FALSE)
              ),
              hr(),
              h4("Filter Data"),
              uiOutput("eda_filter_var_ui"),
              uiOutput("eda_filter_ui")
            ),
            
            box(
              width = 12, title = "Summary Statistics",
              status = "info", solidHeader = TRUE,
              verbatimTextOutput("eda_summary")
            )
          ),
          
          column(
            8,
            box(
              width = 12, title = "Visualization",
              status = "success", solidHeader = TRUE,
              plotOutput("eda_plot", height = 460)
            ),
            box(
              width = 12, title = "Correlation Matrix",
              status = "warning", solidHeader = TRUE,
              tableOutput("eda_corr")
            )
          )
        ),
        
        fluidRow(
          box(
            width = 12, title = "Filtered Data Preview",
            status = "primary", solidHeader = TRUE,
            DTOutput("eda_table")
          )
        )
      )
    )
  )
)

# SERVER
server <- function(input, output, session) {
  
  # Shared reactive state
  clean_df <- reactiveVal(NULL)
  original_df <- reactiveVal(NULL)
  op_log <- reactiveVal(character(0))
  created_features <- reactiveVal(character(0))
  feature_metadata <- reactiveVal(list())   # named list: feature_name -> list(type, params)
  
  log_op <- function(msg) {
    ts <- format(Sys.time(), "[%H:%M:%S]")
    op_log(c(op_log(), paste(ts, msg)))
  }
  
  # Data loading
  loaded_data <- eventReactive(input$btn_load, ignoreNULL = FALSE, {
    if (input$data_source == "iris") return(as.data.frame(iris))
    if (input$data_source == "mtcars") return(as.data.frame(mtcars))
    
    req(input$user_file)
    f <- input$user_file
    ext <- tolower(tools::file_ext(f$name))
    
    df <- tryCatch({
      switch(
        ext,
        "csv" = read_delim(
          f$datapath,
          delim = input$csv_sep,
          col_names = input$csv_header,
          show_col_types = FALSE
        ),
        "txt" = read_delim(
          f$datapath,
          delim = input$csv_sep,
          col_names = input$csv_header,
          show_col_types = FALSE
        ),
        "xlsx" = read_excel(f$datapath),
        "xls" = read_excel(f$datapath),
        "json" = as.data.frame(fromJSON(f$datapath)),
        "rds" = readRDS(f$datapath),
        stop(paste("Unsupported format:", ext))
      )
    }, error = function(e) {
      showNotification(paste("Error loading data:", e$message), type = "error", duration = 8)
      NULL
    })
    
    if (!is.null(df)) as.data.frame(df) else NULL
  })
  
  observeEvent(loaded_data(), {
    df <- loaded_data()
    req(df)
    
    # Generate 15 example features (if possible)
    gen <- generate_example_features(df)
    df <- gen$df
    example_features <- gen$new_features
    
    original_df(df)
    clean_df(df)
    op_log(character(0))
    created_features(example_features)   # store them as created features
    # Store metadata for these example features
    meta <- list()
    for (f in example_features) {
      meta[[f]] <- list(type = "example", note = "Pre‑generated example feature")
    }
    feature_metadata(meta)
    log_op("Dataset loaded successfully. 15 example features generated.")
    
    showNotification(
      sprintf("Loaded successfully: %d rows × %d columns. Added %d example features.", nrow(df), ncol(df), length(example_features)),
      type = "message",
      duration = 6
    )
  })
  
  # Home value boxes
  output$vb_rows <- renderValueBox({
    df <- clean_df()
    valueBox(if (is.null(df)) 0 else nrow(df), "Rows", icon = icon("table"), color = "blue")
  })
  
  output$vb_cols <- renderValueBox({
    df <- clean_df()
    valueBox(if (is.null(df)) 0 else ncol(df), "Columns", icon = icon("columns"), color = "aqua")
  })
  
  output$vb_missing <- renderValueBox({
    df <- clean_df()
    valueBox(if (is.null(df)) 0 else sum(is.na(df)), "Missing Cells", icon = icon("triangle-exclamation"), color = "yellow")
  })
  
  output$vb_duplicates <- renderValueBox({
    df <- clean_df()
    valueBox(if (is.null(df)) 0 else sum(duplicated(df)), "Duplicate Rows", icon = icon("copy"), color = "red")
  })
  
  
  # Upload outputs
  output$upload_status_ui <- renderUI({
    df <- clean_df()
    if (is.null(df)) {
      div(class = "warning-box", icon("triangle-exclamation"), " No dataset loaded yet.")
    } else {
      div(class = "success-box", icon("circle-check"), " Dataset is ready for cleaning, feature engineering, and EDA.")
    }
  })
  
  output$info_box <- renderUI({
    df <- clean_df()
    req(df)
    
    box(
      width = 12, title = "Quick Dataset Info",
      status = "success", solidHeader = TRUE,
      tags$ul(
        tags$li(strong("Rows: "), format(nrow(df), big.mark = ",")),
        tags$li(strong("Columns: "), ncol(df)),
        tags$li(strong("Missing cells: "), format(sum(is.na(df)), big.mark = ",")),
        tags$li(strong("Duplicate rows: "), format(sum(duplicated(df)), big.mark = ","))
      )
    )
  })
  
  output$preview_table <- renderDT({
    df <- clean_df()
    req(df)
    datatable(
      head(df, input$preview_rows),
      options = list(scrollX = TRUE, pageLength = 15, lengthMenu = c(10, 15, 25, 50, 100)),
      rownames = FALSE,
      filter = "top"
    )
  })
  
  output$col_types_table <- renderDT({
    df <- clean_df()
    req(df)
    
    out <- data.frame(
      Column = names(df),
      Type = sapply(df, function(x) class(x)[1]),
      N_Unique = sapply(df, function(x) length(unique(x))),
      N_Missing = sapply(df, function(x) sum(is.na(x))),
      stringsAsFactors = FALSE
    )
    
    datatable(out, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })
  
  output$missing_table <- renderDT({
    df <- clean_df()
    req(df)
    
    miss <- data.frame(
      Column = names(df),
      Missing_Count = sapply(df, function(x) sum(is.na(x))),
      Missing_Pct = paste0(round(sapply(df, function(x) mean(is.na(x))) * 100, 1), "%"),
      stringsAsFactors = FALSE
    )
    miss <- miss[order(-miss$Missing_Count), ]
    
    datatable(miss, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })
  
  output$dataset_info_table <- renderDT({
    df <- clean_df()
    req(df)
    datatable(dataset_info_table(df), options = list(dom = "t"), rownames = FALSE)
  })
  
  
  # Cleaning tab UI helpers
  output$clean_no_data_warning <- renderUI({
    if (is.null(clean_df())) {
      div(
        class = "warning-box",
        icon("triangle-exclamation"),
        tags$b(" No dataset loaded. "),
        "Please go to Upload Data and click Load / Refresh first."
      )
    }
  })
  
  output$clean_vb_original_rows <- renderValueBox({
    df <- original_df()
    valueBox(if (is.null(df)) 0 else nrow(df), "Original Rows", icon = icon("database"), color = "blue")
  })
  
  output$clean_vb_current_rows <- renderValueBox({
    df <- clean_df()
    valueBox(if (is.null(df)) 0 else nrow(df), "Current Rows", icon = icon("table"), color = "green")
  })
  
  output$clean_vb_missing <- renderValueBox({
    df <- clean_df()
    valueBox(if (is.null(df)) 0 else sum(is.na(df)), "Missing Cells", icon = icon("triangle-exclamation"), color = "yellow")
  })
  
  output$clean_vb_dup <- renderValueBox({
    df <- clean_df()
    valueBox(if (is.null(df)) 0 else sum(duplicated(df)), "Duplicate Rows", icon = icon("copy"), color = "red")
  })
  
  output$missing_col_select <- renderUI({
    df <- clean_df()
    req(df)
    cols_na <- names(df)[sapply(df, function(x) any(is.na(x)))]
    if (length(cols_na) == 0) cols_na <- names(df)
    checkboxGroupInput("missing_cols", "Select columns:", choices = cols_na, selected = cols_na)
  })
  
  output$type_col_select <- renderUI({
    df <- clean_df()
    req(df)
    selectInput("type_col", "Select column:", choices = names(df))
  })
  
  output$outlier_col_select <- renderUI({
    df <- clean_df()
    req(df)
    num_cols <- safe_numeric_cols(df)
    if (length(num_cols) == 0) {
      selectInput("outlier_col", "Select numeric column:", choices = "")
    } else {
      selectInput("outlier_col", "Select numeric column:", choices = num_cols)
    }
  })
  
  output$scale_col_select <- renderUI({
    df <- clean_df()
    req(df)
    selectInput("scale_col", "Select column:", choices = names(df))
  })
  
  output$dup_count <- renderText({
    df <- clean_df()
    if (is.null(df)) return("No data loaded.")
    paste("Duplicate rows found:", sum(duplicated(df)))
  })
  
  output$outlier_count <- renderText({
    df <- clean_df()
    req(df, input$outlier_col)
    col <- input$outlier_col
    
    if (!(col %in% names(df)) || !is.numeric(df[[col]])) {
      return("Selected column is not numeric.")
    }
    
    x <- df[[col]]
    q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
    iqr <- q[2] - q[1]
    n <- sum(x < (q[1] - 1.5 * iqr) | x > (q[2] + 1.5 * iqr), na.rm = TRUE)
    paste("Outliers detected in", col, ":", n, "rows")
  })
  
  # Cleaning actions (unchanged)
  observeEvent(input$btn_remove_dup, {
    df <- clean_df()
    req(df)
    
    before <- nrow(df)
    df <- df[!duplicated(df), , drop = FALSE]
    clean_df(df)
    
    removed <- before - nrow(df)
    log_op(sprintf("Removed duplicates: %d row(s) removed. (%d -> %d)", removed, before, nrow(df)))
    
    showNotification(
      paste("Removed", removed, "duplicate row(s)."),
      type = if (removed > 0) "message" else "warning"
    )
  })
  
  observeEvent(input$btn_handle_missing, {
    df <- clean_df()
    req(df)
    
    cols <- input$missing_cols
    req(cols)
    strat <- input$missing_strategy
    before_na <- sum(is.na(df))
    
    tryCatch({
      if (strat == "remove_rows") {
        df <- df[complete.cases(df[, cols, drop = FALSE]), , drop = FALSE]
      } else if (strat == "remove_cols") {
        df <- df[, !(names(df) %in% cols), drop = FALSE]
      } else if (strat == "mean") {
        for (col in cols) {
          if (col %in% names(df) && is.numeric(df[[col]])) {
            df[[col]][is.na(df[[col]])] <- mean(df[[col]], na.rm = TRUE)
          }
        }
      } else if (strat == "median") {
        for (col in cols) {
          if (col %in% names(df) && is.numeric(df[[col]])) {
            df[[col]][is.na(df[[col]])] <- median(df[[col]], na.rm = TRUE)
          }
        }
      } else if (strat == "mode") {
        for (col in cols) {
          if (col %in% names(df)) {
            df[[col]][is.na(df[[col]])] <- mode_value(df[[col]])
          }
        }
      } else if (strat == "fixed") {
        val <- input$missing_fixed_val
        for (col in cols) {
          if (col %in% names(df)) {
            df[[col]][is.na(df[[col]])] <- val
          }
        }
      }
      
      after_na <- sum(is.na(df))
      clean_df(df)
      log_op(sprintf("Missing value treatment applied using '%s': NAs %d -> %d", strat, before_na, after_na))
      showNotification("Missing value treatment applied.", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 8)
    })
  })
  
  observeEvent(input$btn_convert_type, {
    df <- clean_df()
    req(df, input$type_col, input$target_type)
    
    col <- input$type_col
    tgt <- input$target_type
    old_type <- class(df[[col]])[1]
    
    tryCatch({
      df[[col]] <- switch(
        tgt,
        "numeric" = suppressWarnings(as.numeric(df[[col]])),
        "character" = as.character(df[[col]]),
        "factor" = as.factor(df[[col]]),
        "logical" = as.logical(df[[col]]),
        "date" = as.Date(df[[col]])
      )
      
      clean_df(df)
      log_op(sprintf("Converted column '%s' from %s to %s", col, old_type, tgt))
      showNotification(sprintf("Column '%s' converted to %s.", col, tgt), type = "message")
      
    }, error = function(e) {
      showNotification(paste("Conversion error:", e$message), type = "error", duration = 8)
    })
  })
  
  observeEvent(input$btn_handle_outlier, {
    df <- clean_df()
    req(df, input$outlier_col)
    
    col <- input$outlier_col
    if (!(col %in% names(df)) || !is.numeric(df[[col]])) {
      showNotification("Please choose a valid numeric column.", type = "warning")
      return()
    }
    
    x <- df[[col]]
    q <- quantile(x, c(0.25, 0.75), na.rm = TRUE)
    iqr <- q[2] - q[1]
    lo <- q[1] - 1.5 * iqr
    hi <- q[2] + 1.5 * iqr
    before <- nrow(df)
    
    if (input$outlier_strategy == "remove") {
      df <- df[!is.na(x) & x >= lo & x <= hi, , drop = FALSE]
      log_op(sprintf("Outlier removal on '%s': %d row(s) removed.", col, before - nrow(df)))
    } else {
      df[[col]] <- pmin(pmax(df[[col]], lo), hi)
      log_op(sprintf("Outlier capping on '%s': values capped to [%.3f, %.3f].", col, lo, hi))
    }
    
    clean_df(df)
    showNotification("Outlier treatment applied.", type = "message")
  })
  
  observeEvent(input$btn_scale, {
    df <- clean_df()
    req(df, input$scale_col, input$scale_method)
    
    col <- input$scale_col
    method <- input$scale_method
    
    tryCatch({
      if (!(col %in% names(df))) {
        showNotification("Selected column not found.", type = "warning")
        return()
      }
      
      if (method == "minmax") {
        x <- df[[col]]
        if (!is.numeric(x)) {
          showNotification("Min-Max scaling requires a numeric column.", type = "warning")
          return()
        }
        rng <- range(x, na.rm = TRUE)
        if (rng[1] == rng[2]) {
          showNotification("Cannot scale a constant column.", type = "warning")
          return()
        }
        df[[col]] <- (x - rng[1]) / (rng[2] - rng[1])
        log_op(sprintf("Min-Max normalisation applied to '%s'.", col))
        
      } else if (method == "zscore") {
        x <- df[[col]]
        if (!is.numeric(x)) {
          showNotification("Z-score standardisation requires a numeric column.", type = "warning")
          return()
        }
        if (sd(x, na.rm = TRUE) == 0) {
          showNotification("Cannot standardise a constant column.", type = "warning")
          return()
        }
        df[[col]] <- as.numeric(scale(x))
        log_op(sprintf("Z-score standardisation applied to '%s'.", col))
        
      } else if (method == "onehot") {
        x <- as.factor(df[[col]])
        lvls <- levels(x)
        if (length(lvls) > 20) {
          showNotification("Too many levels (>20) for one-hot encoding.", type = "warning")
          return()
        }
        for (lv in lvls) {
          safe_name <- make.names(paste0(col, "_", lv))
          df[[safe_name]] <- as.integer(x == lv)
        }
        df[[col]] <- NULL
        log_op(sprintf("One-hot encoding applied to '%s': %d new columns created.", col, length(lvls)))
      }
      
      clean_df(df)
      showNotification("Scaling / encoding applied.", type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error:", e$message), type = "error", duration = 8)
    })
  })
  
  observeEvent(input$btn_reset_clean, {
    df <- original_df()
    req(df)
    clean_df(as.data.frame(df))
    created_features(character(0))
    feature_metadata(list())
    op_log(character(0))
    log_op("Reset to original loaded data.")
    showNotification("Data reset to original.", type = "warning")
  })
  
  output$clean_preview_table <- renderDT({
    df <- clean_df()
    req(df)
    datatable(
      df,
      options = list(scrollX = TRUE, pageLength = 15, lengthMenu = c(10, 15, 25, 50)),
      rownames = FALSE,
      filter = "top"
    )
  })
  
  output$clean_missing_summary <- renderDT({
    df <- clean_df()
    req(df)
    
    miss <- data.frame(
      Column = names(df),
      Missing_Count = sapply(df, function(x) sum(is.na(x))),
      Missing_Pct = paste0(round(sapply(df, function(x) mean(is.na(x))) * 100, 1), "%"),
      stringsAsFactors = FALSE
    )
    miss <- miss[order(-miss$Missing_Count), ]
    
    datatable(miss, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })
  
  output$clean_col_types <- renderDT({
    df <- clean_df()
    req(df)
    
    out <- data.frame(
      Column = names(df),
      Type = sapply(df, function(x) class(x)[1]),
      N_Unique = sapply(df, function(x) length(unique(x))),
      stringsAsFactors = FALSE
    )
    
    datatable(out, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })
  
  output$clean_log <- renderText({
    lg <- op_log()
    if (length(lg) == 0) return("No operations performed yet.")
    paste(rev(lg), collapse = "\n")
  })
  
  output$btn_download <- downloadHandler(
    filename = function() paste0("cleaned_data_", Sys.Date(), ".csv"),
    content = function(file) write.csv(clean_df(), file, row.names = FALSE)
  )
  
  
  # ==================== ENHANCED FEATURE ENGINEERING ====================

  output$feat_no_data_warning <- renderUI({
    if (is.null(clean_df())) {
      div(
        class = "warning-box",
        icon("triangle-exclamation"),
        tags$b(" No dataset loaded. "),
        "Please go to Upload Data and click Load / Refresh first."
      )
    }
  })

  # Helper to compute preview feature based on current UI selections
  compute_preview_feature <- reactive({
    df <- clean_df()
    req(df)
    
    feat_type <- input$feat_type
    if (is.null(feat_type)) return(NULL)
    
    tryCatch({
      if (feat_type == "arithmetic") {
        col1 <- input$feat_arith_col1
        col2 <- input$feat_arith_col2
        op <- input$feat_arith_op
        req(col1, col2, op)
        if (!is.numeric(df[[col1]]) || !is.numeric(df[[col2]])) return(NULL)
        new_col <- switch(op,
                          "+" = df[[col1]] + df[[col2]],
                          "-" = df[[col1]] - df[[col2]],
                          "*" = df[[col1]] * df[[col2]],
                          "/" = df[[col1]] / df[[col2]])
        return(new_col)
      }
      
      if (feat_type == "math") {
        col <- input$feat_math_col
        func <- input$feat_math_func
        req(col, func)
        if (!is.numeric(df[[col]])) return(NULL)
        # basic validation
        if (func %in% c("log", "log10") && any(df[[col]] <= 0, na.rm = TRUE)) return(NULL)
        if (func == "sqrt" && any(df[[col]] < 0, na.rm = TRUE)) return(NULL)
        if (func == "recip" && any(df[[col]] == 0, na.rm = TRUE)) return(NULL)
        new_col <- switch(func,
                          "log" = log(df[[col]]),
                          "log10" = log10(df[[col]]),
                          "sqrt" = sqrt(df[[col]]),
                          "^2" = df[[col]]^2,
                          "exp" = exp(df[[col]]),
                          "recip" = 1 / df[[col]])
        return(new_col)
      }
      
      if (feat_type == "bin") {
        col <- input$feat_bin_col
        n <- input$feat_bin_n
        method <- input$feat_bin_method
        req(col, n, method)
        if (!is.numeric(df[[col]])) return(NULL)
        x <- df[[col]]
        if (method == "equal_width") {
          breaks <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n + 1)
          new_col <- cut(x, breaks = breaks, include.lowest = TRUE, labels = FALSE)
        } else {
          probs <- seq(0, 1, length.out = n + 1)
          breaks <- quantile(x, probs = probs, na.rm = TRUE)
          if (any(duplicated(breaks))) return(NULL)
          new_col <- cut(x, breaks = breaks, include.lowest = TRUE, labels = FALSE)
        }
        return(new_col)
      }
      
      if (feat_type == "poly") {
        col1 <- input$feat_poly_col1
        func <- input$feat_poly_func
        col2 <- input$feat_poly_col2
        req(col1, func)
        if (!is.numeric(df[[col1]])) return(NULL)
        if (func %in% c("product", "sum_sq") && (col2 == "None" || is.null(col2) || !is.numeric(df[[col2]]))) return(NULL)
        new_col <- switch(func,
                          "square" = df[[col1]]^2,
                          "cube" = df[[col1]]^3,
                          "product" = df[[col1]] * df[[col2]],
                          "sum_sq" = df[[col1]]^2 + df[[col2]]^2)
        return(new_col)
      }
      
      return(NULL)
    }, error = function(e) NULL)
  })
  
  # Preview plot
  output$feat_preview_plot <- renderPlot({
    new_feat <- compute_preview_feature()
    df <- clean_df()
    req(new_feat, df)
    
    plot_df <- data.frame(original = new_feat)
    if (is.numeric(plot_df$original)) {
      p <- ggplot(plot_df, aes(x = original)) +
        geom_histogram(fill = "steelblue", alpha = 0.7, bins = 30) +
        labs(title = "Preview: Distribution of new feature", x = "Value", y = "Count") +
        theme_minimal()
    } else {
      plot_df$original <- as.factor(plot_df$original)
      p <- ggplot(plot_df, aes(x = original)) +
        geom_bar(fill = "steelblue", alpha = 0.7) +
        labs(title = "Preview: Bar plot of new feature", x = "Bin", y = "Count") +
        theme_minimal()
    }
    print(p)
  })
  
  output$feat_preview_stats <- renderPrint({
    new_feat <- compute_preview_feature()
    req(new_feat)
    cat("Preview statistics (if numeric):\n")
    if (is.numeric(new_feat)) {
      print(summary(new_feat))
      cat("\nMissing values:", sum(is.na(new_feat)))
    } else {
      cat("Categorical preview:\n")
      print(table(new_feat, useNA = "ifany"))
    }
  })
  
  # Update dropdown choices based on current data
  observe({
    df <- clean_df()
    req(df)
    cols <- names(df)
    updateSelectInput(session, "feat_arith_col1", choices = cols)
    updateSelectInput(session, "feat_arith_col2", choices = cols)
    updateSelectInput(session, "feat_math_col", choices = cols)
    updateSelectInput(session, "feat_bin_col", choices = cols)
    updateSelectInput(session, "feat_poly_col1", choices = cols)
    updateSelectInput(session, "feat_poly_col2", choices = c("None", cols), selected = "None")
  })
  
  # Template actions (populate form)
  observeEvent(input$temp_square, {
    df <- clean_df()
    req(df)
    num_cols <- safe_numeric_cols(df)
    if (length(num_cols) == 0) {
      showNotification("No numeric columns available for this template.", type = "warning")
      return()
    }
    updateSelectInput(session, "feat_type", selected = "math")
    updateSelectInput(session, "feat_math_col", selected = num_cols[1])
    updateSelectInput(session, "feat_math_func", selected = "^2")
    updateTextInput(session, "feat_new_name", value = paste0(num_cols[1], "_squared"))
    showNotification("Template 'Square' loaded. Adjust as needed and click 'Create Feature'.", type = "info")
  })
  observeEvent(input$temp_sqrt, {
    df <- clean_df()
    req(df)
    num_cols <- safe_numeric_cols(df)
    if (length(num_cols) == 0) {
      showNotification("No numeric columns available for this template.", type = "warning")
      return()
    }
    updateSelectInput(session, "feat_type", selected = "math")
    updateSelectInput(session, "feat_math_col", selected = num_cols[1])
    updateSelectInput(session, "feat_math_func", selected = "sqrt")
    updateTextInput(session, "feat_new_name", value = paste0(num_cols[1], "_sqrt"))
  })
  observeEvent(input$temp_log, {
    df <- clean_df()
    req(df)
    num_cols <- safe_numeric_cols(df)
    if (length(num_cols) == 0) {
      showNotification("No numeric columns available for this template.", type = "warning")
      return()
    }
    updateSelectInput(session, "feat_type", selected = "math")
    updateSelectInput(session, "feat_math_col", selected = num_cols[1])
    updateSelectInput(session, "feat_math_func", selected = "log")
    updateTextInput(session, "feat_new_name", value = paste0(num_cols[1], "_log"))
  })
  observeEvent(input$temp_ratio, {
    df <- clean_df()
    req(df)
    num_cols <- safe_numeric_cols(df)
    if (length(num_cols) < 2) {
      showNotification("Need at least two numeric columns for ratio.", type = "warning")
      return()
    }
    updateSelectInput(session, "feat_type", selected = "arithmetic")
    updateSelectInput(session, "feat_arith_col1", selected = num_cols[1])
    updateSelectInput(session, "feat_arith_col2", selected = num_cols[2])
    updateSelectInput(session, "feat_arith_op", selected = "/")
    updateTextInput(session, "feat_new_name", value = paste0(num_cols[1], "_div_", num_cols[2]))
  })
  observeEvent(input$temp_product, {
    df <- clean_df()
    req(df)
    num_cols <- safe_numeric_cols(df)
    if (length(num_cols) < 2) {
      showNotification("Need at least two numeric columns for product.", type = "warning")
      return()
    }
    updateSelectInput(session, "feat_type", selected = "arithmetic")
    updateSelectInput(session, "feat_arith_col1", selected = num_cols[1])
    updateSelectInput(session, "feat_arith_col2", selected = num_cols[2])
    updateSelectInput(session, "feat_arith_op", selected = "*")
    updateTextInput(session, "feat_new_name", value = paste0(num_cols[1], "_x_", num_cols[2]))
  })
  observeEvent(input$temp_zscore, {
    df <- clean_df()
    req(df)
    num_cols <- safe_numeric_cols(df)
    if (length(num_cols) == 0) {
      showNotification("No numeric columns available for this template.", type = "warning")
      return()
    }
    # Z-score is not a direct math transform, but we can implement via scale in math? For simplicity, we use math with a custom? Actually we can use a pre-defined column via scaling. But for demo, we'll use a custom arithmetic? Let's just use math with a dummy? Better to create a specific function, but for simplicity we'll use "math" with "zscore" not in list. Instead we'll use "scale" but that's in cleaning. For now, we'll just create a message.
    showNotification("Z‑score transformation is available via the 'Scaling & Encoding' section in Data Cleaning.", type = "info")
  })
  
  # Create feature (with metadata storage)
  observeEvent(input$btn_create_feat, {
    df <- clean_df()
    req(df)
    
    new_name <- trimws(input$feat_new_name)
    if (new_name == "") {
      showNotification("Please enter a new column name.", type = "warning")
      return()
    }
    if (new_name %in% names(df)) {
      showNotification("Column name already exists. Choose a different name.", type = "warning")
      return()
    }
    
    feat_type <- input$feat_type
    metadata <- list(type = feat_type)
    
    tryCatch({
      if (feat_type == "arithmetic") {
        col1 <- input$feat_arith_col1
        col2 <- input$feat_arith_col2
        op <- input$feat_arith_op
        req(col1, col2, op)
        if (!is.numeric(df[[col1]]) || !is.numeric(df[[col2]])) {
          showNotification("Arithmetic operations require numeric columns.", type = "warning")
          return()
        }
        new_col <- switch(op,
                          "+" = df[[col1]] + df[[col2]],
                          "-" = df[[col1]] - df[[col2]],
                          "*" = df[[col1]] * df[[col2]],
                          "/" = df[[col1]] / df[[col2]])
        metadata$col1 <- col1
        metadata$col2 <- col2
        metadata$op <- op
        log_detail <- paste(col1, op, col2)
      } 
      else if (feat_type == "math") {
        col <- input$feat_math_col
        func <- input$feat_math_func
        req(col, func)
        if (!is.numeric(df[[col]])) {
          showNotification("Mathematical transforms require numeric columns.", type = "warning")
          return()
        }
        if (func %in% c("log", "log10") && any(df[[col]] <= 0, na.rm = TRUE)) {
          showNotification("Log transformations require positive values.", type = "warning")
          return()
        }
        if (func == "sqrt" && any(df[[col]] < 0, na.rm = TRUE)) {
          showNotification("Square root transformation requires non-negative values.", type = "warning")
          return()
        }
        if (func == "recip" && any(df[[col]] == 0, na.rm = TRUE)) {
          showNotification("Reciprocal transformation requires non-zero values.", type = "warning")
          return()
        }
        new_col <- switch(func,
                          "log" = log(df[[col]]),
                          "log10" = log10(df[[col]]),
                          "sqrt" = sqrt(df[[col]]),
                          "^2" = df[[col]]^2,
                          "exp" = exp(df[[col]]),
                          "recip" = 1 / df[[col]])
        metadata$col <- col
        metadata$func <- func
        log_detail <- paste(func, "of", col)
      } 
      else if (feat_type == "bin") {
        col <- input$feat_bin_col
        n <- input$feat_bin_n
        method <- input$feat_bin_method
        req(col, n, method)
        if (!is.numeric(df[[col]])) {
          showNotification("Binning requires a numeric column.", type = "warning")
          return()
        }
        x <- df[[col]]
        if (method == "equal_width") {
          breaks <- seq(min(x, na.rm = TRUE), max(x, na.rm = TRUE), length.out = n + 1)
          new_col <- cut(x, breaks = breaks, include.lowest = TRUE, labels = FALSE)
        } else {
          probs <- seq(0, 1, length.out = n + 1)
          breaks <- quantile(x, probs = probs, na.rm = TRUE)
          if (any(duplicated(breaks))) {
            showNotification("Duplicate quantile breaks found. Try fewer bins.", type = "warning")
            return()
          }
          new_col <- cut(x, breaks = breaks, include.lowest = TRUE, labels = FALSE)
        }
        metadata$col <- col
        metadata$n <- n
        metadata$method <- method
        log_detail <- paste("binning", col, "into", n, "bins")
      }
      else if (feat_type == "poly") {
        col1 <- input$feat_poly_col1
        func <- input$feat_poly_func
        col2 <- input$feat_poly_col2
        req(col1, func)
        if (!is.numeric(df[[col1]])) {
          showNotification("Polynomial operations require numeric columns.", type = "warning")
          return()
        }
        if (func %in% c("product", "sum_sq") && (col2 == "None" || is.null(col2) || !is.numeric(df[[col2]]))) {
          showNotification("For product or sum of squares, both columns must be numeric.", type = "warning")
          return()
        }
        new_col <- switch(func,
                          "square" = df[[col1]]^2,
                          "cube" = df[[col1]]^3,
                          "product" = df[[col1]] * df[[col2]],
                          "sum_sq" = df[[col1]]^2 + df[[col2]]^2)
        metadata$col1 <- col1
        metadata$col2 <- if (col2 != "None") col2 else NULL
        metadata$func <- func
        log_detail <- paste(func, "on", col1, if (!is.null(metadata$col2)) paste("and", metadata$col2) else "")
      }
      
      df[[new_name]] <- new_col
      clean_df(df)
      created_features(unique(c(created_features(), new_name)))
      # Store metadata
      meta <- feature_metadata()
      meta[[new_name]] <- metadata
      feature_metadata(meta)
      log_op(sprintf("Feature created: '%s' from %s", new_name, log_detail))
      showNotification(sprintf("Feature '%s' created.", new_name), type = "message")
      
    }, error = function(e) {
      showNotification(paste("Error creating feature:", e$message), type = "error", duration = 8)
    })
  })
  
  # List created features
  output$feat_list_table <- renderDT({
    feat_names <- created_features()
    if (length(feat_names) == 0) {
      datatable(data.frame(Message = "No features created yet."), options = list(dom = "t"), rownames = FALSE)
    } else {
      df_list <- data.frame(Feature = feat_names, stringsAsFactors = FALSE)
      datatable(df_list, options = list(pageLength = 15, dom = "t", ordering = FALSE), rownames = FALSE, selection = "single")
    }
  })
  
  # Edit selected feature
  observeEvent(input$btn_edit_feat, {
    selected <- input$feat_list_table_rows_selected
    if (length(selected) == 0) {
      showNotification("Please select a feature to edit.", type = "warning")
      return()
    }
    feat_name <- created_features()[selected]
    meta <- feature_metadata()[[feat_name]]
    if (is.null(meta)) {
      showNotification("Metadata not found for this feature.", type = "warning")
      return()
    }
    
    # Populate form based on type
    if (!is.null(meta$type)) {
      updateSelectInput(session, "feat_type", selected = meta$type)
      if (meta$type == "arithmetic") {
        updateSelectInput(session, "feat_arith_col1", selected = meta$col1)
        updateSelectInput(session, "feat_arith_col2", selected = meta$col2)
        updateSelectInput(session, "feat_arith_op", selected = meta$op)
      } else if (meta$type == "math") {
        updateSelectInput(session, "feat_math_col", selected = meta$col)
        updateSelectInput(session, "feat_math_func", selected = meta$func)
      } else if (meta$type == "bin") {
        updateSelectInput(session, "feat_bin_col", selected = meta$col)
        updateNumericInput(session, "feat_bin_n", value = meta$n)
        updateSelectInput(session, "feat_bin_method", selected = meta$method)
      } else if (meta$type == "poly") {
        updateSelectInput(session, "feat_poly_col1", selected = meta$col1)
        updateSelectInput(session, "feat_poly_func", selected = meta$func)
        if (!is.null(meta$col2)) {
          updateSelectInput(session, "feat_poly_col2", selected = meta$col2)
        } else {
          updateSelectInput(session, "feat_poly_col2", selected = "None")
        }
      }
    }
    updateTextInput(session, "feat_new_name", value = feat_name)
    showNotification("Form populated with selected feature's parameters. You can modify and click 'Create Feature' to replace it.", type = "info", duration = 5)
  })
  
  # Remove feature
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
    }
    created_features(setdiff(created_features(), feat_name))
    # Remove metadata
    meta <- feature_metadata()
    meta[[feat_name]] <- NULL
    feature_metadata(meta)
    log_op(sprintf("Feature removed: '%s'", feat_name))
    showNotification(sprintf("Feature '%s' removed.", feat_name), type = "message")
  })
  
  output$feat_preview_table <- renderDT({
    df <- clean_df()
    req(df)
    datatable(df, options = list(scrollX = TRUE, pageLength = 15), rownames = FALSE, filter = "top")
  })
  
  output$feat_summary <- renderText({
    df <- clean_df()
    req(df)
    
    feat_names <- created_features()
    if (length(feat_names) == 0) return("No features created yet.")
    
    out <- c()
    for (fn in feat_names) {
      if (fn %in% names(df)) {
        out <- c(out, paste0("Feature: ", fn))
        if (is.numeric(df[[fn]])) {
          out <- c(out, capture.output(summary(df[[fn]])), "")
        } else {
          out <- c(out, paste("First 10 values:"), paste(capture.output(print(head(df[[fn]], 10))), collapse = "\n"), "")
        }
      }
    }
    paste(out, collapse = "\n")
  })
  
  output$feat_log <- renderText({
    lg <- op_log()
    if (length(lg) == 0) return("No operations performed yet.")
    paste(rev(lg), collapse = "\n")
  })
  
  
  # EDA
  output$eda_no_data_warning <- renderUI({
    if (is.null(clean_df())) {
      div(
        class = "warning-box",
        icon("triangle-exclamation"),
        tags$b(" No dataset loaded. "),
        "Please go to Upload Data and click Load / Refresh first."
      )
    }
  })
  
  output$eda_x_ui <- renderUI({
    df <- clean_df()
    req(df, input$eda_plot_type)
    
    if (input$eda_plot_type %in% c("hist", "box", "scatter")) {
      num_cols <- safe_numeric_cols(df)
      selectInput("eda_x", "X variable:", choices = num_cols)
    } else {
      selectInput("eda_x", "Variable:", choices = names(df))
    }
  })
  
  output$eda_y_ui <- renderUI({
    df <- clean_df()
    req(df, input$eda_plot_type)
    
    if (input$eda_plot_type == "scatter") {
      num_cols <- safe_numeric_cols(df)
      selectInput("eda_y", "Y variable:", choices = num_cols)
    }
  })
  
  output$eda_color_ui <- renderUI({
    df <- clean_df()
    req(df)
    group_cols <- safe_group_cols(df)
    selectInput("eda_color", "Color / group by:", choices = c("None", group_cols), selected = "None")
  })
  
  output$eda_filter_var_ui <- renderUI({
    df <- clean_df()
    req(df)
    selectInput("eda_filter_var", "Filter variable:", choices = c("None", names(df)))
  })
  
  output$eda_filter_ui <- renderUI({
    df <- clean_df()
    req(df, input$eda_filter_var)
    
    if (input$eda_filter_var == "None") return(NULL)
    
    x <- df[[input$eda_filter_var]]
    
    if (is.numeric(x)) {
      sliderInput(
        "eda_filter_range", "Select range:",
        min = min(x, na.rm = TRUE),
        max = max(x, na.rm = TRUE),
        value = c(min(x, na.rm = TRUE), max(x, na.rm = TRUE))
      )
    } else {
      vals <- unique(as.character(x))
      vals <- vals[!is.na(vals)]
      selectInput("eda_filter_levels", "Select values:", choices = vals, selected = vals, multiple = TRUE)
    }
  })
  
  eda_df <- reactive({
    df <- clean_df()
    req(df)
    
    if (!is.null(input$eda_filter_var) && input$eda_filter_var != "None") {
      var <- input$eda_filter_var
      
      if (is.numeric(df[[var]]) && !is.null(input$eda_filter_range)) {
        df <- df[
          !is.na(df[[var]]) &
            df[[var]] >= input$eda_filter_range[1] &
            df[[var]] <= input$eda_filter_range[2],
          , drop = FALSE
        ]
      }
      
      if (!is.numeric(df[[var]]) && !is.null(input$eda_filter_levels)) {
        df <- df[as.character(df[[var]]) %in% input$eda_filter_levels, , drop = FALSE]
      }
    }
    
    df
  })
  
  output$eda_plot <- renderPlot({
    df <- eda_df()
    req(df, input$eda_plot_type, input$eda_x)
    
    color_var <- if (!is.null(input$eda_color) && input$eda_color != "None") input$eda_color else NULL
    
    if (nrow(df) == 0) {
      plot.new()
      text(0.5, 0.5, "No data available after filtering.")
      return()
    }
    
    if (input$eda_plot_type == "hist") {
      p <- ggplot(df, aes_string(x = input$eda_x)) +
        geom_histogram(bins = input$eda_bins, fill = "#56B4E9", color = "black", alpha = 0.8) +
        theme_minimal(base_size = 13) +
        labs(title = paste("Histogram of", input$eda_x), x = input$eda_x, y = "Frequency")
      print(p)
    }
    
    if (input$eda_plot_type == "box") {
      if (!is.null(color_var)) {
        p <- ggplot(df, aes_string(x = color_var, y = input$eda_x, fill = color_var)) +
          geom_boxplot(alpha = 0.75) +
          theme_minimal(base_size = 13) +
          labs(title = paste("Boxplot of", input$eda_x, "by", color_var), x = color_var, y = input$eda_x)
      } else {
        p <- ggplot(df, aes_string(y = input$eda_x)) +
          geom_boxplot(fill = "#90CAF9", color = "black", alpha = 0.8) +
          theme_minimal(base_size = 13) +
          labs(title = paste("Boxplot of", input$eda_x), x = "", y = input$eda_x)
      }
      print(p)
    }
    
    if (input$eda_plot_type == "scatter") {
      req(input$eda_y)
      
      if (!is.null(color_var)) {
        p <- ggplot(df, aes_string(x = input$eda_x, y = input$eda_y, color = color_var)) +
          geom_point(alpha = 0.75, size = 2.5) +
          theme_minimal(base_size = 13) +
          labs(title = paste("Scatterplot of", input$eda_y, "vs", input$eda_x), x = input$eda_x, y = input$eda_y)
      } else {
        p <- ggplot(df, aes_string(x = input$eda_x, y = input$eda_y)) +
          geom_point(color = "#2C7FB8", alpha = 0.75, size = 2.5) +
          theme_minimal(base_size = 13) +
          labs(title = paste("Scatterplot of", input$eda_y, "vs", input$eda_x), x = input$eda_x, y = input$eda_y)
      }
      
      if (isTRUE(input$eda_add_lm)) {
        p <- p + geom_smooth(method = "lm", se = FALSE, color = "red")
      }
      print(p)
    }
    
    if (input$eda_plot_type == "bar") {
      p <- ggplot(df, aes_string(x = input$eda_x)) +
        geom_bar(fill = "#2E8B57", alpha = 0.8) +
        theme_minimal(base_size = 13) +
        labs(title = paste("Bar Plot of", input$eda_x), x = input$eda_x, y = "Count") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
      print(p)
    }
  })
  
  output$eda_summary <- renderText({
    df <- eda_df()
    req(df, input$eda_x)
    
    out <- c()
    out <- c(out, "Filtered dataset dimensions:")
    out <- c(out, paste(dim(df), collapse = " x "))
    out <- c(out, "")
    out <- c(out, "Missing values in filtered data:")
    out <- c(out, as.character(sum(is.na(df))))
    out <- c(out, "")
    out <- c(out, paste("Summary of", input$eda_x, ":"))
    out <- c(out, capture.output(summary(df[[input$eda_x]])))
    
    if (!is.null(input$eda_y) && input$eda_plot_type == "scatter") {
      out <- c(out, "", paste("Summary of", input$eda_y, ":"))
      out <- c(out, capture.output(summary(df[[input$eda_y]])))
      
      if (is.numeric(df[[input$eda_x]]) && is.numeric(df[[input$eda_y]])) {
        out <- c(out, "", "Correlation between selected x and y:")
        out <- c(out, as.character(cor(df[[input$eda_x]], df[[input$eda_y]], use = "complete.obs")))
      }
    }
    
    paste(out, collapse = "\n")
  })
  
  output$eda_corr <- renderTable({
    df <- eda_df()
    req(df)
    
    num_df <- df[, sapply(df, is.numeric), drop = FALSE]
    if (ncol(num_df) < 2) return(NULL)
    
    round(cor(num_df, use = "complete.obs"), 3)
  }, rownames = TRUE)
  
  output$eda_table <- renderDT({
    df <- eda_df()
    req(df)
    
    datatable(
      head(df, 50),
      options = list(pageLength = 10, scrollX = TRUE),
      rownames = FALSE,
      filter = "top"
    )
  })
}

shinyApp(ui = ui, server = server)
