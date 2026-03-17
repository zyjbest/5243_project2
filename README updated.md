# 5243_project2

An interactive R Shiny web application for data upload, cleaning, preprocessing, feature engineering, and exploratory data analysis.

**Live App:** `

---

## Table of Contents

- [Overview](#overview)
- [Features](#features)
- [Requirements](#requirements)
- [How to Run Locally](#how-to-run-locally)
- [How to Use the App](#how-to-use-the-app)
- [File Structure](#file-structure)
- [Team Contributions](#team-contributions)

---

## Overview

DataExplorer Pro is a versatile, user-friendly web application built with R Shiny. It allows users to upload their own datasets or explore built-in examples, then interactively clean, transform, and analyze the data — all without writing a single line of code.

---

## Features

### 1. Data Upload
- Supports **CSV, Excel (.xlsx / .xls), JSON, and RDS** file formats
- Two built-in demo datasets: **iris** and **mtcars**
- CSV options: configurable delimiter and header detection
- Adjustable row preview count
- Dataset info panel showing row count, column count, missing cells, and duplicate rows

### 2. Data Cleaning
- **Duplicate removal** — detects and removes identical rows with live count feedback
- **Missing value treatment** — six strategies:
  - Remove rows with NA
  - Impute with mean / median (numeric columns)
  - Impute with mode (any column type)
  - Impute with a custom fixed value
  - Remove selected columns entirely
- **Column type conversion** — convert any column to numeric, character, factor, logical, or date
- **Outlier handling (IQR method)** — detect outliers using the 1.5 × IQR rule, then choose to remove rows or winsorise (cap) values
- **Scaling & encoding**:
  - Min-Max normalisation [0, 1]
  - Z-score standardisation
  - One-hot encoding for categorical columns
- **Operation log** — timestamped history of every cleaning step applied
- **Reset** — revert to the original uploaded data at any time
- **Download** — export the cleaned dataset as a CSV file

### 3. Feature Engineering

**Design Goal**  
The feature engineering module empowers users to create new, meaningful features from existing columns through an intuitive, code‑free interface. It is designed to provide instant feedback and maintain a clear record of all transformations, enabling users to iteratively refine their dataset for better analysis or modeling.

**Supported Operations**  
- **Arithmetic** – combine two numeric columns using basic operators: addition (`+`), subtraction (`-`), multiplication (`*`), and division (`/`). For example, one could create a new column `Sepal.Length + Sepal.Width` from the iris dataset.  
- **Mathematical transforms** – apply common mathematical functions to a single numeric column: natural logarithm (`log`), base‑10 logarithm (`log10`), square root (`sqrt`), square (`^2`), and exponential (`exp`). These transforms are useful for normalizing data, reducing skewness, or meeting model assumptions.  
- **Binning** – discretize a numeric column into a specified number of bins, using either **equal‑width** (fixed intervals) or **equal‑frequency** (quantile‑based) strategies. Binning converts continuous variables into categorical ones, which can be helpful for creating ordinal features or simplifying patterns.

**Implementation Logic**  
- The module reuses the same reactive data source (`clean_df()`) as the Upload and Cleaning tabs, ensuring seamless integration and a consistent user experience across the application.  
- Every created feature is recorded in a separate reactive value (`created_features`) and listed in an interactive table under “Existing Features”. Users can select any feature from this list and remove it with a single click, allowing them to experiment without cluttering the dataset.  
- All operations are logged with timestamps, providing full traceability of the feature engineering process. This log is displayed in the “Operation Log” tab, helping users understand the sequence of transformations applied.  
- A “Reset to Original Data” button (shared with the Cleaning tab) clears all created features and reverts the dataset to its initial state, giving users a clean slate to start over if needed.

---

## Requirements

Install the following R packages before running:

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "DT",
  "readr",
  "readxl",
  "jsonlite"
))
```

- R version: **4.0.0 or higher** recommended

---

## How to Run Locally

1. Clone or download this repository:

```bash
git clone https://github.com/YOUR_USERNAME/YOUR_REPO_NAME.git
cd YOUR_REPO_NAME
```

2. Open R or RStudio and run:

```r
shiny::runApp("app.R")
```

Or open `app.R` in RStudio and click the **Run App** button.

---

## How to Use the App

### Step 1 — Upload Data
1. Navigate to the **Upload Data** tab in the sidebar.
2. Choose a data source:
   - Select **"Built-in: iris"** or **"Built-in: mtcars"** to use a demo dataset, or
   - Select **"Upload my own file"** and click **Browse...** to upload a file.
3. For CSV files, set the delimiter and header options if needed.
4. Click **Load / Refresh** to load the data.
5. The preview panel on the right will display the data table, column types, and missing value summary.

### Step 2 — Clean Data
1. Navigate to the **Data Cleaning** tab.
2. The stats bar at the top shows original rows, current rows, missing cells, and duplicate rows — updated in real time after each operation.
3. Use the panels on the left to apply cleaning steps in any order:
   - **Panel 1** — Remove duplicate rows
   - **Panel 2** — Handle missing values (select columns and choose a strategy)
   - **Panel 3** — Convert column data types
   - **Panel 4** — Handle outliers using the IQR method
   - **Panel 5** — Scale numeric columns or one-hot encode categorical columns
4. The cleaned data preview on the right updates automatically after each operation.
5. Check the **Operation Log** tab to review all changes made.
6. Click **Reset to Original Data** to undo all changes.
7. Click **Download Cleaned Data (.csv)** to save the result.

### Step 3 — Feature Engineering
1. Navigate to the Feature Engineering tab in the sidebar.
2. In the left panel, choose the type of feature you want to create from the dropdown:
   - Arithmetic: select two numeric columns and an operator (+, -, *, /).
   - Math Transform: select one numeric column and a function (log, log10, sqrt, square, exp).
   - Binning: select a numeric column, set the number of bins (between 2 and 100), and choose the binning method (equal‑width or equal‑frequency).
   - Enter a descriptive name for the new column in the “New column name” field.
3. Click the Create Feature button. The preview table on the right updates instantly, showing the new column appended to the dataset.
4. To understand the impact of the new feature, switch to the New Feature Summary tab:
   - For numeric features, you will see summary statistics (min, max, mean, quartiles).
   - For non‑numeric features (e.g., binned categories), the first 10 values are displayed.
   - The Existing Features table lists all columns that have been created via this module. You can select any row and click Remove Selected Feature to delete that column from the dataset.
   - The Operation Log tab records every feature creation and removal, allowing you to review the entire feature engineering history.
5. If you wish to discard all changes and start over, click Reset to Original Data (this button is also available in the Cleaning tab).
6. Tips for Effective Feature Engineering
   - When creating arithmetic features, ensure the columns are numeric and the operation makes sense contextually (e.g., avoid division by zero).
   - Mathematical transforms are particularly useful for variables with skewed distributions; for example, log transformation can help linearize relationships.
   - Binning can simplify complex relationships, but choosing too few bins may lose information, while too many bins may overfit. The equal‑frequency method often yields balanced categories when the variable is not uniformly distributed.
   - Use the Existing Features list to keep track of which columns were created; remove any that prove unnecessary to avoid clutter and potential confusion in downstream analysis.

---

## File Structure

```
.
├── app.R          # Main Shiny application (UI + Server)
└── README.md      # Project documentation
```

---

## Team Contributions

| Name | UNI | Contribution |
|Yuxuan Ji|yj2924|feature engineering|


---

*Project 2 — Web Application Development and Deployment*
*Submitted: March 20, 2025*
