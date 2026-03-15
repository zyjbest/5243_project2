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
|------|-----|--------------|


---

*Project 2 — Web Application Development and Deployment*
*Submitted: March 20, 2025*
