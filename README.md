# DataExplorer Pro

An interactive R Shiny web application for end-to-end data exploration — from uploading raw datasets to cleaning, feature engineering, and visual analysis.

**Live App:** https://yz5049.shinyapps.io/5243_project2-main/

**GitHub Repository:** https://github.com/zyjbest/5243_project2

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

DataExplorer Pro is a versatile, user-friendly web application built with R Shiny. It supports the full data science workflow: uploading datasets in multiple formats, cleaning and preprocessing data interactively, engineering new features, and performing exploratory data analysis — all without writing any code. The app is designed to be intuitive, responsive, and suitable for users at any level of technical experience.

---

## Features

### 1. Upload Data
- Supports **CSV, TXT, Excel (.xlsx / .xls), JSON, and RDS** file formats
- Two built-in demo datasets available: **iris** and **mtcars**
- Configurable CSV options: delimiter selection and header toggle
- Adjustable row preview count
- Dataset info panel showing rows, columns, missing cells, and duplicate rows
- Four preview tabs: Table, Column Types, Missing Values, Dataset Info

### 2. Data Cleaning & Preprocessing
- **Duplicate removal** — detect and remove identical rows with live count feedback
- **Missing value treatment** — six strategies:
  - Remove rows with NA
  - Impute with mean / median (numeric columns)
  - Impute with mode (any column type)
  - Impute with a custom fixed value
  - Remove selected columns
- **Column type conversion** — convert any column to numeric, character, factor, logical, or date
- **Outlier handling (IQR method)** — detect outliers using the 1.5 × IQR rule, with options to remove rows or winsorise (cap) values
- **Scaling & encoding:**
  - Min-Max normalisation [0, 1]
  - Z-score standardisation
  - One-hot encoding for categorical columns
- **Operation log** — timestamped record of every cleaning action
- **Reset** — revert to original uploaded data at any time
- **Download** — export the cleaned dataset as a CSV file

### 3. Feature Engineering
- **Quick templates** — one-click buttons to populate the form: Square, Square Root, Log, Ratio (A/B), Product (A×B), Z-Score
- **15 example features** are automatically generated on data load (squares, logs, ratios, bins, z-scores, etc.)
- **Four feature types:**
  - Arithmetic (+, −, ×, ÷) between two numeric columns
  - Mathematical transforms: log, log10, sqrt, square, exponential, reciprocal
  - Binning: equal-width or equal-frequency (quantile) with configurable number of bins
  - Polynomial / interaction: square, cube, product, sum of squares
- **Real-time preview** — histogram or bar chart of the new feature before committing
- **Preview statistics** — summary stats shown before creating the feature
- **Edit & remove** created features from the feature list table
- **Operation log** — tracks all feature creation and removal actions

### 4. Exploratory Data Analysis (EDA)
- **Four chart types:** Histogram, Boxplot, Scatterplot, Bar Plot
- **Color / group by** any categorical column
- **Linear regression overlay** on scatterplots
- **Bin count slider** for histograms
- **Dynamic filtering** — filter by any variable (numeric range slider or categorical multi-select)
- **Summary statistics** panel — dataset dimensions, missing count, and per-variable summary
- **Correlation matrix** — computed across all numeric columns with complete-case handling
- **Filtered data preview** table — shows up to 50 rows matching current filters

### 5. User Interface & User Experience
- Clean, polished dashboard layout using `shinydashboard`
- Home page with feature overview cards, step-by-step guide, and live dataset value boxes
- Colour-coded status boxes and real-time value box counters across all tabs
- Collapsible control panels to reduce visual clutter
- Warning banners when no dataset is loaded
- Consistent responsive layout across all tabs

---

## Requirements

Install the following R packages before running the app:

```r
install.packages(c(
  "shiny",
  "shinydashboard",
  "DT",
  "readr",
  "readxl",
  "jsonlite",
  "ggplot2",
  "scales"
))
```

- **R version:** 4.0.0 or higher recommended

---

## How to Run Locally

1. Clone this repository:

```bash
git clone https://github.com/zyjbest/5243_project2.git
cd 5243_project2
```

2. Open R or RStudio and run:

```r
shiny::runApp("project2app.R")
```

Or open `project2app.R` in RStudio and click the **Run App** button in the top-right corner.

---

## How to Use the App

### Step 1 — Upload Data
1. Navigate to the **Upload Data** tab in the left sidebar.
2. Choose a data source:
   - Select **Built-in: iris** or **Built-in: mtcars** to load a demo dataset instantly, or
   - Select **Upload my own file** and click **Browse...** to upload your own file.
3. For CSV/TXT files, configure the delimiter and header options as needed.
4. Click **Load / Refresh** to load the data.
5. Use the preview tabs on the right to inspect the table, column types, and missing value summary.

### Step 2 — Clean Data
1. Navigate to the **Data Cleaning** tab.
2. The value boxes at the top show original rows, current rows, missing cells, and duplicate rows — updated in real time after each operation.
3. Work through the panels on the left in any order:
   - **Panel 1** — Remove duplicate rows
   - **Panel 2** — Handle missing values (select columns and choose a strategy)
   - **Panel 3** — Convert column data types
   - **Panel 4** — Handle outliers using the IQR method
   - **Panel 5** — Scale numeric columns or one-hot encode categorical columns
4. Monitor changes in the **Cleaned Data Preview** panel on the right.
5. Check the **Operation Log** tab to review all changes.
6. Use **Reset to Original Data** to undo all cleaning steps.
7. Click **Download Cleaned Data (.csv)** to save your result.

### Step 3 — Engineer Features
1. Navigate to the **Feature Engineering** tab.
2. Use the **Quick Templates** buttons for common transformations, or manually configure a new feature:
   - Select a feature type (arithmetic, math transform, binning, or polynomial)
   - Configure the relevant columns and parameters
   - Enter a name for the new column
3. Check the **preview plot** and **preview statistics** before committing.
4. Click **Create Feature** to add the column to the dataset.
5. Use the **Created Features** table to select, edit, or remove existing features.

### Step 4 — Explore Data (EDA)
1. Navigate to the **EDA** tab.
2. Select a plot type from the dropdown (Histogram, Boxplot, Scatterplot, Bar Plot).
3. Choose the X and Y variables and optionally a grouping/color variable.
4. Use the **Filter Data** section to narrow down the dataset by any variable.
5. View the **Summary Statistics** panel for per-variable summaries and correlation values.
6. Scroll down to see the **Correlation Matrix** and the **Filtered Data Preview** table.

---

## File Structure

```
.
├── project2app.R    # Main Shiny application (UI + Server, all modules)
└── README.md        # Project documentation
```

---

## Team Contributions

| Name | UNI | Contribution |
|------|-----|--------------|
| Yijing Zhang | yz5049 | Data Upload module, Data Cleaning & Preprocessing module |
| Yuxuan Ji | yj2924 | Feature Engineering module |
| Jana Choe | jc6198 | Exploratory Data Analysis (EDA) module |
| Jisheng Zeng | jz3993 | UI/UX design, overall app integration and deployment |

---

*STAT 5243 — Applied Data Science*  
*Project 2: Web Application Development and Deployment*
