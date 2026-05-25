# GlobalDevelopment_System

# PCA Shiny Dashboard

An interactive dashboard built with R Shiny for performing Principal Component Analysis (PCA) and visualizing multivariate datasets.

## Features

* Interactive visualizations
* Scree plot for explained variance
* Data preprocessing support
* Clean and responsive Shiny UI
* searching by the country aligibility

## Technologies Used

* R
* Shiny
* tidyverse
* ggplot2
* Rcpp

## Project Structure

```text
├── app.R
├── data/
├── www/
├── README.md
└── rsconnect/
```

## Run Locally

Clone the repository:

```bash
git clone https://github.com/YOUR_USERNAME/YOUR_REPOSITORY.git
```

Open the project in RStudio and run:

```r
shiny::runApp()
```

## Live Demo

Deployed on shinyapps.io:

```text
https://haniyeraji.shinyapps.io/pca_project_-_copy/
```

## Installation

Install required packages:

```r
install.packages(c(
  "shiny",
  "tidyverse",
  "ggplot2",
  "FactoMineR",
  "factoextra"
))
```

## About PCA

Principal Component Analysis (PCA) is a dimensionality reduction technique used to simplify complex datasets while preserving as much variance as possible.

This application helps users:

* Explore high-dimensional data
* Identify patterns and clusters
* Visualize variance contribution
* Understand feature relationships

