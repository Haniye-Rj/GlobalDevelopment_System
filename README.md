# Global Development Decision Support System

An R-based analytical and decision-support system for global development analysis and aid allocation using unsupervised machine learning techniques.

## Overview

This project was developed as part of the **Advanced Programming in R** course. The system combines data science, machine learning, object-oriented programming, and interactive visualization to analyze global development indicators and support aid allocation decisions.

The application uses World Bank World Development Indicators (WDI) data to:

* analyze multidimensional development patterns
* reduce data complexity using Principal Component Analysis (PCA)
* cluster countries based on development similarities
* rank countries using a development priority scoring system
* provide interactive exploration through a Shiny dashboard

The project transforms a traditional static analysis into a reusable analytical framework and decision-support tool.

---

## Features

### Data Analysis

* Data preprocessing and cleaning
* Standardization of development indicators
* Principal Component Analysis (PCA)
* Correlation analysis

### Machine Learning

* K-means clustering
* Hierarchical clustering
* Cluster evaluation methods
* Development pattern identification

### Decision Support

* Country ranking by development priority
* Aid allocation recommendation system
* Priority scoring using PCA-based distance analysis

### Interactive Dashboard

* Interactive Shiny application
* PCA visualization
* Cluster visualization
* Country-level exploration
* Ranking and recommendation tables

### Advanced Programming Features

* Object-Oriented Programming using R6
* Rcpp integration with C++ optimization
* Modular analytical pipeline
* Package-like project structure

---

## Technologies Used

* R
* Shiny
* R6
* Rcpp
* tidyverse
* WDI
* factoextra
* cluster
* ggplot2
* corrplot

---

## Project Structure

```text
GlobalDevelopment_System/
│
├── app.R                     # Main Shiny application
├── R/
│   └── DevelopmentModel.R   # R6 analytical engine
├── src/
│   └── distance.cpp         # Rcpp distance calculation
├── inst/
│   └── PCA_Clustering.R     # Exploratory prototype analysis
├── rsconnect/               # Shiny deployment configuration
└── README.md
```

---

## Methodology

### 1. Data Collection

The system retrieves global development indicators using the World Bank WDI API.

### 2. Data Preprocessing

* Removal of aggregate regions
* Handling missing values
* Standardization using `scale()`

### 3. Dimensionality Reduction

PCA is applied to reduce high-dimensional indicator space into principal components representing development patterns.

### 4. Clustering

Countries are grouped using:

* K-means clustering
* Hierarchical clustering

### 5. Aid Priority Scoring

Countries are ranked using a PCA-based Euclidean distance scoring system to identify higher-priority development needs.

---

## Indicators Used

* GDP
* Life Expectancy
* Fertility Rate
* Secondary Education Enrollment
* Population Growth
* Unemployment Rate
* Mortality Rate
* CO2 Emissions

---

## Running the Project

### Install Dependencies

```r
install.packages(c(
  "shiny",
  "tidyverse",
  "WDI",
  "R6",
  "factoextra",
  "cluster",
  "corrplot",
  "DT",
  "Rcpp"
))
```

### Run the Application

```r
shiny::runApp()
```

## Learning Outcomes

This project demonstrates:

* unsupervised machine learning
* advanced R programming
* object-oriented programming with R6
* C++ integration using Rcpp
* interactive dashboard development
* reusable analytical system design

## Live Demo

Deployed on shinyapps.io:

```text
https://haniyeraji.shinyapps.io/pca_project_-_copy/
```
