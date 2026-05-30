library(R6)
library(WDI)
library(tidyverse)

DevelopmentModel <- R6Class(
  "DevelopmentModel",
  
  public = list(
    
    global_development = NULL,
    data_clean = NULL,
    scaled_data = NULL,
    pca_res = NULL,
    cluster_model = NULL,
    
    # ------------------------------------------------------------
    # INITIALIZE
    # ------------------------------------------------------------
    
    initialize = function() {
      
      indicators <- c(
        GDP = "NY.GDP.MKTP.CD",
        lifeExp = "SP.DYN.LE00.IN",
        fertilityRt = "SP.DYN.TFRT.IN",
        education = "SE.SEC.ENRR",
        populationGth = "SP.POP.GROW",
        unemploymentRt = "SL.UEM.TOTL.ZS",
        mortalityRt = "SH.DYN.MORT",
        CO2Em = "EN.GHG.CO2.IC.MT.CE.AR5"
      )
      
      self$global_development <- WDI(
        country = "all",
        indicator = indicators,
        start = 2019,
        end = 2019,
        extra = TRUE
      )
      
      self$data_clean <- self$global_development %>%
        filter(region != "Aggregates") %>%
        distinct(iso3c, .keep_all = TRUE) %>%
        select(
          country,
          iso3c,
          GDP,
          lifeExp,
          fertilityRt,
          education,
          populationGth,
          unemploymentRt,
          mortalityRt,
          CO2Em
        ) %>%
        drop_na()
      
      numeric_data <- self$data_clean %>%
        select(
          GDP,
          lifeExp,
          fertilityRt,
          education,
          populationGth,
          unemploymentRt,
          mortalityRt,
          CO2Em
        )
      
      self$scaled_data <- scale(numeric_data)
      rownames(self$scaled_data) <- self$data_clean$country
      
      cat("Total rows downloaded:", nrow(self$global_development), "\n")
      cat("Rows after cleaning:", nrow(self$data_clean), "\n")
    },
    
    # ------------------------------------------------------------
    # PCA
    # ------------------------------------------------------------
    
    run_pca = function() {
      self$pca_res <- prcomp(self$scaled_data)
      invisible(self)
    },
    
    # ------------------------------------------------------------
    # CLUSTERING
    # ------------------------------------------------------------
    
    run_clustering = function(k = 3, method = "kmeans") {
      
      if (is.null(self$pca_res)) {
        self$run_pca()
      }
      
      if (!method %in% c("kmeans", "hierarchical")) {
        stop("method must be 'kmeans' or 'hierarchical'", call. = FALSE)
      }
      
      if (!is.numeric(k) || k < 2 || k > 10) {
        stop("k must be a number between 2 and 10", call. = FALSE)
      }
      
      pca_scores <- self$pca_res$x[, 1:2]
      
      set.seed(123)
      
      if (method == "kmeans") {
        
        self$cluster_model <- kmeans(
          pca_scores,
          centers = k,
          nstart = 25
        )
        
      } else {
        
        hc <- hclust(
          dist(pca_scores),
          method = "ward.D2"
        )
        
        self$cluster_model <- list(
          cluster = cutree(hc, k = k)
        )
      }
      
      invisible(self)
    },
    
    # ------------------------------------------------------------
    # AID PRIORITY SCORING USING RCPP
    # ------------------------------------------------------------
    
    calculate_aid_priority = function() {
      
      if (is.null(self$pca_res)) {
        self$run_pca()
      }
      
      if (is.null(self$cluster_model)) {
        self$run_clustering(k = 3)
      }
      
      pca_scores <- self$pca_res$x[, 1:2]
      
      priority_scores <- calculate_priority_scores_cpp(
        as.matrix(pca_scores)
      )
      
      data.frame(
        Country = self$data_clean$country,
        Cluster = self$cluster_model$cluster,
        Priority_Score = round(priority_scores, 2),
        stringsAsFactors = FALSE
      ) %>%
        arrange(desc(Priority_Score))
    }
  )
)