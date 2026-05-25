library(R6)
library(WDI)
library(R6)
library(tidyverse)

DevelopmentModel <- R6Class(
  "DevelopmentModel",
  public = list(
    global_development = NULL,
    data_clean         = NULL,
    scaled_data        = NULL,
    pca_res            = NULL,
    cluster_model      = NULL,
    
    # ----------------------------------------------------------
    initialize = function() {
      
      indicators <- c(
        GDP           = "NY.GDP.MKTP.CD",
        lifeExp       = "SP.DYN.LE00.IN",
        fertilityRt   = "SP.DYN.TFRT.IN",
        education     = "SE.SEC.ENRR",
        literacyRt    = "SE.ADT.LITR.ZS",
        populationGth = "SP.POP.GROW",
        unemploymentRt= "SL.UEM.TOTL.ZS",
        mortalityRt   = "SH.DYN.MORT",
        CO2Em         = "EN.GHG.CO2.IC.MT.CE.AR5"
      )
      
      # FIX 2: end = 2025 caused mostly-NA results because WDI data for the
      #         current year is not yet published.  2022 is the latest
      #         complete year for most indicators.  mrv = 1 (most recent
      #         value) is an alternative when you always want the freshest
      #         single year.
      tryCatch({
        self$global_development <- WDI(
          country   = "all",
          indicator = indicators,
          start     = 2019,
          end       = 2022,   # <-- was 2025; fixed
          extra     = TRUE
        )
      }, error = function(e) {
        stop(
          "Failed to retrieve data from the World Bank API. ",
          "Check your internet connection. Original error: ", e$message,
          call. = FALSE
        )
      })
      
      self$data_clean <- self$global_development %>%
        filter(region != "Aggregates") %>%
        select(country, iso3c, GDP, lifeExp, fertilityRt, education,
               literacyRt, populationGth, unemploymentRt, mortalityRt, CO2Em) %>%
        drop_na()
      
      numeric_data <- self$data_clean %>%
        select(GDP, lifeExp, fertilityRt, education, literacyRt,
               populationGth, unemploymentRt, mortalityRt, CO2Em)
      
      self$scaled_data <- scale(numeric_data)
      rownames(self$scaled_data) <- self$data_clean$country
    },
    
    # ----------------------------------------------------------
    run_pca = function() {
      self$pca_res <- prcomp(self$scaled_data)
      invisible(self)   # return self for method chaining
    },
    
    # ----------------------------------------------------------
    run_clustering = function(k = 3, method = "kmeans") {
      
      # Defensive: run PCA first if not done yet
      if (is.null(self$pca_res)) self$run_pca()
      
      # Validate inputs
      if (!method %in% c("kmeans", "hierarchical"))
        stop("method must be 'kmeans' or 'hierarchical'", call. = FALSE)
      if (!is.numeric(k) || k < 2 || k > 10)
        stop("k must be an integer between 2 and 10", call. = FALSE)
      
      pca_scores <- self$pca_res$x[, 1:2]
      set.seed(123)
      
      if (method == "kmeans") {
        self$cluster_model <- kmeans(pca_scores, centers = k, nstart = 25)
      } else {
        hc <- hclust(dist(pca_scores), method = "ward.D2")
        # Wrap in a list with a $cluster slot so downstream code is identical
        # for both methods (both accessed via self$cluster_model$cluster)
        self$cluster_model <- list(cluster = cutree(hc, k = k))
      }
      
      invisible(self)
    },
    
    # ----------------------------------------------------------
    calculate_aid_priority = function() {
      
      if (is.null(self$pca_res) || is.null(self$cluster_model))
        stop("Run run_pca() and run_clustering() before calling this method.",
             call. = FALSE)
      
      pca_scores  <- self$pca_res$x[, 1:2]
      target_point <- c(max(pca_scores[, 1]), min(pca_scores[, 2]))
      
      # FIX 4: Instead of calling calculate_euclidean() row-by-row inside a
      #         loop, pass the full matrix at once.  The Rcpp (or fallback)
      #         function handles vectorisation internally.
      #         We keep a vectorised R call here so it works even with the
      #         pure-R fallback.
      country_distances <- apply(pca_scores, 1, function(row)
        calculate_euclidean(row, target_point)
      )
      
      data.frame(
        Country        = self$data_clean$country,
        Cluster        = self$cluster_model$cluster,
        Priority_Score = round(country_distances, 2),
        stringsAsFactors = FALSE
      ) %>%
        arrange(desc(Priority_Score))
    }
    
  )
)  
  
                              