required_pkgs <- c(
  "shiny", "tidyverse", "Rcpp", "R6",
  "factoextra", "corrplot", "bslib", "WDI",
  "cluster", "DT"
)

new_pkgs <- required_pkgs[!sapply(required_pkgs, requireNamespace, quietly = TRUE)]
if (length(new_pkgs)) install.packages(new_pkgs)

library(shiny)
library(tidyverse)
library(Rcpp)
library(R6)
library(factoextra)
library(corrplot)
library(bslib)
library(WDI)
library(cluster)
library(DT)


source("R/DevelopmentModel.R")
Rcpp::sourceCpp("src/distance.cpp")

ui <- fluidPage(
  theme = bslib::bs_theme(version = 5, bootswatch = "lux"),
  
  titlePanel("Global Development Decision Support System"),
  p("PCA, clustering, development scoring, aid allocation ranking, and country eligibility analysis"),
  hr(),
  
  fluidRow(
    column(
      4,
      sliderInput(
        "k_clusters",
        "Number of Clusters:",
        min = 2,
        max = 6,
        value = 3
      )
    ),
    column(
      4,
      selectInput(
        "method",
        "Clustering Method:",
        choices = c(
          "K-Means" = "kmeans",
          "Hierarchical" = "hierarchical"
        )
      )
    ),
    column(
      4,
      textInput(
        "country_name",
        "Check Country Eligibility:",
        placeholder = "Example: Afghanistan"
      ),
      actionButton(
        "check_country",
        "Check Eligibility",
        class = "btn-primary"
      )
    )
  ),
  
  hr(),
  
  tabsetPanel(
    tabPanel("Dataset Preview", DTOutput("data_preview")),
    tabPanel("Correlation Analysis", plotOutput("corr_plot")),
    tabPanel("Scree / Evaluation", plotOutput("scree_plot")),
    #tabPanel("PCA Biplot", plotOutput("pca_biplot")),
    tabPanel("Cluster Biplot Space", plotOutput("cluster_plot")),
    tabPanel("Silhouette Evaluation", plotOutput("silhouette_plot")),
    tabPanel("Cluster Profiles", DTOutput("cluster_profiles")),
    tabPanel("Aid Allocation Priority", DTOutput("rankings_table")),
    tabPanel("Country Eligibility", DTOutput("country_check"))
  )
)

server <- function(input, output, session) {
  
  model <- DevelopmentModel$new()
  
  run_pipeline <- reactive({
    model$run_pca()
    model$run_clustering(
      k = input$k_clusters,
      method = input$method
    )
    model
  })
  
  add_eligibility <- function(rankings) {
    rankings %>%
      mutate(
        Eligibility = case_when(
          Priority_Score >= quantile(Priority_Score, 0.75, na.rm = TRUE) ~ "Eligible - High aid priority",
          Priority_Score >= quantile(Priority_Score, 0.50, na.rm = TRUE) ~ "Potentially eligible - Medium aid priority",
          TRUE ~ "Not priority eligible"
        ),
        Recommendation = case_when(
          Eligibility == "Eligible - High aid priority" ~ "Recommended for aid allocation",
          Eligibility == "Potentially eligible - Medium aid priority" ~ "Consider after high-priority countries",
          TRUE ~ "Not recommended as immediate aid priority"
        )
      )
  }
  
  output$data_preview <- renderDT({
    datatable(
      model$data_clean %>%
        select(
          country, GDP, lifeExp, fertilityRt, education, literacyRt,
          populationGth, unemploymentRt, mortalityRt, CO2Em
        ),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        searching = TRUE
      ),
      rownames = FALSE
    )
  })
  
  output$corr_plot <- renderPlot({
    correlation <- cor(model$scaled_data, method = "pearson")
    
    corrplot(
      correlation,
      method = "color",
      type = "upper",
      tl.col = "black",
      tl.srt = 45
    )
  })
  
  output$scree_plot <- renderPlot({
    m <- run_pipeline()
    
    fviz_eig(
      m$pca_res,
      addlabels = TRUE
    ) +
      theme_minimal() +
      labs(title = "PCA Scree Plot")
  })
  
  output$pca_biplot <- renderPlot({
    m <- run_pipeline()
    
    fviz_pca_biplot(
      m$pca_res,
      repel = TRUE,
      col.var = "steelblue",
      col.ind = "gray"
    ) +
      theme_minimal() +
      labs(title = "PCA Biplot")
  })
  
  output$cluster_plot <- renderPlot({
    m <- run_pipeline()
    
    pca_scores <- as.data.frame(m$pca_res$x[, 1:2])
    colnames(pca_scores) <- c("PC1", "PC2")
    pca_scores$Cluster <- as.factor(m$cluster_model$cluster)
    pca_scores$Country <- m$data_clean$country
    
    ggplot(pca_scores, aes(x = PC1, y = PC2, color = Cluster)) +
      geom_point(size = 3, alpha = 0.8) +
      theme_minimal() +
      labs(
        title = paste("Global Country Clusters via", toupper(input$method)),
        x = "Principal Component 1",
        y = "Principal Component 2",
        color = "Cluster"
      )
  })
  
  output$silhouette_plot <- renderPlot({
    m <- run_pipeline()
    
    pca_scores <- m$pca_res$x[, 1:2]
    clusters <- m$cluster_model$cluster
    
    sil <- silhouette(clusters, dist(pca_scores))
    
    fviz_silhouette(sil) +
      theme_minimal() +
      labs(title = "Silhouette Evaluation of Clusters")
  })
  
  output$cluster_profiles <- renderDT({
    m <- run_pipeline()
    
    profiles <- m$data_clean %>%
      mutate(Cluster = m$cluster_model$cluster) %>%
      group_by(Cluster) %>%
      summarise(
        Countries = n(),
        Avg_GDP = round(mean(GDP, na.rm = TRUE), 2),
        Avg_Life_Expectancy = round(mean(lifeExp, na.rm = TRUE), 2),
        Avg_Fertility = round(mean(fertilityRt, na.rm = TRUE), 2),
        Avg_Education = round(mean(education, na.rm = TRUE), 2),
        Avg_Literacy = round(mean(literacyRt, na.rm = TRUE), 2),
        Avg_Population_Growth = round(mean(populationGth, na.rm = TRUE), 2),
        Avg_Unemployment = round(mean(unemploymentRt, na.rm = TRUE), 2),
        Avg_Mortality = round(mean(mortalityRt, na.rm = TRUE), 2),
        Avg_CO2 = round(mean(CO2Em, na.rm = TRUE), 2),
        .groups = "drop"
      )
    
    datatable(
      profiles,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        searching = FALSE
      ),
      rownames = FALSE
    )
  })
  
  output$rankings_table <- renderDT({
    m <- run_pipeline()
    
    rankings <- m$calculate_aid_priority() %>%
      add_eligibility()
    
    datatable(
      rankings,
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        searching = TRUE
      ),
      rownames = FALSE
    )
  })
  
  country_result <- eventReactive(input$check_country, {
    req(input$country_name)
    
    m <- run_pipeline()
    
    rankings <- m$calculate_aid_priority() %>%
      add_eligibility()
    
    result <- rankings %>%
      filter(str_detect(
        str_to_lower(Country),
        str_to_lower(input$country_name)
      ))
    
    validate(
      need(nrow(result) > 0, "Country not found. Please check the spelling.")
    )
    
    result
  })
  
  output$country_check <- renderDT({
    datatable(
      country_result(),
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        searching = FALSE
      ),
      rownames = FALSE
    )
  })
}

shinyApp(ui, server)