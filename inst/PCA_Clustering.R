library(factoextra)
library(cluster)
library(WDI)
library(tidyverse)
library(dplyr)
library("ggplot2")
library("reshape2")
library("corrplot")


indicators <- c(
  GDP            = "NY.GDP.MKTP.CD",
  lifeExp        = "SP.DYN.LE00.IN",
  fertilityRt    = "SP.DYN.TFRT.IN",
  education      = "SE.SEC.ENRR",
  literacyRt     = "SE.ADT.LITR.ZS",
  populationGth  = "SP.POP.GROW",
  unemploymentRt = "SL.UEM.TOTL.ZS",
  mortalityRt    = "SH.DYN.MORT",
  CO2Em          = "EN.GHG.CO2.IC.MT.CE.AR5"
)

global_development <- WDI(
  country = "all",
  indicator = indicators,
  start = 2019,
  end = 2019,
  extra = TRUE
)
saveRDS(global_development, "global_development.rds")

## Basic information about the dataset
View(global_development)
head(global_development)
str(global_development)
summary(global_development)
dim(global_development)

sapply(global_development, class)

## Checking for missing values
missing_in_cols <- sapply(global_development, function(x) sum(is.na(x))/nrow(global_development))
head(missing_in_cols)


names(global_development)

data_clean <- global_development %>%
  filter(region != "Aggregates") %>%        
  select(
    country,
    iso3c,
    GDP,
    lifeExp,
    fertilityRt,
    education,
    literacyRt,
    populationGth,
    unemploymentRt,
    mortalityRt,
    CO2Em
  ) %>%
  drop_na()

numeric_data <- data_clean %>%
  select(
    GDP,
    lifeExp,
    fertilityRt,
    education,
    literacyRt,
    populationGth,
    unemploymentRt,
    mortalityRt,
    CO2Em
  )

numeric_data <- numeric_data %>% drop_na()

numeric_long <- numeric_data %>%
  pivot_longer(
    cols = everything(),
    names_to = "indicator",
    values_to = "value"
  )

ggplot(numeric_long, aes(x = value)) +
  geom_histogram(bins = 30, fill = "blue", color = "white") +
  facet_wrap(~ indicator, scales = "free") +
  theme_minimal() +
  labs(
    title = "Distribution of Development Indicators",
    x = "Value",
    y = "Frequency"
  )

ggplot(numeric_long, aes(x = indicator, y = value)) +
  geom_boxplot(fill = "blue") +
  theme_minimal() +
  coord_flip() +
  labs(
    title = "Boxplots of Development Indicators",
    x = "",
    y = "Value"
  )

## Standalize the data
scaled_data <- scale(numeric_data)

# Visualization of your data
correlation<-cor(scaled_data, method="pearson") 
corrplot(correlation)

pca_res <- prcomp(scaled_data)
summary(pca_res) ## Standard deviation greater than 2 is more significant 
#while Proportion of Variance determines the percentage of difference explained by each variable
# Cumulative Proportion shows you the number of PCA you need for your analysis, I decided to pick 2
## Explain more; PC3 as robustness
#Use a method to determine your PCA

# visualize my result
plot(pca_res)


fviz_eig(pca_res, addlabels = TRUE)
fviz_pca_biplot(
  pca_res,
  repel = TRUE,
  col.var = "lblue",
  col.ind = "gray"
)

pca_scores <- pca_res$x[, 1:2]
pca_scores

fviz_nbclust(pca_scores, kmeans, method = "wss")
set.seed(123)

kmeans_model <- kmeans(pca_scores, centers = 3, nstart = 25)

fviz_cluster(
  kmeans_model,
  data = pca_scores,
  geom = "point",
  ellipse.type = "convex",
  main = "Country Clusters Based on Development Indicators"
)

final_data <- data_clean %>%
  mutate(cluster = factor(kmeans_model$cluster))

set.seed(123)

kmeans_model <- kmeans(pca_scores,centers = 3,nstart = 25)

str(kmeans_model)

