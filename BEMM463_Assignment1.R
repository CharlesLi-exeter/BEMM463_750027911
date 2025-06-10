# BEMM463 Assignment 1 R Script
rm(list = ls())
setwd("~/Documents/Exeter university tasks/Marketing Analytics Assessment")
library(tidyverse)
library(dendextend)
library(NbClust)
library(flexclust)

# Read data
data <- read.csv("retailer.csv", stringsAsFactors = FALSE)
data <- data %>% mutate(across(.cols = -respondent_id, .fns = as.numeric))  # Ensure numeric columns
data <- distinct(data)

# Task 1: Descriptive analysis
summary_stats <- data %>%
  select(-respondent_id) %>%
  summarise(across(.cols = everything(), .fns = ~mean(.x, na.rm = TRUE), .names = "mean_{.col}"),
            across(.cols = everything(), .fns = ~median(.x, na.rm = TRUE), .names = "median_{.col}"),
            across(.cols = everything(), .fns = ~min(.x, na.rm = TRUE), .names = "min_{.col}"),
            across(.cols = everything(), .fns = ~max(.x, na.rm = TRUE), .names = "max_{.col}"),
            across(.cols = everything(), .fns = ~sd(.x, na.rm = TRUE), .names = "sd_{.col}")) %>%
  pivot_longer(cols = everything(), names_to = c("statistic", "variable"), names_sep = "_", values_to = "value") %>%
  pivot_wider(names_from = statistic, values_from = value)
print(summary_stats)

# Task 2: Smallest min and largest max
normalized_data <- data %>%
  select(-respondent_id) %>%
  scale() %>%
  as.data.frame()
min_values <- apply(normalized_data, 2, min)
max_values <- apply(normalized_data, 2, max)
smallest_min_var <- names(which.min(min_values))
largest_max_var <- names(which.max(max_values))
cat("Smallest minimum:", smallest_min_var, min_values[smallest_min_var], "\n")
cat("Largest maximum:", largest_max_var, max_values[largest_max_var], "\n")

# Task 3: Clustering data
cluster_data <- data %>%
  select(variety_of_choice, electronics, furniture, quality_of_service, low_prices, return_policy)
normalized_cluster_data <- scale(cluster_data) %>%
  as.data.frame()

# Task 4: Euclidean distance
dist_matrix <- dist(normalized_cluster_data, method = "euclidean")

# Task 5: Set seed
set.seed(123)

# Task 6: Hierarchical clustering
hclust_model <- hclust(dist_matrix, method = "ward.D2")

# Task 7: Plot dendrogram
png("dendrogram.png")
plot(hclust_model, main = "Dendrogram for Hierarchical Clustering (Ward.D2)",
     xlab = "Respondents", ylab = "Height", sub = "")
dev.off()

# Task 8: 3-cluster solution
clusters_3 <- cutree(hclust_model, k = 3)
png("dendrogram_3clusters.png")
dend <- as.dendrogram(hclust_model)
dend <- color_branches(dend, k = 3)
plot(dend, main = "Dendrogram with 3 Clusters (Ward.D2)")
dev.off()

# Task 9: Observations in 3 clusters
print(table(clusters_3))

# Task 10: K-means with 3 clusters
kmeans_3 <- kmeans(normalized_cluster_data, centers = 3, iter.max = 1000, nstart = 100)
print(table(kmeans_3$cluster))

# Task 11: 4-cluster solution
clusters_4 <- cutree(hclust_model, k = 4)
png("dendrogram_4clusters.png")
dend_4 <- as.dendrogram(hclust_model)
dend_4 <- color_branches(dend_4, k = 4)
plot(dend_4, main = "Dendrogram with 4 Clusters (Ward.D2)")
dev.off()
print(table(clusters_4))

# Task 12: K-means with 4 clusters
kmeans_4 <- kmeans(normalized_cluster_data, centers = 4, iter.max = 1000, nstart = 100)
print(table(kmeans_4$cluster))

# Task 13: Compare 3 vs. 4 clusters
nbclust_result <- NbClust(normalized_cluster_data, distance = "euclidean",
                          method = "ward.D2", index = "all", min.nc = 2, max.nc = 6)
print(nbclust_result$Best.nc)

# Task 13 (Part 2): Segment profile plot
data_with_clusters <- data %>%
  mutate(cluster = kmeans_3$cluster)
cluster_means <- data_with_clusters %>%
  select(cluster, variety_of_choice, electronics, furniture, quality_of_service, low_prices, return_policy) %>%
  group_by(cluster) %>%
  summarise_all(mean)
kmeans_cclust <- as.kcca(kmeans_3, normalized_cluster_data)
png("segment_profile.png")
plot(kmeans_cclust, main = "Segment Profile Plot for 3 Clusters")
dev.off()

# Task 14: Demographic analysis
demo_means <- data_with_clusters %>%
  group_by(cluster) %>%
  summarise(mean_income = mean(income), mean_age = mean(age))
print(demo_means)
