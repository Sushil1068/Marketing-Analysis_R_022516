# Marketing-Analysis_R_022516

library(readxl)      # For reading Excel files
library(dplyr)       # For data manipulation
library(ggplot2)     # For plotting
library(cluster)     # For silhouette analysis
library(factoextra)  # For visualizing clustering results (e.g., dendrogram)
library(tidyverse)   # For data manipulation and visualization
library(ggcorrplot)  # For correlation heatmaps

# 1. Load the data
data <- read_excel("SmartWatch Data File (2).xlsx")
head(data)

# 2. Data Cleaning and Preparation
segment_data <- data %>% 
  select(ConstCom, TimelyInf, TaskMgm, DeviceSt, Wellness, Athlete, Style)
segment_data <- na.omit(segment_data)
scaled_data <- scale(segment_data)

# 3. Determine the Optimal Number of Clusters

## 3.1 Elbow Method (Improved with color)
set.seed(123)
wss <- sapply(1:10, function(k){
  kmeans(scaled_data, centers = k, nstart = 25)$tot.withinss
})
elbow_df <- data.frame(k = 1:10, wss = wss)
elbow_plot <- ggplot(elbow_df, aes(x = k, y = wss)) +
  geom_line(color = "steelblue", size = 1.2) +
  geom_point(color = "steelblue", size = 3) +
  labs(x = "Number of Clusters (k)", y = "Total Within Sum of Squares",
       title = "Elbow Method for Determining Optimal Clusters") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(elbow_plot)

## 3.2 Silhouette Analysis (Improved)
sil_width <- sapply(2:10, function(k){
  km.res <- kmeans(scaled_data, centers = k, nstart = 25)
  ss <- silhouette(km.res$cluster, dist(scaled_data))
  mean(ss[, 3])
})
sil_df <- data.frame(k = 2:10, sil_width = sil_width)
sil_plot <- ggplot(sil_df, aes(x = k, y = sil_width)) +
  geom_line(color = "darkgreen", size = 1.2) +
  geom_point(color = "darkgreen", size = 3) +
  labs(x = "Number of Clusters (k)", y = "Average Silhouette Width",
       title = "Silhouette Analysis for Optimal Clusters") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(sil_plot)

# Choose an optimal k based on the above analysis (for example, k = 3)
optimal_k <- 3

# 4. Run k-means clustering
set.seed(123)
km_result <- kmeans(scaled_data, centers = optimal_k, nstart = 25)
data_with_clusters <- data %>% 
  mutate(Cluster = as.factor(km_result$cluster))

# 5. Visualize Clusters using PCA
pca_res <- prcomp(scaled_data, center = TRUE, scale. = TRUE)
pca_data <- as.data.frame(pca_res$x)
pca_data$Cluster <- data_with_clusters$Cluster
cluster_plot <- ggplot(pca_data, aes(x = PC1, y = PC2, color = Cluster)) +
  geom_point(size = 2) +
  labs(title = "K-Means Clustering Visualization (PCA)",
       x = "Principal Component 1", y = "Principal Component 2") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(cluster_plot)

# 6. Dendrogram from Hierarchical Clustering
hclust_res <- hclust(dist(scaled_data), method = "ward.D2")
dendrogram_plot <- fviz_dend(hclust_res, k = optimal_k, 
                             color_labels_by_k = TRUE, 
                             rect = TRUE, 
                             rect_fill = TRUE, 
                             rect_border = "black",
                             main = "Dendrogram of Smartwatch Attributes",
                             xlab = "Observations",
                             ylab = "Height",
                             theme = theme_minimal())
print(dendrogram_plot)

# 7. Additional Graphs

## 7.1 Correlation Heatmap
cor_matrix <- cor(scaled_data)
corr_heatmap <- ggcorrplot(cor_matrix, method = "circle", lab = TRUE, 
                           title = "Correlation Matrix of Smartwatch Attributes") +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(corr_heatmap)

## 7.2 Box Plots for Each Attribute by Cluster
df_long_box <- data_with_clusters %>% 
  select(ConstCom, TimelyInf, TaskMgm, DeviceSt, Wellness, Athlete, Style, Cluster) %>% 
  pivot_longer(cols = ConstCom:Style, names_to = "Attribute", values_to = "Rating")
boxplot_chart <- ggplot(df_long_box, aes(x = Attribute, y = Rating, fill = Cluster)) +
  geom_boxplot() +
  labs(title = "Box Plot of Attribute Ratings by Cluster",
       x = "Smartwatch Attributes", y = "Rating (1-7)") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"),
        axis.text.x = element_text(angle = 45, hjust = 1))
print(boxplot_chart)

## 7.3 PCA Scree Plot
pca_var <- pca_res$sdev^2
pca_var_percent <- round(pca_var/sum(pca_var) * 100, 1)
scree_data <- data.frame(PC = paste0("PC", 1:length(pca_var_percent)), Variance = pca_var_percent)
scree_plot <- ggplot(scree_data, aes(x = PC, y = Variance)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  geom_line(aes(group = 1), color = "blue", size = 1) +
  geom_point(color = "blue", size = 2) +
  labs(title = "PCA Scree Plot",
       x = "Principal Components", y = "Percentage of Variance Explained") +
  theme_minimal() +
  theme(plot.title = element_text(hjust = 0.5, face = "bold"))
print(scree_plot)

# 8. Summarize Cluster Characteristics
cluster_summary <- data_with_clusters %>%
  group_by(Cluster) %>%
  summarise(across(c(ConstCom, TimelyInf, TaskMgm, DeviceSt, Wellness, Athlete, Style), mean, na.rm = TRUE),
            Count = n())
print(cluster_summary)

## 8.1 Create a Grouped Bar Chart for Average Ratings by Cluster (using provided tibble)
df <- tibble::tribble(
  ~Cluster, ~ConstCom, ~TimelyInf, ~TaskMgm, ~DeviceSt, ~Wellness, ~Athlete, ~Style, ~Count,
  "1",       5.47,      5.70,      4.93,     5.09,     3.55,    2.44,    3.44,   207,
  "2",       5.11,      4.40,      4.69,     4.34,     5.78,    5.04,    5.29,   430,
  "3",       3.75,      3.36,      3.18,     2.53,     3.16,    3.15,    3.62,   363
)

df_long <- df %>% 
  pivot_longer(
    cols = ConstCom:Style,
    names_to = "Attribute",
    values_to = "Rating"
  )

bar_chart <- ggplot(df_long, aes(x = Attribute, y = Rating, fill = Cluster)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(
    title = "Average Smartwatch Attribute Ratings by Cluster",
    x = "Smartwatch Attributes",
    y = "Average Rating (1-7)",
    fill = "Cluster"
  ) +
  scale_fill_brewer(palette = "Pastel1") +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )
print(bar_chart)

# 9. Save the results if needed
write.csv(data_with_clusters, "Smartwatch_Clusters.csv", row.names = FALSE)

