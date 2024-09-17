# Load required libraries
library(readr)
library(dplyr)
library(lubridate)
library(mclust)  # for GMM
library(cluster)  # for silhouette
library(ggplot2)

# Load the dataset
df <- read_csv('classification_data_set_post_1979.csv')
df$DATE <- as.Date(df$DATE)
df <- df %>% column_to_rownames('DATE')

# Min-Max normalization function
min_max_normalize <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}

# Normalize the data
normalized_df <- as.data.frame(lapply(df, min_max_normalize))

# Function to calculate BIC and Silhouette scores
calculate_metrics <- function(data, max_components = 10) {
  n_components_range <- 2:max_components
  bic_values <- numeric(length(n_components_range))
  silhouette_scores <- numeric(length(n_components_range))
  
  for (i in seq_along(n_components_range)) {
    n <- n_components_range[i]
    
    # Fit GMM and calculate BIC
    gmm <- Mclust(data, G = n)
    bic_values[i] <- gmm$BIC
    
    # Calculate Silhouette score
    sil <- silhouette(gmm$classification, dist(data))
    silhouette_scores[i] <- mean(sil[, 3])
  }
  
  list(n_components = n_components_range,
       bic = bic_values,
       silhouette = silhouette_scores)
}

# Calculate metrics
metrics <- calculate_metrics(normalized_df)

# Create a data frame for plotting
plot_data <- data.frame(
  n_components = metrics$n_components,
  BIC = metrics$bic,
  Silhouette = metrics$silhouette
)

# Plot using ggplot2
p <- ggplot(plot_data, aes(x = n_components)) +
  geom_line(aes(y = BIC, color = "BIC")) +
  geom_point(aes(y = BIC, color = "BIC")) +
  geom_line(aes(y = Silhouette * diff(range(BIC)) + min(BIC), color = "Silhouette")) +
  geom_point(aes(y = Silhouette * diff(range(BIC)) + min(BIC), color = "Silhouette")) +
  scale_y_continuous(
    name = "BIC",
    sec.axis = sec_axis(~ (. - min(plot_data$BIC)) / diff(range(plot_data$BIC)),
                        name = "Silhouette Score")
  ) +
  scale_color_manual(values = c("BIC" = "blue", "Silhouette" = "red")) +
  labs(x = "Number of components",
       title = "BIC and Silhouette Scores vs. Number of GMM Components",
       color = "Metric") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Save the plot
ggsave("gmm_metrics_plot.png", p, width = 12, height = 6, dpi = 300)

print("Plot has been saved as 'gmm_metrics_plot.png'")