# Load required libraries
library(ggplot2)
library(readr)
library(scales)
library(gridExtra)  # for arranging multiple plots

# Set your file path to the CSV file (adjust this according to your file's path)
file_path <- "classification_data_set_post_1979.csv"  # Update with the correct path

# Load the dataset
data <- read_csv(file_path)

# Check column names to ensure they match the dataset
print(colnames(data))

# Titles based on the column names you provided, with Industrial_production_pct_change shortened
titles <- c("PPI_pct_change",
            "Real_GDP_pct_change",
            "Industrial_production_pct_change",
            "Consumer_sentiment_diff",
            "Unemployment_rate_diff",
            "CPI_log")

# Creating a custom plot function with specified colors and custom title mapping
plot_histogram <- function(column_name, data, hist_color = "#B07AA1", line_color = "#4E79A7") {
  # Create a mapping for custom titles
  title_mapping <- c("Industrial_production_pct_change" = "IP_pct")
  
  # Use the custom title if it exists in the mapping, otherwise use the original column name
  display_title <- title_mapping[column_name]
  if (is.na(display_title)) display_title <- column_name
  
  ggplot(data, aes(x = .data[[column_name]])) +
    geom_histogram(aes(y = after_stat(density)), 
                   fill = hist_color, color = "white", alpha = 0.7, 
                   bins = 30) +
    geom_density(color = line_color, size = 1) +
    theme_minimal() +
    labs(title = paste("Distribution of", display_title), 
         x = display_title, 
         y = "Density") +
    theme(text = element_text(family = "Times", size = 10),
          plot.title = element_text(hjust = 0.5, size = 12, face = "bold")) +
    scale_x_continuous(labels = scales::number_format(accuracy = 0.01)) +
    scale_y_continuous(labels = scales::number_format(accuracy = 0.01))
}

# Create a list to store all plots
plot_list <- list()

# Loop through the specified titles and create plots
for (title in titles) {
  if (title %in% colnames(data)) {
    plot_list[[title]] <- plot_histogram(title, data)
  } else {
    cat("Column", title, "not found in the dataset.\n")
  }
}

# Arrange all plots in a grid
combined_plot <- do.call(grid.arrange, c(plot_list, ncol = 2))

# Save the combined plot as a PNG file
ggsave("combined_histograms.png", combined_plot, width = 12, height = 15, units = "in", dpi = 300)

# Print a message to confirm the file has been saved
cat("Combined plot has been saved as 'combined_histograms.png'\n")