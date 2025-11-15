# Load necessary library
library(dplyr)
library(skimr)
library(ggplot2)

# Define file paths
file_paths <- c("Data Collection - Lower Div Boys.csv", "Data Collection - Lower Div Girls.csv", "Data Collection - Upper Div Boys.csv", "Data Collection - Upper Div Girls.csv")  # adjust file names as needed

# Define columns to keep
columns_to_keep <- c("Name", "School.Town", "Grade.Level", "Gender", "Music.Genre", "Difference.in.Time")

# Read and clean each file, then combine
cleaned_data <- lapply(file_paths, function(file) {
  read.csv(file, stringsAsFactors = FALSE) %>%
    select(all_of(columns_to_keep))
}) %>% 
  bind_rows()

cleaned_data <- cleaned_data %>%
  mutate(
    Grade.Level = as.numeric(Grade.Level),  # Ensure it's numeric
    Division = case_when(
      Grade.Level >= 1 & Grade.Level <= 3 ~ "lower",
      Grade.Level >= 4 & Grade.Level <= 6 ~ "upper",
      TRUE ~ NA_character_  # fallback for unexpected values
    ),
    # reorder the factors
    Music.Genre = factor(Music.Genre, levels = c("Control", "Classical", "Country", "Heavy Metal"))
  )

# View first few rows
head(cleaned_data)

# Write to a new CSV
write.csv(cleaned_data, "cleaned_results.csv", row.names = FALSE)

# summary statistics
skim(cleaned_data)

# Analysis of Variance
model <- aov(Difference.in.Time ~ Music.Genre+Gender+Division, data = cleaned_data)
summary(model)

# check for residual assumption
res <- residuals(model)
fit <- fitted(model)
png("residual_plot.png", width = 600, height = 400)
plot(fit, res, main = "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")
dev.off()


png("qq_plot.png", width = 600, height = 400)
qqnorm(res, main = "Normal Q-Q Plot")
qqline(res, col = "blue")
dev.off()

# Post-hoc test
TukeyHSD(model)

# graphs
genre_plot <- ggplot(cleaned_data, aes(x = Music.Genre, y = Difference.in.Time, fill = Music.Genre)) +
  geom_boxplot() +
  labs(
    title = "Effect of Music Genre on Difference in Time",
    x = "Music Genre",
    y = "Difference in Time"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# Save it
ggsave("music_genre_boxplot.png", plot = genre_plot, width = 8, height = 6, dpi = 300)

# additional analysis

# Get the unique genres
genres <- unique(cleaned_data$Music.Genre)

# Loop through each genre and perform a one-sample t-test
for (g in genres) {
  cat("\n---", g, "---\n")
  genre_data <- cleaned_data %>%
    filter(Music.Genre == g) %>%
    pull(Difference.in.Time)
  
  print(t.test(genre_data, mu = 0))
}

