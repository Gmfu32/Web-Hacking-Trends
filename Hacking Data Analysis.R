library(dplyr)
library(stringr)
library(countrycode)
library(ggplot2)
library(tidyr)
library(cluster)
library(FSA)
library(lubridate)
library(MASS) 
library(readr)
library(car)  # For Levene's test
library(rcompanion)


#============================ Data Import ============================
fileUrl <- file.choose()  
data <- read.csv(fileUrl)
data
View(data)



#============================ CountryAnalysis ============================


#============================ Feature Engineering ============================

# Remove countries with very few records (< 30 incidents)
filtered_data <- data %>%
  group_by(Country) %>%
  filter(n() > 30)

# =========================== Clustering Analysis ===========================

# Filter the top 10 countries based on the number of incidents
high_incident_countries <- filtered_large %>%
  group_by(Country) %>%
  summarise(
    Total_Incidents = n(),
    Mean_Loss = mean(Loss, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Incidents)) %>%
  slice(1:20)  # Top 20 countries by incident count

# Perform clustering based on financial loss (Mean_Loss) for these top 10 countries
# Compute distance matrix
dist_matrix <- dist(high_incident_countries$Mean_Loss, method = "euclidean")

# Perform hierarchical clustering
clustering_result <- hclust(dist_matrix, method = "ward.D2")

# Plot the dendrogram
plot(clustering_result, labels = high_incident_countries$Country, 
     main = "Clustering of High-Incident Countries by Financial Loss", 
     xlab = "Countries", ylab = "Distance", cex = 0.8)

# To get a clearer look,added a horizontal line to cut the dendrogram into clusters
abline(h = 10, col = "red", lty = 2)

#============================ Exploratory Analysis ===================

# Summary Statistics of Financial Loss by Country
summary_stats_ext <- filtered_data %>%
  group_by(Country) %>%
  summarise(
    Count = n(),
    Mean_Loss = mean(Loss, na.rm = TRUE),
    Median_Loss = median(Loss, na.rm = TRUE),
    SD_Loss = sd(Loss, na.rm = TRUE),
    Min_Loss = min(Loss, na.rm = TRUE),
    Max_Loss = max(Loss, na.rm = TRUE),
    Skewness = e1071::skewness(Loss, na.rm = TRUE),
    Kurtosis = e1071::kurtosis(Loss, na.rm = TRUE)
  ) %>%
  arrange(desc(Mean_Loss))

print(summary_stats_ext)

#========================= Correlation Analysis ====================

# Perform Spearman's rank correlation between Total_Incidents and Total_Loss
correlation_result <- cor.test(country_loss_summary$Total_Incidents, 
                               country_loss_summary$Total_Loss, 
                               method = "spearman")

# Print the correlation result
print(correlation_result)

#============================ Outliers Detection ============================

# Identifying outliers
# Compute IQR (Interquartile Range)
Q1 <- quantile(filtered_large$Loss, 0.25, na.rm = TRUE)  # 25th percentile
Q3 <- quantile(filtered_large$Loss, 0.75, na.rm = TRUE)  # 75th percentile
IQR_value <- Q3 - Q1  # IQR calculation

# Define lower and upper bounds for outliers
lower_bound <- Q1 - 1.5 * IQR_value
upper_bound <- Q3 + 1.5 * IQR_value

# Filter outliers
outliers <- filtered_large %>%
  filter(Loss < lower_bound | Loss > upper_bound)

# View outliers
print(outliers %>% dplyr::select(Country, Loss))

# Find Which Countries Appear Most Frequently as Outliers
outliers %>%
  count(Country) %>%
  arrange(desc(n))

# Check if the U.S. has more total incidents
filtered_large %>%
  count(Country) %>%
  arrange(desc(n))

# Check if U.S. has Higher Average Loss per Incident
us_stats <- filtered_large %>%
  filter(Country == "United States") %>%
  summarise(
    Mean_Loss = mean(Loss, na.rm = TRUE),
    Median_Loss = median(Loss, na.rm = TRUE),
    Max_Loss = max(Loss, na.rm = TRUE)
  )

print(us_stats)

# Calculate global loss (excluding the U.S.) and order from highest to lowest
country_stats <- filtered_large %>%
  filter(Country != "United States") %>%
  group_by(Country) %>%
  summarise(
    Total_Loss = sum(Loss, na.rm = TRUE),
    Mean_Loss = mean(Loss, na.rm = TRUE),
    Median_Loss = median(Loss, na.rm = TRUE),
    Max_Loss = max(Loss, na.rm = TRUE)
  ) %>%
  arrange(desc(Total_Loss))  # Arrange countries by Total_Loss in descending order

# Print the sorted statistics
print(country_stats)


#============================ Time-Series Analysis =====================

# Ensure Date is in Date format
filtered_data$Date <- as.Date(filtered_data$Date, format = "%m/%d/%Y")

# Summarize total loss by country to get the top 10 countries based on total loss
top_countries <- filtered_data %>%
  group_by(Country) %>%
  summarise(Total_Loss_All_Time = sum(Loss, na.rm = TRUE)) %>%
  arrange(desc(Total_Loss_All_Time)) %>%
  head(10) %>%
  pull(Country)  # Get the names of the top 10 countries

# Filter original data to only include top 10 countries
time_series_data <- filtered_data %>%
  filter(Country %in% top_countries) %>%
  group_by(Date, Country) %>%
  summarise(Total_Loss = sum(Loss, na.rm = TRUE), .groups = 'drop')  # Drop grouping after summarizing

# Plot the time series, with color by Country
# Filter out data where Total Loss exceeds 300000 to avoid extreme outliers
filtered_data_no_outliers <- time_series_data %>%
  filter(Total_Loss <= 300000)

# Plot without outliers
ggplot(filtered_data_no_outliers, aes(x = Date, y = Total_Loss, color = Country)) +
  geom_line() +
  labs(title = "Trend of Financial Loss Over Time (Top 10 Countries)", x = "Date", y = "Total Loss (USD)") +
  theme_minimal() +
  theme(legend.title = element_blank())

#================ Statistical Analysis (Comparing US loss to other major economy) ==============

# Filter data for the relevant countries
selected_countries <- c("United States", "China", "Japan", "Germany")
filtered_data <- data %>% filter(Country %in% selected_countries)

# Check if the data is filtered correctly
head(filtered_data)

# Boxplot to compare financial losses across the selected countries
ggplot(filtered_data, aes(x = Country, y = Loss, fill = Country)) +
  geom_boxplot(outlier.colour = "red", outlier.shape = 16, outlier.size = 2) +
  labs(title = "Financial Loss Distribution by Country",
       x = "Country", 
       y = "Financial Loss (USD)") +
  theme_minimal()


# Violin plot for financial loss comparison
ggplot(filtered_data, aes(x = Country, y = Loss, fill = Country)) +
  geom_violin(trim = TRUE) +
  labs(title = "Financial Loss Distribution by Country",
       x = "Country", 
       y = "Financial Loss (USD)") +
  theme_minimal()


# Kruskal-Wallis test to compare financial losses across the countries
kruskal_test_result <- kruskal.test(Loss ~ Country, data = filtered_data)

# Print the test result
print(kruskal_test_result)

#======= Statistical Analysis (Finding countries with low incidents but high loss) =========

# Calculate the average financial loss per incident
country_loss_summary <- filtered_data %>%
  group_by(Country) %>%
  summarise(
    Total_Incidents = n(),
    Total_Loss = sum(Loss, na.rm = TRUE),
    Avg_Loss_Per_Incident = Total_Loss / Total_Incidents
  )


# Adjust thresholds for low incidents (<200) and high loss per incident (>1600)
threshold_incidents <- 200
threshold_loss_per_incident <- 1600

# Filter countries that meet the criteria for low incidents and high financial loss
high_loss_low_incidents <- country_loss_summary %>%
  filter(Total_Incidents < threshold_incidents & Avg_Loss_Per_Incident > threshold_loss_per_incident)

# Check the filtered data to ensure there are countries meeting the criteria
print(high_loss_low_incidents)

# Scatter plot to show the relationship between incidents and average loss per incident (for low incident, high loss countries only)
ggplot(high_loss_low_incidents, aes(x = Total_Incidents, y = Avg_Loss_Per_Incident)) +
  geom_point(aes(color = Country), size = 4) +
  geom_text(aes(label = Country), vjust = -0.5, size = 3) +  # Label countries
  labs(title = "Low Incident, High Loss Countries",
       x = "Number of Incidents", y = "Average Loss Per Incident (USD)") +
  theme_minimal()

#============================ Hypothesis Testing =======================

# Hypothesis Testing: Kruskal-Wallis Test (Non-Normal Data)
kruskal.test(Loss ~ Country, data = filtered_data)

# Alternative: Log-Transformed Analysis
kruskal.test(Log_Loss ~ Country, data = filtered_data)

dunnTest(Loss ~ Country, data = filtered_data, method = "bonferroni")
#============================ Visualization ============================

# Compare Financial Loss Across High-Incident Countries
high_incident_countries <- filtered_large %>%
  group_by(Country) %>%
  summarise(Total_Incidents = n(), Mean_Loss = mean(Loss, na.rm = TRUE)) %>%
  arrange(desc(Total_Incidents)) %>%
  slice(1:10)  # Top 10 countries by incident count

ggplot(high_incident_countries, aes(x = reorder(Country, -Total_Incidents), y = Mean_Loss)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Mean Financial Loss in High-Incident Countries",
       x = "Country", y = "Mean Financial Loss (USD)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# To confirm if the U.S. has a right-skewed distribution, we can plot a histogram or density plot
ggplot(filtered_large %>% filter(Country == "United States"), aes(x = Loss)) +
  geom_histogram(binwidth = 500, fill = "steelblue", color = "white") +
  labs(title = "Distribution of Financial Loss in U.S. Cyber Incidents",
       x = "Financial Loss (USD)", y = "Frequency") +
  theme_minimal()


# Boxplot of Financial Loss per Country
# Calculate median loss per country, filter top 20 countries with the highest median loss
top_countries <- filtered_large %>%
  group_by(Country) %>%
  summarise(Median_Loss = median(Loss, na.rm = TRUE)) %>%
  arrange(desc(Median_Loss)) %>%
  head(20) %>%
  pull(Country)  # Get the names of the top 20 countries


# Filter the data to include only top 20 countries
filtered_top_countries <- filtered_large %>%
  filter(Country %in% top_countries)

# Create the boxplot for the top 20 countries
ggplot(filtered_top_countries, aes(x = reorder(Country, Loss, FUN = median), y = Loss)) +
  geom_boxplot(outlier.colour = "red", fill = "lightblue") +
  labs(title = "Financial Loss Distribution (Top 20 Countries)",
       x = "Country", y = "Financial Loss (USD)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Violin Plot of the density of financial loss distribution per country.
# Filter top 20 countries based on median financial loss
top_countries <- filtered_large %>%
  group_by(Country) %>%
  summarise(Median_Loss = median(Loss, na.rm = TRUE)) %>%
  arrange(desc(Median_Loss)) %>%
  slice(1:20)  # Select top 10 countries

ggplot(filtered_large %>% filter(Country %in% top_countries$Country), 
       aes(x = reorder(Country, Loss, FUN = median), y = Loss)) +
  geom_violin(fill = "lightblue") +
  labs(title = "Financial Loss Density (Top 20 Countries)", 
       x = "Country", y = "Financial Loss (USD)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Scatter Plot: Financial Loss vs. Number of Incidents
# Calculate the number of incidents and total financial loss per country
incident_loss_data <- filtered_large %>%
  group_by(Country) %>%
  summarise(
    Number_of_Incidents = n(),  # Count number of incidents
    Total_Loss = sum(Loss, na.rm = TRUE)  # Sum of financial loss
  )

incident_loss_data <- incident_loss_data %>%
  top_n(10, Total_Loss)  # Only top 10 countries by total loss

# Scatter plot for Financial Loss vs. Number of Incidents
ggplot(incident_loss_data, aes(x = Number_of_Incidents, y = Total_Loss)) +
  geom_point(color = "blue", alpha = 0.6, size = 3) +  # Add points with transparency
  geom_smooth(method = "lm", color = "red", se = TRUE) +  # Add a linear regression line
  geom_label(aes(label = Country), size = 3, fill = "white", fontface = "bold") +  # Add country labels to each point
  labs(title = "Financial Loss vs. Number of Incidents",
       x = "Number of Incidents",
       y = "Total Financial Loss (USD)") +
  theme_minimal()

#============================ Ransom Analysis ============================


#============================ Exploratory Data Analysis ============================

# Descriptive statistics
data %>%
  summarise(
    avg_ransom = mean(Ransom, na.rm = TRUE),
    median_ransom = median(Ransom, na.rm = TRUE),
    sd_ransom = sd(Ransom, na.rm = TRUE),
    IQR_ransom = IQR(Ransom, na.rm = TRUE),
    avg_loss = mean(Loss, na.rm = TRUE),
    median_loss = median(Loss, na.rm = TRUE),
    sd_loss = sd(Loss, na.rm = TRUE),
    IQR_loss = IQR(Loss, na.rm = TRUE)
  )


#============================ Descriptive Statistics ===============

# Calculate summary statistics for the Ransom column
summary_stats <- data %>%
  summarise(
    count = n(),
    missing = sum(is.na(Ransom)),
    mean = mean(Ransom, na.rm = TRUE),
    median = median(Ransom, na.rm = TRUE),
    sd = sd(Ransom, na.rm = TRUE),
    IQR = IQR(Ransom, na.rm = TRUE),
    min = min(Ransom, na.rm = TRUE),
    max = max(Ransom, na.rm = TRUE)
  )

print(summary_stats)


ggplot(data, aes(x = Ransom)) +
  geom_histogram(binwidth = 500, fill = "steelblue", color = "black") +
  labs(title = "Histogram of Ransom Demands", x = "Ransom Demand", y = "Frequency") +
  theme_minimal()


ggplot(data, aes(x = Ransom)) +
  geom_density(fill = "lightgreen", alpha = 0.6) +
  labs(title = "Density Plot of Ransom Demands", x = "Ransom Demand", y = "Density") +
  theme_minimal()


#============================ Correlation Analysis ===========================

# Spearman's rank correlation between Ransom and Loss
correlation_result <- cor.test(data$Ransom, data$Loss, method = "spearman")
print(correlation_result)


# Create a scatter plot with a regression line
ggplot(data, aes(x = Ransom, y = Loss)) +
  geom_point(color = "blue", alpha = 0.6) +  # Scatter plot of data points
  geom_smooth(method = "lm", se = FALSE, color = "red") +  # Add a linear regression line
  labs(title = "Scatter Plot of Ransom vs Financial Loss",
       x = "Ransom Demand",
       y = "Financial Loss") +
  theme_minimal() +
  annotate("text", x = max(data$Ransom) * 0.8, y = max(data$Loss) * 0.9,
           label = paste("Spearman's rho = 0.648"), color = "black", size = 5)

#============================ Frequency Analysis ============================

# Define the threshold for large ransom demands
threshold <- 2500

# Create a new column indicating whether the ransom is large
data$LargeRansom <- ifelse(data$Ransom > threshold, 1, 0)

# Check how many large ransom demands exist in the dataset
sum(data$LargeRansom)


# Extract the year from the Date column
data$Year <- format(data$Date, "%Y")

# Aggregate by year to count the number of large ransom demands
large_ransom_by_year <- data %>%
  group_by(Year) %>%
  summarise(LargeRansomCount = sum(LargeRansom))

# View the aggregated data
head(large_ransom_by_year)


# Plot the trend of large ransom demands over time (aggregated by year)
ggplot(large_ransom_by_year, aes(x = Year, y = LargeRansomCount)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red", size = 2) +
  labs(title = "Trend of Large Ransom Demands Over Time",
       x = "Year",
       y = "Number of Large Ransom Demands") +
  theme_minimal()


#============================ Hypothesis Testing ============================

# Calculate Spearman's correlation (for non-normally distributed data)
correlation_result <- cor.test(data$Ransom, data$Loss, method = "spearman")

# Display the correlation result
print(correlation_result)


#============================ WebServer Analysis ============================

#============================ frequency analysis =========================

# Frequency of attacks by WebServer
webserver_counts <- table(data$WebServer)

# Create frequency table of attacks by WebServer
webserver_counts <- table(data$WebServer)

# Convert to a data frame and sort by frequency
webserver_df <- as.data.frame(webserver_counts)
colnames(webserver_df) <- c("WebServer", "Frequency")
webserver_df <- webserver_df[order(-webserver_df$Frequency), ]

# Get top 10 web servers
top10_webservers <- head(webserver_df, 10)

# Plot
library(ggplot2)
ggplot(top10_webservers, aes(x = reorder(WebServer, -Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  labs(title = "Top 10 Web Server Attacks",
       x = "Web Server Type",
       y = "Frequency of Attacks") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the counts
print(webserver_counts)

# Convert the frequency table to a data frame for better visualization
webserver_counts_df <- as.data.frame(webserver_counts)
colnames(webserver_counts_df) <- c("WebServer", "Frequency")

# Plot the frequency of each WebServer type
ggplot(webserver_counts_df, aes(x = WebServer, y = Frequency)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +  # Rotate x-axis labels for readability
  labs(title = "Frequency of Web Server Attacks",
       x = "Web Server Type",
       y = "Frequency of Attacks")

#============================ Statistical Analysis =========================

# Group by WebServer and calculate the average loss for each server type
average_loss_per_server <- data %>%
  group_by(WebServer) %>%
  summarise(AverageLoss = mean(Loss, na.rm = TRUE))

# Display the result
print(average_loss_per_server)


# Sort the results in descending order to find the web server with the highest average loss
sorted_loss <- average_loss_per_server %>%
  arrange(desc(AverageLoss))

# Display the top web server with the highest average loss
top_webserver <- sorted_loss[1, ]
print(top_webserver)


# Plot the average financial loss per WebServer
top10_loss <- average_loss_per_server %>%
  arrange(desc(AverageLoss)) %>%
  slice(1:10)

str(average_loss_per_server)

ggplot(top10_loss, aes(x = reorder(Webserver, -AverageLoss), y = AverageLoss)) +
  geom_bar(stat = "identity", fill = "lightblue") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  labs(title = "Top 10 Web Servers by Average Financial Loss",
       x = "Web Server Type",
       y = "Average Financial Loss")
head(top10_loss)
#============================ Comparative Analysis =========================

# Classify Web Servers into open-source, proprietary, and other
data$ServerType <- case_when(
  data$WebServer %in% c("Apache", "Nginx", "LiteSpeed", "lighttpd") ~ "Open-Source",
  data$WebServer %in% c("IIS", "zeus", "microsoftofficeweb5.0_pub") ~ "Proprietary",
  TRUE ~ "Other"  # For any web servers that don't fit the above categories
)

# Check the classification
table(data$ServerType)

# Group by ServerType and calculate the average financial loss
average_loss_by_type <- data %>%
  group_by(ServerType) %>%
  summarise(AverageLoss = mean(Loss, na.rm = TRUE))

# Display the result
print(average_loss_by_type)

# Visualize the average financial loss by server type
ggplot(average_loss_by_type, aes(x = ServerType, y = AverageLoss, fill = ServerType)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Financial Loss by Web Server Type",
       x = "Server Type",
       y = "Average Financial Loss") +
  theme_minimal()


# Perform the Kruskal-Wallis test to compare financial loss across server types
kruskal_result <- kruskal.test(Loss ~ ServerType, data = data)

# Display the results of the Kruskal-Wallis test
print(kruskal_result)


#============================ Hypothesis Testing =========================

# Hypothesis Testing

# Kruskal-Wallis Test
kruskal.test(Loss ~ WebServer, data = data)




#============================ OS Analysis =========================


# Financial Loss Boxplot by OS
boxplot_os <- ggplot(data_clean, aes(x = OS, y = Loss)) +
  geom_boxplot(fill = "lightblue", color = "black") +
  theme_minimal() +
  labs(title = "Financial Loss by Operating System",
       x = "Operating System",
       y = "Financial Loss (in USD)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Bar Chart of Average Financial Loss by OS
avg_loss_os <- data_clean %>%
  group_by(OS) %>%
  summarise(Average_Loss = mean(Loss, na.rm = TRUE)) %>%
  ggplot(aes(x = reorder(OS, -Average_Loss), y = Average_Loss, fill = OS)) +
  geom_col() +
  theme_minimal() +
  labs(title = "Average Financial Loss by Operating System",
       x = "Operating System",
       y = "Average Financial Loss (in USD)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Perform Kruskal-Wallis test (Non-parametric alternative to ANOVA)
kruskal_test <- kruskal.test(Loss ~ OS, data = data_clean)
print(kruskal_test)

# Perform pairwise Wilcoxon rank-sum tests with Bonferroni correction
pairwise_wilcox <- pairwise.wilcox.test(data_clean$Loss, data_clean$OS, p.adjust.method = "bonferroni")
print(pairwise_wilcox)

# Interpretation:
if (kruskal_test$p.value < 0.05) {
  cat("There is a statistically significant difference in financial loss across different operating systems.\n")
} else {
  cat("There is no significant difference in financial loss across operating systems.\n")
}

# Compute summary statistics for visualization
summary_data <- data_clean %>%
  group_by(OS) %>%
  summarise(Average_Loss = mean(Loss, na.rm = TRUE),
            SD = sd(Loss, na.rm = TRUE),
            Count = n()) %>%
  mutate(SE = SD / sqrt(Count))  # Standard Error for error bars

# Create a bar chart showing average financial loss by OS with error bars
bar_chart <- ggplot(summary_data, aes(x = reorder(OS, -Average_Loss), y = Average_Loss, fill = OS)) +
  geom_bar(stat = "identity", alpha = 0.7) +
  geom_errorbar(aes(ymin = Average_Loss - SE, ymax = Average_Loss + SE), width = 0.2, color = "black") +
  theme_minimal() +
  labs(title = "Average Financial Loss by Operating System",
       x = "Operating System",
       y = "Average Financial Loss (USD)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
print(bar_chart)
# Boxplot: Financial Loss by Operating System
boxplot_os <- ggplot(data_clean, aes(x = OS, y = Loss, fill = OS)) +
  geom_boxplot(alpha = 0.7, outlier.colour = "red", outlier.shape = 16) +
  theme_minimal() +
  labs(title = "Financial Loss Distribution by Operating System",
       x = "Operating System",
       y = "Financial Loss (USD)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

# Display the plot
print(boxplot_os)


# Convert OS to factor for correlation analysis
data_clean$OS <- as.factor(data_clean$OS)

# Calculate Cramér's V for OS and Loss (Categorical vs Continuous)
cramer_v_result <- cramerV(as.numeric(as.factor(data_clean$OS)), data_clean$Loss)

# Print the result
cat("Cramér's V value for OS and Financial Loss:", round(cramer_v_result, 3), "\n")

# Interpretation based on effect size:
if (cramer_v_result < 0.1) {
  cat("There is little to no correlation between the operating system and financial loss severity.\n")
} else if (cramer_v_result < 0.3) {
  cat("There is a weak correlation between the operating system and financial loss severity.\n")
} else if (cramer_v_result < 0.5) {
  cat("There is a moderate correlation between the operating system and financial loss severity.\n")
} else {
  cat("There is a strong correlation between the operating system and financial loss severity.\n")
}

# Boxplot: Financial Loss Distribution by OS (to visualize severity)
boxplot_severity <- ggplot(data_clean, aes(x = OS, y = Loss, fill = OS)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Financial Loss Severity by Operating System",
       x = "Operating System",
       y = "Financial Loss (USD)") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


# Hypothesis Testing for Non-Normal Data
# Check Normality with Anderson-Darling test
ad_result <- ad.test(data_clean$Loss)
print(ad_result)

if (ad_result$p.value < 0.05) {
  print("Financial Loss is NOT normally distributed. Proceeding with non-parametric tests.")
  
  # Kruskal-Wallis Test (for multiple OS groups)
  kruskal_test <- kruskal.test(Loss ~ OS, data = data_clean)
  print(kruskal_test)
  
  # Wilcoxon Rank-Sum Test (if only two OS groups)
  if (length(unique(data_clean$OS)) == 2) {
    wilcox_test <- wilcox.test(Loss ~ OS, data = data_clean)
    print(wilcox_test)
  }
  
  # Generalized Linear Model (GLM) with Gamma distribution
  glm_model <- glm(Loss ~ OS, data = data_clean, family = Gamma(link = "log"))
  summary(glm_model)
} else {
  print("Financial Loss is normally distributed. Use parametric tests instead.")
}




