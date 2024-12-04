# 1.Reading the CSV file
data = read.csv("Content.csv", header = TRUE)

# Display the first few rows of the data
head(data)

# 2.Reading the CSV file
#data <- read.csv("Content.csv", header = TRUE)

# Display the total number of rows
total_rows <- nrow(data)
print(total_rows)

# 3.Removing rows with NA values
cleaned_data <- na.omit(data)

# Display the cleaned data
print(cleaned_data)

# 4_1.Install the plotrix library if you haven't
install.packages("plotrix")

# Load the library
library(plotrix)

# Read your CSV file
data <- read.csv("Content.csv", header = TRUE)

# Create a frequency table for the specific column
category_counts <- table(data$Gender)

# Calculate the percentage occurrence
category_percent <- prop.table(category_counts) * 100

# Create labels with percentages
labels <- paste(names(category_percent), round(category_percent, 3), "%")

# Plot a pie chart with percentage labels
pie(category_percent, labels = labels, main = "Percentage Occurrence of Category")





# 4_2.Install and load the dplyr package if you haven't already
install.packages("dplyr")
library(dplyr)

# Reading the CSV file
data <- read.csv("Content.csv", header = TRUE)

cleaned_data <- na.omit(data)

# Load necessary libraries
library(dplyr)
library(ggplot2)

# Assuming 'cleaned_data' is your dataset with an 'Age' column
updated_data <- cleaned_data %>%
  mutate(Category = case_when(
    Age <= 18 ~ "CHILD",
    Age <= 65 ~ "ADULT",
    TRUE ~ "SENIOR CITIZEN"  # This will catch all values greater than 65
  ))

# Create a frequency table for the categories
category_counts <- updated_data %>%
  count(Category)

# Plot a bar graph with count labels
ggplot(category_counts, aes(x = Category, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = n), vjust = -0.5, size = 5) +  # Add counts above bars
  labs(title = "Age Category Distribution", x = "Category", y = "Count") +
  theme_minimal()



# 4_6.Load the necessary library
library(ggplot2)

# Sample data generation
set.seed(123)  # For reproducibility
n <- 1000  # Number of observations

# Create a data frame with Gender and Age
data <- data.frame(
  ChestPainType = sample(c("Typical Angina", "Atypical Angina", "Non-anginal Pain", "Asymptomatic"), n, replace = TRUE),  # Randomly assigning Gender
  Age = sample(15:90, n, replace = TRUE)  # Random ages between 18 and 70
)

# Create a scatter plot with Gender on the x-axis and Age on the y-axis
ggplot(data, aes(x = ChestPainType, y = Age)) +
  geom_jitter(color = "blue", width = 0.2, height = 0) +  # Use jitter to avoid overplotting
  labs(title = "Scatter Plot of Age by Chest Pain Type",  # Title of the plot
       x = "Chest Pain Type",  # Label for the x-axis
       y = "Age") +  # Label for the y-axis
  theme_minimal()  # Use a minimal theme for a clean look


# 4_5.Load necessary libraries
library(dplyr)
library(ggplot2)

# Assuming 'cleaned_data' is your dataset with an 'Age' column
updated_data <- cleaned_data %>%
  mutate(Category = case_when(
    SmokingStatus == 'Former' ~ "Former",
    SmokingStatus == 'Never' ~ "Never" ,
    SmokingStatus == 'Current' ~ "Current"
  ))

# Create a frequency table for the categories
category_counts <- updated_data %>%
  count(Category)

# Plot a bar graph with count labels
ggplot(category_counts, aes(x = Category, y = n)) +
  geom_bar(stat = "identity", fill = "steelblue") +
  geom_text(aes(label = n), vjust = -0.5, size = 5) +  # Add counts above bars
  labs(title = "Age Category Distribution", x = "Category", y = "Count") +
  theme_minimal()


# 4_4 Has_Diabetics

# Create a frequency table for the Response column
response_counts <- table(cleaned_data$HasDiabetes)

# Calculate the percentage occurrence
response_percent <- prop.table(response_counts) * 100

# Create labels with percentages
labels <- paste(names(response_percent), round(response_percent, 1), "%")

# Plot a pie chart with percentage labels
pie(response_percent, labels = labels, main = "Percentage of Has Diabetes")

#4_7 Blood Pressure

# Read your CSV file
cleaned_data <- read.csv("Content.csv", header = TRUE)

# Create a frequency table and convert it to a data frame
blood_pressure_counts <- as.data.frame(table(cleaned_data$BloodPressure))

# Rename the columns for easier access
colnames(blood_pressure_counts) <- c("BloodPressure", "Count")


# Create the bar chart
ggplot(blood_pressure_counts, aes(x = BloodPressure, y = Count, fill = BloodPressure)) +
  geom_bar(stat = "identity") +
  labs(title = "Blood Pressure Readings Count",
       y = "Count",
       x = "Blood Pressure (mm Hg)") +
  theme_minimal()




# Load necessary libraries
library(ggplot2)

# Read your CSV file
cleaned_data <- read.csv("Content.csv", header = TRUE)

# Define a function to categorize blood pressure
categorize_bp <- function(bp) {
  if (bp < 120) {
    return("Normal")
  } else if (bp >= 130 & bp <= 139) {
    return("Stage 1 Hypertension")
  } else if (bp >= 140 & bp < 160) {
    return("Stage 2 Hypertension")
  } else if (bp >= 160) {
    return("Stage 3 Hypertension")
  } else {
    return("Other")
  }
}

# Apply the categorization function to the Blood Pressure data
Category <- sapply(cleaned_data$BloodPressure(mmHg), categorize_bp)

# Create a frequency table of the categories
category_counts <- as.data.frame(table(cleaned_data$Category, cleaned_data$Age))

# Rename the columns for easier access
colnames(category_counts) <- c("Category", "Age", "Count")

# Create a bar chart with Age on the y-axis and Categories on the x-axis
ggplot(category_counts, aes(x = Category, y = Count, fill = Category)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = "Blood Pressure Categories by Age",
       y = "Count",
       x = "Blood Pressure Category") +
  theme_minimal() +
  facet_wrap(~ Age)  # This line will create a facet for each age group





