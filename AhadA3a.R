
# Read in the data
setwd("C:\\Users\\Ahad\\OneDrive\\Pictures\\Documents\\Desktop\\scma")
df <- read.csv("lung_cancer_examples.csv")
# Load necessary libraries
library(caret)
library(FSelector)

# Assuming data is loaded into a data frame named 'data'
# data <- read.csv('data.csv')  # example of loading data

# Convert non-numeric values to NA
data[] <- lapply(data, function(x) as.numeric(as.character(x)))

# Handle missing values by replacing NA with column means
data[is.na(data)] <- lapply(data, function(x) ifelse(is.na(x), mean(x, na.rm = TRUE), x))

# Split data into train and test sets
set.seed(42)
trainIndex <- createDataPartition(data$target, p = .8, list = FALSE, times = 1)
x_train <- data[trainIndex, -ncol(data)]
x_test <- data[-trainIndex, -ncol(data)]
y_train <- data[trainIndex, ncol(data)]
y_test <- data[-trainIndex, ncol(data)]

# Scale data to 0 to 1 range
preProcValues <- preProcess(x_train, method = c("range"))
sc_x_train <- predict(preProcValues, x_train)
sc_x_test <- predict(preProcValues, x_test)

# Feature selection using mutual information
mutual_info <- information.gain(target ~ ., data = data.frame(target = y_train, sc_x_train))
feature_scores_df <- data.frame(Feature = rownames(mutual_info), Mutual_Info_Score = mutual_info$attr_importance)
feature_scores_df <- feature_scores_df[order(-feature_scores_df$Mutual_Info_Score), ]

# Assuming models are defined and trained
# model1_results <- ...  # example placeholder
# model2_results <- ...  # example placeholder

# Example of placeholder data frames for model results
df1 <- data.frame(class = factor(c("A", "B")), precision = c(0.8, 0.7), recall = c(0.75, 0.65), f1_score = c(0.77, 0.67), support = c(50, 50))
df2 <- data.frame(class = factor(c("A", "B")), precision = c(0.85, 0.78), recall = c(0.8, 0.7), f1_score = c(0.82, 0.74), support = c(50, 50))

# Add model names
df1$model <- 'Decision Tree'
df2$model <- 'Logistic Regression'

# Combine data frames
comparison_df <- rbind(df1, df2)

# Reorder columns
comparison_df <- comparison_df[, c('model', 'class', 'precision', 'recall', 'f1_score', 'support')]

# Display the comparison table
print(comparison_df)
