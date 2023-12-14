# Load necessary libraries
library(httr)
library(RCurl)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)

# Set the working directory to the folder containing your data file
setwd("C:/Users/tusha/Data Analytics/Data Sets - R Studio/Assignments/Data/Assignment 4")

# Load the dataset from a CSV file
df <- read.csv("NYC_Citywide_Annualized_Calendar_Sales_Update_20231103.csv")

# Filter the dataset to include only records for Brooklyn
Brooklyn_df <- df %>%
  filter(grepl("BROOKLYN", BOROUGH) | BOROUGH == 3)

# Select specific columns of interest
Brooklyn_df_new <- Brooklyn_df %>%
  select(
    NEIGHBORHOOD, BUILDING.CLASS.CATEGORY, YEAR.BUILT, ADDRESS,
    APARTMENT.NUMBER, ZIP.CODE, RESIDENTIAL.UNITS, COMMERCIAL.UNITS,
    TOTAL.UNITS, LAND.SQUARE.FEET, GROSS.SQUARE.FEET, SALE.PRICE, SALE.DATE
  )

# Convert SALE.PRICE to numeric and SALE.DATE to a date format
Brooklyn_df_new$SALE.PRICE <- as.numeric(gsub("[^0-9]", "", Brooklyn_df_new$SALE.PRICE))
Brooklyn_df_new$SALE.DATE <- as.Date(Brooklyn_df_new$SALE.DATE, "%m/%d/%y")

# Convert ZIP.CODE to a factor
Brooklyn_df_new$ZIP.CODE <- as.factor(as.character(Brooklyn_df_new$ZIP.CODE))

# Clean and format numeric columns
Brooklyn_df_new$LAND.SQUARE.FEET <- as.numeric(gsub("[^0-9]", "", Brooklyn_df_new$LAND.SQUARE.FEET))
Brooklyn_df_new$GROSS.SQUARE.FEET <- as.numeric(gsub("[^0-9]", "", Brooklyn_df_new$GROSS.SQUARE.FEET))
Brooklyn_df_new$TOTAL.UNITS <- as.integer(Brooklyn_df_new$TOTAL.UNITS)
Brooklyn_df_new$RESIDENTIAL.UNITS <- as.integer(Brooklyn_df_new$RESIDENTIAL.UNITS)

# Check for null values in each column
null_counts <- colSums(is.na(Brooklyn_df_new))

# Create a histogram of the YEAR.BUILT column
hist(Brooklyn_df_new$YEAR.BUILT, breaks = 50)

# Calculate the correlation matrix for numeric columns
numeric_cols <- sapply(Brooklyn_df_new, is.numeric)
df_numeric <- Brooklyn_df_new[, numeric_cols]
cormat <- round(cor(df_numeric, use = "complete.obs"), 2)

# Create a correlation heatmap
library(reshape2)
melted_cormat <- melt(cormat)

# Generate a heatmap for the lower triangle of the correlation matrix
get_lower_tri <- function(cormat) {
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}
upper_tri <- get_lower_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

# Create a heatmap plot
library(ggplot2)
ggplot(data = melted_cormat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile()

# Reorder the correlation matrix to highlight patterns
reorder_cormat <- function(cormat) {
  dd <- as.dist((1 - cormat) / 2)
  hc <- hclust(dd)
  cormat <- cormat[hc$order, hc$order]
}
cormat <- reorder_cormat(cormat)

# Create a reordered correlation heatmap
ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value)) +
  geom_tile(color = "white") +
  scale_fill_gradient2(
    low = "blue", high = "red", mid = "white",
    midpoint = 0, limit = c(-1, 1), space = "Lab",
    name = "Pearson\nCorrelation"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, vjust = 1, size = 12, hjust = 1)
  ) +
  coord_fixed()

# Print the reordered correlation heatmap
print(ggheatmap)

# Remove duplicates from the dataset
df_no_duplicates <- unique(Brooklyn_df_new)

# Check if duplicates have been removed
duplicated_rows <- duplicated(df_no_duplicates)
if (any(duplicated_rows)) {
  message("Duplicates still exist in the dataframe.")
} else {
  message("Duplicates have been successfully removed from the dataframe.")
}

# Create a long-format data frame for visualization
df_long <- gather(df_no_duplicates, key = "variable", value = "value")

# Plot the distributions of all variables
ggplot(df_long, aes(x = value, fill = variable)) +
  geom_density(alpha = 0.5) +
  labs(title = "Distributions of all variables in the dataframe")

# Plot histograms for specific numeric columns
ggplot(gather(df_no_duplicates, cols, value), aes(x = value)) +
  geom_histogram(binwidth = 20) +
  facet_grid(. ~ cols)

# Create separate histograms for specific numeric columns
hist(df_no_duplicates$YEAR.BUILT)
hist(df_no_duplicates$RESIDENTIAL.UNITS)
hist(df_no_duplicates$COMMERCIAL.UNITS)
hist(df_no_duplicates$TOTAL.UNITS)
hist(df_no_duplicates$LAND.SQUARE.FEET)
hist(df_no_duplicates$SALE.PRICE, breaks = 5)
hist(df_no_duplicates$GROSS.SQUARE.FEET, breaks = 10)

# Create individual histograms for specific columns
ggplot(df_no_duplicates, aes(x = YEAR.BUILT)) +
  geom_histogram() +
  labs(title = "Histogram of the YEAR.BUILT column")

ggplot(df_no_duplicates, aes(x = RESIDENTIAL.UNITS)) +
  geom_histogram() +
  labs(title = "Histogram of the RESIDENTIAL.UNITS column")

ggplot(df_no_duplicates, aes(x = COMMERCIAL.UNITS)) +
  geom_histogram() +
  labs(title = "Histogram of the COMMERCIAL.UNITS column")

ggplot(df_no_duplicates, aes(x = TOTAL.UNITS)) +
  geom_histogram() +
  labs(title = "Histogram of the TOTAL.UNITS column")

ggplot(df_no_duplicates, aes(x = LAND.SQUARE.FEET)) +
  geom_histogram() +
  labs(title = "Histogram of the LAND.SQUARE.FEET column")

ggplot(df_no_duplicates, aes(x = SALE.PRICE)) +
  geom_histogram() +
  labs(title = "Histogram of the SALE.PRICE column")

ggplot(df_no_duplicates, aes(x = GROSS.SQUARE.FEET)) +
  geom_histogram() +
  labs(title = "Histogram of the GROSS.SQUARE.FEET column")

summary(Brooklyn_df_new$SALE.PRICE)

# plot the histogram of the "Sales" column with sales price < 5000000
df_filtered <- df1_numeric %>% filter(SALE.PRICE > 0 & SALE.PRICE < 50000)
ggplot(df_filtered, aes(x = SALE.PRICE)) +
  geom_histogram(binwidth = 500000) +
  labs(title = "Histogram of Sales with Sales < 5000000")


library(ggplot2)
library(scales)
library(gridExtra)

hsales_filtered <- subset(Brooklyn_df_new , SALE.PRICE < 2375000)

plotd <- ggplot(hsales_filtered, aes(x = SALE.PRICE)) + 
  geom_histogram(aes(y = ..density..), binwidth = 5000000, colour = "black", fill = "white") + 
  geom_density(alpha = .2, fill="lightblue")+
  scale_x_continuous() + 
  scale_y_continuous() + 
  theme_bw() + 
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), axis.line = element_line(colour = "black")) + 
  xlim(-100000, 5000000) + 
  xlab("SALE PRICE") + 
  ylab("Density")

mean_line <- geom_vline(aes(xintercept = mean(Brooklyn_df_new$SALE.PRICE)), colour = "red", linetype = "dashed", size = 1)
median_line <- geom_vline(aes(xintercept = median(Brooklyn_df_new$SALE.PRICE)), colour = "blue", linetype = "dashed", size = 1)

plotd <- plotd + mean_line + median_line

text1 <- annotate("text", x = 250000, y = 0.0000012, label = "median")
text2 <- annotate("text", x = 850000, y = 0.0000010, label = "mean")

grid.arrange(plotd + text1 + text2, ncol=1)

mean(Brooklyn_df_new$SALE.PRICE)
max(Brooklyn_df_new$SALE.PRICE)

summary(Brooklyn_df_new$SALE.PRICE)


df_filtered <- df1_numeric %>% filter(YEAR.BUILT > 1850)

# plot the histogram of the "x" column
ggplot(df_filtered, aes(x = YEAR.BUILT)) +
  geom_histogram() +
  labs(title = "Histogram of the YEAR.BUILT column after removing ouliers")

# finding outliers for year column

q1 <- quantile(df_filtered$YEAR.BUILT, 0.25)
q3 <- quantile(df_filtered$YEAR.BUILT, 0.75)
iqr <- q3 - q1

# determine the upper and lower limits of the outliers
upper_limit <- q3 + 1.5*iqr
lower_limit <- q1 - 1.5*iqr

# identify the outliers in the  column
outliers <- df_filtered %>% filter(YEAR.BUILT < lower_limit | YEAR.BUILT > upper_limit)

# find the minimum and maximum values of the outliers
min_outliers <- min(outliers$YEAR.BUILT)
max_outliers <- max(outliers$YEAR.BUILT)

min_outliers
max_outliers

upper_limit
lower_limit 
q1
q3


# finding outliers for sales price column

q1 <- quantile(df_filtered$SALE.PRICE, 0.25)
q3 <- quantile(df_filtered$SALE.PRICE, 0.75)
iqr <- q3 - q1

# determine the upper and lower limits of the outliers
upper_limit <- q3 + 1.5*iqr
lower_limit <- q1 - 1.5*iqr

# identify the outliers in the  column
outliers <- df_filtered %>% filter(SALE.PRICE < lower_limit | SALE.PRICE > upper_limit)

# find the minimum and maximum values of the outliers
min_outliers <- min(outliers$SALE.PRICE)
max_outliers <- max(outliers$SALE.PRICE)

min_outliers
max_outliers

upper_limit
lower_limit 
q1
q3

############## Regression

# Load required libraries
library(dplyr)
library(caret)
# finding outliers for sales price column

q1 <- quantile(Brooklyn_df_new$SALE.PRICE, 0.25)
q3 <- quantile(Brooklyn_df_new$SALE.PRICE, 0.75)
iqr <- q3 - q1

# determine the upper and lower limits of the outliers
upper_limit <- q3 + 1.5*iqr
lower_limit <- q1 - 1.5*iqr

# identify the outliers in the  column
outliers <- df_filtered %>% filter(SALE.PRICE < lower_limit | SALE.PRICE > upper_limit)

df_filtered_reg <- Brooklyn_df_new %>% filter(SALE.PRICE > lower_limit & SALE.PRICE < upper_limit)

str(df_filtered_reg)
df_filered_reg_model <- select(df_filtered_reg,LAND.SQUARE.FEET,GROSS.SQUARE.FEET, SALE.PRICE)

df_filered_reg_model  <- na.omit(df_filered_reg_model)


write.csv(Brooklyn_df_new,"model_data1.csv")

# regression models using random sampling


Brooklyn_df_new_data  <- select(Brooklyn_df_new,YEAR.BUILT,LAND.SQUARE.FEET,GROSS.SQUARE.FEET, SALE.PRICE)

Brooklyn_df_new_data1  <- na.omit(Brooklyn_df_new_data)


df_filtered <- Brooklyn_df_new_data1%>% filter(SALE.PRICE > 500000)
df_filtered1 <- df_filtered%>% filter(GROSS.SQUARE.FEET> 0)
df_filtered2 <- df_filtered1 %>% filter(YEAR.BUILT> 0)
df_filtered2 <- df_filtered2 %>% filter(LAND.SQUARE.FEET> 1)


# Set standard deviation threshold
standard_deviations <- 3

# Calculate mean and standard deviation for each column
means <- apply(df_filtered2, 2, mean)
sds <- apply(df_filtered2, 2, sd)

# Identify rows where all values are within the standard deviation threshold
keep_rows <- apply(df_filtered2, 1, function(x) all(abs(x - means) / sds < standard_deviations))

# Subset data frame to keep only the rows within the threshold
reg_final_data <- df_filtered2[keep_rows, ]


model <- lm(SALE.PRICE ~ LAND.SQUARE.FEET + GROSS.SQUARE.FEET, data = reg_final_data)
summary(model)


# Set seed for reproducibility
set.seed(123)

# Create an empty data frame to store the model results
model_results <- data.frame(matrix(ncol = 5, nrow = 0))
colnames(model_results) <- c("model_number", "R_squared", "coeff_land_sf", "coeff_gross_sf", "coeff_intercept")

# Loop through each model
for (i in 1:5) {
  
  # Randomly sample 70% of the data for the model
  sample_rows <- sample(nrow(reg_final_data), round(0.90 * nrow(reg_final_data)))
  my_sample <- reg_final_data[sample_rows, ]
  
  # Fit linear model
  model <- lm(SALE.PRICE ~ LAND.SQUARE.FEET + GROSS.SQUARE.FEET, data = my_sample)
  
  # Get R-squared score and coefficients
  r_squared <- summary(model)$r.squared
  coefficients <- round(coef(model), 3)
  
  # Add results to data frame
  model_results <- rbind(model_results, data.frame(model_number = i, R_squared = r_squared, 
                                                   coeff_land_sf = coefficients[2], coeff_gross_sf = coefficients[3], 
                                                   coeff_intercept = coefficients[1]))
  
}

# Print data frame with model results
print(model_results)

avg_sale_price <- mean(model_results$R_squared)
print(avg_sale_price)

avg_land_sq<- mean(model_results$coeff_land_sf)
print(avg_land_sq)


avg_gross_sq<- mean(model_results$coeff_gross_sf)
print(avg_gross_sq)


######################### D


Brooklyn_df_new_data1  <- na.omit(Brooklyn_df_new)


df_filtered <- Brooklyn_df_new_data1%>% filter(SALE.PRICE > 500000)
df_filtered1 <- df_filtered%>% filter(GROSS.SQUARE.FEET> 0)
df_filtered2 <- df_filtered1 %>% filter(YEAR.BUILT> 0)
df_filtered2 <- df_filtered2 %>% filter(LAND.SQUARE.FEET> 1)

str(df_filtered2)

df_filtered3 = subset(df_filtered2, select = -c(ADDRESS,APARTMENT.NUMBER,ZIP.CODE,SALE.DATE))

write.csv(df_filtered3,"model_data13.csv")



library(caret)

# Define your target variable
target <- 'SALE.PRICE'

# Define the number of folds
n_folds <- 5

# Define the models you want to use
models <- list("Decision Tree" = rpart::rpart(), "KNN Regression" = kknn::kknn())

# Create a dataframe to store the results
results_df <- data.frame(Model = character(),
                         Fold = integer(),
                         R2 = numeric(),
                         stringsAsFactors = FALSE)

# Split the data into folds
kf <- createFolds(data[[target]], k = n_folds)

# Iterate over the folds
for (fold in 1:n_folds) {
  
  # Get the data for this fold
  train_index <- unlist(kf[-fold])
  test_index <- kf[[fold]]
  train_data <- data[train_index, ]
  test_data <- data[test_index, ]
  
  # Get the features and target variables
  X_train <- train_data[, !(names(train_data) %in% target)]
  y_train <- train_data[[target]]
  X_test <- test_data[, !(names(test_data) %in% target)]
  y_test <- test_data[[target]]
  
  # Iterate over the models
  for (model_name in names(models)) {
    
    # Fit the model
    model <- models[[model_name]]
    model_fit <- train(x = X_train, y = y_train, method = model, trControl = trainControl(method = "none"))
    
    # Make predictions
    y_pred <- predict(model_fit, newdata = X_test)
    
    # Calculate the R2 score
    r2 <- cor(y_test, y_pred)^2
    
    # Append the results to the dataframe
    results_df <- rbind(results_df, data.frame(Model = model_name, Fold = fold, R2 = r2))
  }
}

# Print the results
aggregate(R2 ~ Model, data = results_df, FUN = mean)



results <- read.csv("results.csv")


results

feat_imp <- read.csv("feat_imp.csv")

feat_imp

str(df_filtered3)

library(dplyr)
library(tidyr)

# Separate the numerical and categorical columns
num_cols <- df_filtered3 %>% select_if(is.numeric) # Select all numerical columns
cat_cols <- df_filtered3 %>% select_if(is.character) # Select all categorical columns

# Create dummies for categorical columns using 'model.matrix' function
dummy_cols <- lapply(cat_cols, function(x) {
  mm <- model.matrix(~x, data = df_filtered3)
  return(mm[,-1]) # Drop the intercept column
})
dummy_cols <- do.call(cbind, dummy_cols) # Combine the dummy variables into one data frame

# Rename the dummy variables to match the original categorical column names
colnames(dummy_cols) <- unlist(lapply(cat_cols, colnames))

# Merge the numerical and dummy columns
merged_data <- bind_cols(num_cols, dummy_cols)


# assume your dataset is called 'df' and output variable is called 'y'
set.seed(1)
folds <- createFolds(merged_data$SALE.PRICE, k = 5)

# initialize an empty dataframe to store results
result_df <- data.frame(split = integer(), r2_score = numeric())

# iterate through each fold and train/test model, store results in dataframe
for (i in 1:length(folds)) {
  train_idx <- unlist(folds[-i])
  test_idx <- unlist(folds[i])
  train <- merged_data[train_idx, ]
  test <- merged_data[test_idx, ]
  model <- train(SALE.PRICE ~ ., data = train, method = "rpart")
  predictions <- predict(model, newdata = test)
  r2_score <- cor(predictions, test$SALE.PRICE)^2
  temp_df <- data.frame(split = i, r2_score = r2_score)
  result_df <- rbind(result_df, temp_df)
}


importance <- varImp(model)$importance
importance
print(result_df)


library(caret)
library(rpart)

# Define number of folds for cross-validation
k <- 5

# Split data into k equally sized folds
folds <- createFolds(merged_data$SALE.PRICE, k = k, list = TRUE, returnTrain = FALSE)

# Initialize dataframes to store results
coefficients_df <- data.frame(matrix(ncol = 11, nrow = k))
colnames(coefficients_df) <- c("Fold", "Intercept", names(data)[1:10])
r2_df <- data.frame(matrix(ncol = 2, nrow = k))
colnames(r2_df) <- c("Fold", "R2")

# Loop over each fold
for (i in 1:k) {
  
  # Get test and train sets for current fold
  test_set <- merged_data[folds[[i]],]
  train_set <- merged_data[-folds[[i]],]
  
  # Fit decision tree regression model
  fit <- rpart(SALE.PRICE ~ ., data = train_set, method = "anova")
  
  # Get coefficients of each input variable
  coefficients <- coef(fit)
  intercept <- coefficients[1]
  coefficients <- coefficients[2:length(coefficients)]
  
  # Calculate R2 of the model
  pred <- predict(fit, newdata = test_set)
  r2 <- summary(lm((test_set$SALE.PRICE - pred)^2 ~ 1))$r.squared
  
  # Store results in dataframes
  coefficients_df[i,] <- c(i, intercept, coefficients)
  r2_df[i,] <- c(i, r2)
}

# Print results
print(coefficients_df)
print(r2_df)


library(rpart)
library(caret)

# Assuming your data frame is called 'my_data'

# Set up the k-fold cross-validation
folds <- createFolds(merged_data$SALE.PRICE, k = 5)

# Initialize data frames to store results
results_df <- data.frame(R2 = numeric(), Coefficients = list(), stringsAsFactors = FALSE)
feature_importance_df <- data.frame(Feature = character(), Importance = numeric(), stringsAsFactors = FALSE)

# Loop over the folds
for (i in 1:length(5)) {
  # Split the data into training and testing sets
  train_idx <- unlist(folds[-i])
  test_idx <- folds[[i]]
  train_data <- merged_data[train_idx,]
  test_data <- merged_data[test_idx,]
  
  # Fit the decision tree model
  model <- rpart(SALE.PRICE ~ ., data = train_data)
  
  # Make predictions on the test data
  preds <- predict(model, test_data)
  
  # Calculate R2 and store it in the results data frame
  R2 <- cor(preds, test_data$SALE.PRICE)^2
  results_df[i, "R2"] <- R2
  
  # Get the coefficients of each input variable and store them in the results data frame
  coeffs <- coef(model)[-1]
  results_df[i, "Coefficients"] <- coeffs
  
  # Get the feature importance and store it in the feature importance data frame
  importance <- varImp(model)$importance
  feature_importance_df <- rbind(feature_importance_df, data.frame(Feature = rownames(importance), Importance = importance, stringsAsFactors = FALSE))
}

# Calculate the average R2 across all folds and print it
avg_R2 <- mean(results_df$R2)
cat(paste0("Average R2: ", round(avg_R2, 2)))

# Calculate the average coefficients across all folds and print them
avg_coeffs <- as.data.frame(t(sapply(results_df$Coefficients, mean)))
colnames(avg_coeffs) <- c("Coefficients")
cat("\nAverage Coefficients:\n")
print(avg_coeffs)

# Calculate the feature importance across all folds and print it
feature_importance <- aggregate(Importance ~ Feature, feature_importance_df, mean)
cat("\nFeature Importance:\n")
print(feature_importance)


### Import libraries
library(randomForest)
library(ggplot2)

set.seed(4543)
rf.fit <- randomForest(SALE.PRICE ~ ., data=df_filtered2, ntree=10,
                       keep.forest=FALSE, importance=TRUE)

rf.fit


# Load libraries
library(rpart)
library(caret)

# Set number of folds for k-fold cross-validation
k <- 5

# Split the data into k-folds using caret's createFolds function
folds <- createFolds(merged_data$SALE.PRICE, k = k)

# Create empty data frames to store the results
performance_df <- data.frame(R2 = numeric(),
                             coefficients = list(),
                             fold = numeric())
importance_df <- data.frame(feature = character(),
                            importance = numeric())

# Loop through the folds and fit decision tree models
for (i in 1:5) {
  
  # Create training and testing sets for this fold
  train_index <- unlist(folds[-i])
  test_index <- folds[[i]]
  train_data <- merged_data[train_index, ]
  test_data <- merged_data[test_index, ]
  
  # Fit decision tree model on the training set
  model <- rpart(SALE.PRICE ~ ., data = train_data)
  
  # Predict on the testing set and calculate R2
  preds <- predict(model, newdata = test_data)
  r2 <- cor(preds, test_data$SALE.PRICE)^2
  
  # Extract variable importance from the model and store in importance_df
  var_imp <- varImp(model)
  importance_df <- rbind(importance_df, data.frame(feature = rownames(var_imp), 
                                                   importance = var_imp$Overall))
  
  # Extract coefficients from the model and store in performance_df
  coefs <- as.list(coefficients(model))
  coefs <- unlist(coefs[2:length(coefs)]) # exclude intercept
  performance_df <- rbind(performance_df, data.frame(R2 = r2, 
                                                     coefficients = coefs,
                                                     fold = i))
}

# Print the performance and importance data frames
print(performance_df)
print(importance_df)

# Load rpart package for decision tree modeling
library(rpart)

# Fit decision tree with all variables and sales as output variable
tree_model <- rpart(SALE.PRICE ~ ., data = df_filtered2)

# Get feature importance
imp <- tree_model$variable.importance

# Print feature importance
print(imp)

summary(tree_model)

rsq <- summary(tree_model)$r.squared
# Print R-squared
cat("R-squared:", rsq, "\n")



# Set a random seed for reproducibility
set.seed(1)
# Create three random samples of data
sample1 <- sample_n(reg_model,46000)

set.seed(2)
# Create three random samples of data
sample2 <- sample_n(reg_model,46000)

set.seed(3)
# Create three random samples of data
sample3 <- sample_n(reg_model,46000)


# Create three linear regression models
model1 <- train(SALE.PRICE ~ LAND.SQUARE.FEET + GROSS.SQUARE.FEET, data = sample1, method = "lm")
model2 <- train(SALE.PRICE ~ LAND.SQUARE.FEET + GROSS.SQUARE.FEET, data = sample2, method = "lm")
model3 <- train(SALE.PRICE ~ LAND.SQUARE.FEET + GROSS.SQUARE.FEET, data = sample3, method = "lm")



# Create a data frame to store the results
results_df <- data.frame(
  Model = c("Model 1", "Model 2", "Model 3"),
  R2 = numeric(3),
  RMSE = numeric(3),
  GROSS_SQUARE_FEET_coef = numeric(3),
  LAND_SQUARE_FEET_coef = numeric(3),
  intercept_coef = numeric(3)
)

# Add the R2 and coefficients for each model to the data frame
results_df$R2[1] <- cor(model1$pred$obs, model1$pred$pred)^2
results_df$RMSE[1] <- RMSE(model1$pred$obs, model1$pred$pred)
results_df$GROSS_SQUARE_FEET_coef[1] <- coef(model1$finalModel)["GROSS.SQUARE.FEET"]
results_df$LAND_SQUARE_FEET_coef[1] <- coef(model1$finalModel)["LAND.SQUARE.FEET"]
results_df$intercept_coef[1] <- coef(model1$finalModel)["(Intercept)"]

results_df$R2[2] <- cor(model2$pred$obs, model2$pred$pred)^2
results_df$RMSE[2] <- RMSE(model2$pred$obs, model2$pred$pred)
results_df$GROSS_SQUARE_FEET_coef[2] <- coef(model2$finalModel)["GROSS.SQUARE.FEET"]
results_df$LAND_SQUARE_FEET_coef[2] <- coef(model2$finalModel)["LAND.SQUARE.FEET"]
results_df$intercept_coef[2] <- coef(model2$finalModel)["(Intercept)"]

results_df$R2[3] <- cor(model3$pred$obs, model3$pred$pred)^2
results_df$RMSE[3] <- RMSE(model3$pred$obs, model3$pred$pred)
results_df$GROSS_SQUARE_FEET_coef[3] <- coef(model3$finalModel)["GROSS.SQUARE.FEET"]
results_df$LAND_SQUARE_FEET_coef[3] <- coef(model3$finalModel)["LAND.SQUARE.FEET"]
results_df$intercept_coef[3] <- coef(model3$finalModel)["(Intercept)"]

# Print the results
print(results_df)

install.packages("randomForest")