## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
library(readr)
library(ggplot2)
library(dplyr)

# Load the dataset
df <- read_csv("Placement_Data_Full_Class.csv")

str(df)
head(df)

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Removing unnecessary columns
college_df <- df %>% select(-sl_no)

# Cleaning up null data by replacing salary with 0
college_df$salary[is.na(college_df$salary)] <- 0

# One Hot Encoding
college_df <- college_df %>% 
  mutate(specialisation_Mkt_Fin = as.integer(specialisation == 'Mkt&Fin'),
         specialisation_Mkt_HR = as.integer(specialisation == 'Mkt&HR'),
         ssc_b_Others = as.integer(ssc_b == 'Others'),
         hsc_b_Others = as.integer(hsc_b == 'Others'),
         hsc_s_Arts = as.integer(hsc_s == 'Arts'),
         hsc_s_Commerce = as.integer(hsc_s == 'Commerce'),
         hsc_s_Science = as.integer(hsc_s == 'Science'),
         degree_t_Comm_Mgmt = as.integer(degree_t == 'Comm&Mgmt'),
         degree_t_Sci_Tech = as.integer(degree_t == 'Sci&Tech'),
         degree_t_Others = as.integer(degree_t == 'Others'))

college_df <- college_df %>% select(-c(specialisation, ssc_b, hsc_b, hsc_s, degree_t))

# Dictionary replacement
college_df$gender <- ifelse(college_df$gender == 'M', 0, 1)
college_df$workex <- ifelse(college_df$workex == 'No', 0, 1)
college_df$status <- ifelse(college_df$status == 'Placed', 0, 1)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Load necessary packages
library(reshape2)
library(ggplot2)

# Create a correlation matrix
corr_mat <- round(cor(college_df), 2)

# Melt the correlation matrix for plotting
melted_corr_mat <- melt(corr_mat)

ggplot(data = melted_corr_mat, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile(width = 1, height = 1) +
  geom_text(aes(label = value), size = 2) +
  labs(title = "Correlation Heatmap") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Gender Count
ggplot(df, aes(x = gender, fill = gender)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  labs(x = 'Gender', y = 'Count') +
  scale_fill_brewer(palette = 'Pastel1')

# Work Experience
ggplot(df, aes(x = workex, fill = workex)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  labs(x = 'Work Experience', y = 'Count') +
  scale_fill_brewer(palette = 'Pastel1')  

# Placement Status
ggplot(df, aes(x = status, fill = status)) +
  geom_bar() +
  geom_text(stat = 'count', aes(label = after_stat(count)), vjust = -0.5) +
  labs(x = 'Placement Status', y = 'Count') + 
  scale_fill_brewer(palette = 'Pastel1')  

# Placement Status by Genders
ggplot(df, aes(x = status, fill = gender)) +
  geom_bar(position = 'dodge') +
  geom_text(stat = 'count', aes(label = after_stat(count)), position = position_dodge(0.9), vjust = -0.5) +
  labs(x = 'Placement Status by Genders', y = 'Count') +
  scale_fill_brewer(palette = 'Pastel1')  

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
# Set the plot size
options(repr.plot.width = 15, repr.plot.height = 8)

# Create distribution plots
par(mfrow = c(2, 2))

# Secondary Education percentage
ggplot(df, aes(x = ssc_p)) +
  geom_histogram(binwidth = 5, fill = 'lightblue', color = 'black') +
  labs(x = 'Secondary Education percentage', y = 'Frequency')

# Higher Secondary Education percentage
ggplot(df, aes(x = hsc_p)) +
  geom_histogram(binwidth = 5, fill = 'lightgreen', color = 'black') +
  labs(x = 'Higher Secondary Education percentage', y = 'Frequency')

# Undergraduate percentage
ggplot(df, aes(x = degree_p)) +
  geom_histogram(binwidth = 5, fill = 'lightcoral', color = 'black') +
  labs(x = 'Undergraduate percentage', y = 'Frequency')

# MBA percentage
ggplot(df, aes(x = mba_p)) +
  geom_histogram(binwidth = 5, fill = 'lightsalmon', color = 'black') +
  labs(x = 'MBA percentage', y = 'Frequency')

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
college_glm <- glm(status ~ . -salary, data = college_df, family = "binomial", maxit = 1000)
summary(college_glm)


## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
table = step(college_glm )

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
set.seed(41)
test_error = numeric(10)

for (i in 1:10) {
  sample_indices = sample.int(n = nrow(college_df), size = floor(0.8 * nrow(college_df)), replace = FALSE)
  train = college_df[sample_indices,]
  test = college_df[-sample_indices,]

  college_glm = glm(status ~ gender + ssc_p + hsc_p + degree_p + workex + mba_p + degree_t_Comm_Mgmt,
                     data = train,
                     family = "binomial")

  college_pred = predict.glm(college_glm, newdata = test, type = "response")
  yhat = ifelse(college_pred < 0.5, 'Not Placed', 'Placed')

  conf.test = table(test$status, yhat)
  test_error[i] = (conf.test[1, 2] + conf.test[2, 1]) / nrow(test)
}

mean(test_error)

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
exams.glm2 = glm(status ~ gender + ssc_p + hsc_p + degree_p + workex + mba_p + degree_t_Comm_Mgmt,
                     data = train,
                     family = "binomial")
summary(exams.glm2)

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

### RANDOM FOREST 

library(randomForest)
library(randomForestExplainer)
library(caret)
library(magrittr) 

# Convert binary variables to factors
college_df$gender <- as.factor(college_df$gender)
college_df$workex <- as.factor(college_df$workex)
college_df$status <- as.factor(college_df$status)

# Split the data into training and testing sets
set.seed(42)
sample_indices <- sample.int(n = nrow(college_df), size = floor(0.8 * nrow(-college_df)), replace = FALSE)
train_data <- college_df[sample_indices, ]
test_data <- college_df[-sample_indices, ]

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

rf_model_importance <- randomForest(status ~ . -salary, data = college_df, importance = TRUE)

# Plot feature importance
varImpPlot(rf_model_importance, main = "Random Forest Feature Importance")

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Define the hyperparameter grid
hyperparameter_grid <- expand.grid(
  mtry = c(2, 3, 4)  
)

# Use train function for hyperparameter tuning
set.seed(42)
tuned_rf_model <- train(
  x = train_data[, -which(names(train_data) %in% c("salary", "status"))],
  y = train_data$status,
  method = "rf",
  tuneGrid = hyperparameter_grid,
  trControl = trainControl(method = "cv", number = 5)  # 5-fold cross-validation
)

# Display the tuned model
print(tuned_rf_model)

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Make predictions on the test set
tuned_rf_pred <- predict(tuned_rf_model, newdata = test_data)

# Convert predicted values to factors with the same levels as the original status variable
tuned_rf_pred <- factor(tuned_rf_pred, levels = levels(test_data$status))

# Confusion matrix
conf_matrix_tuned_rf <- confusionMatrix(data = tuned_rf_pred, reference = test_data$status)
print(conf_matrix_tuned_rf)

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Subset the dataframe to include only the selected variables
selected_vars <- c('ssc_p', 'hsc_p', 'degree_p', 'mba_p', 'etest_p', 'workex')
college_df_subset <- college_df[, c(selected_vars, 'status')]

# Convert binary variables to factors
college_df_subset$workex <- as.factor(college_df_subset$workex)
college_df_subset$status <- as.factor(college_df_subset$status)

# Split the data into training and testing sets
set.seed(42)
sample_indices <- sample.int(n = nrow(college_df_subset), size = floor(0.8 * nrow(college_df_subset)), replace = FALSE)
train_data_subset <- college_df_subset[sample_indices, ]
test_data_subset <- college_df_subset[-sample_indices, ]

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Define the hyperparameter grid
hyperparameter_grid <- expand.grid(
  mtry = c(2, 3, 4)  
)

# Use train function for hyperparameter tuning
set.seed(42)
tuned_rf_model_subset <- train(
  x = train_data_subset[, -which(names(train_data_subset) %in% c("status"))],
  y = train_data_subset$status,
  method = "rf",
  tuneGrid = hyperparameter_grid,
  trControl = trainControl(method = "cv", number = 5)  # 5-fold cross-validation
)

# Display the tuned model
print(tuned_rf_model_subset)

## ------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# Make predictions on the test set
tuned_rf_pred_subset <- predict(tuned_rf_model_subset, newdata = test_data_subset)

# Convert predicted values to factors with the same levels as the original status variable
tuned_rf_pred_subset <- factor(tuned_rf_pred_subset, levels = levels(test_data_subset$status))

# Confusion matrix
conf_matrix_tuned_rf_subset <- confusionMatrix(data = tuned_rf_pred_subset, reference = test_data_subset$status)
print(conf_matrix_tuned_rf_subset)

